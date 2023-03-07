;;; cheap-patmatch.lisp
;;; 05-Mar-2023 SVS

;; Copyright (c) 2023, Shannon Spires
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.

;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in
;;     the documentation and/or other materials provided with the
;;     distribution.

;;   * Neither Shannon Spires nor the names of its contributors of the
;;     software may be used to endorse or promote products derived from
;;     this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; I'm sick of trying to understand 17 different flavors of regular expressions.
;;; I'm sick of regular expressions that are write-only. I don't care if the regex is verbose; I
;;;    just want it to be understandable by me tomorrow.
;;; I don't particularly care if the regex matcher is as fast as possible; clarity trumps speed.
;;; I'm sick of regular expressions that confuse and intermix the desire for
;;;  1. A single binary result of whether a match happened
;;;  2. A numeric position within the string where the match happened, or where it failed as may be the case.
;;;  3. A captured, named substring that was matched in the string.

;;; In this system, #'patmatch takes two arguments: a string and a pattern to match the string against.
;;;    It returns a success/failure boolean and a state object, which contains the original string,
;;;    the offset (position) within the string where the pattern succeeded or failed, and a list of 
;;;    captures (if any) that were found within the string.

;;; See tests for examples.
;;; See #'primitive-pattern-dispatch methods for primitive matching keywords and how they work.
;;; Meta-pattern keywords are :seq, :or, :and, :not, and :break.

(in-package :cheap-patmatch)

(defclass state ()
  ((pos :initarg :pos :initform nil :accessor get-pos)
   (string :initarg :string :initform nil :accessor get-string)
   (len :initarg :len :initform nil :accessor get-string-length)
   (captures :initarg :captures :initform nil :accessor get-captures
                 :documentation "An alist of strings collected with :capture forms.")))

(defmethod copy-state ((self state) &optional newpos)
  "Copy a state object, optionally updating its pos slot. We use this quite a bit but
   note that it doesn't cons much because this is Common Lisp; it's not actually copying the contents
   of the slots that contain the most data (string and captures)."
  (make-instance 'state
    :pos (or newpos (get-pos self))
    :string (get-string self)
    :len (get-string-length self)
    :captures (get-captures self)))

(defmethod incf-pos ((self state))
  (let ((newstate (copy-state self)))
    (incf (get-pos newstate))
    newstate))
  
(defun some-match (state pattern)
  "Like #'some but expects fn to return two values: A boolean and a second value which is the actual result.
  Decision whether to proceed is made on the first value.
  Overall value returned is (values t result) or (values nil result). So that's the key difference from
  #'some: We always return a result as a second value."
  (if (car pattern)
    (multiple-value-bind (success? newstate)
                         (inner-patmatch state (car pattern))
      (if success?
          (if (eql :break success?) ;; don't let :break short-circuit an :or
              (some-match state (cdr pattern))
              (values t newstate))
          (some-match state (cdr pattern))))
    (values nil state)))

(defun every-match (state pattern)
  "Like #'every but expects fn to return two values: A boolean and a second value which is the actual result.
  Decision whether to proceed is made on the first value.
  Overall value returned is (values t result) or (values nil result). So that's the key difference from
  #'every: We always return a result as a second value."
  (if (car pattern)
    (multiple-value-bind (success? newstate)
                         (inner-patmatch state (car pattern))
      (if success?
          (every-match state (cdr pattern))
          (values nil newstate)))
    (values t state)))

(defun massage-arg-into-fn (arg)
  "If arg is a function, just return it.
   If arg is a character, return a function that matches that character.
   If arg is a string, return a function that matches any character in that string, case-sensitively."
  (typecase arg
    (function arg)
    (symbol arg) ; assume it represents a function
    (character (lambda (char) (char= char arg)))
    (string (lambda (char) (find char arg :test #'char=)))
    ))

(defgeneric primitive-pattern-dispatch (kwd fn state)
  (:documentation
    "Handles the primitive pattern keywords. Primitive patterns always look like (keyword fn),
     and they never contain subpatterns, which is why this function does not accept a pattern
     argument.
    Fn is a predicate of one argument: a character.
    Fn can also be
      -- A character itself
      -- A string, which means 'any of the characters in this string will satisfy the predicate'. Case-sensitive only for now.
    Returns (values t updated-state) on success.
    (values nil updated-state) on failure."))

(defmethod primitive-pattern-dispatch ((kwd (eql :string)) literal-string state)
  "Require that current string match a literal string."
  (let* ((pos (get-pos state))
         (string (get-string state))
         (len (get-string-length state))
         (endpos (+ pos (length literal-string))))
    (cond ((and (< pos len)
                (<= endpos len))
           ; keep going
           (if (string= literal-string (subseq string pos endpos))
               (values t (copy-state state endpos))
               (values nil state)))
          (t (values nil state))))) ; we're out of string but not out of pattern. This means failure.

(defmethod primitive-pattern-dispatch ((kwd (eql :zero-or-more)) fn state)
  "Require at zero or more characters at current position for which fn returns true."
  (setf fn (massage-arg-into-fn fn))
  (let ((pos (get-pos state))
        (string (get-string state))
        (len (get-string-length state)))
    (cond ((< pos len)
           ; keep going
           (if (funcall fn (char string pos))
               ; okay we have one char match
               (progn
                 (loop while (and (< (incf pos) len)
                                  (funcall fn (char string pos))))
                 (values t (copy-state state pos)))
               (values t state)))
          (t (values t state))))) ; we're out of string prematurely but for _this_ pattern it means success!

(defmethod primitive-pattern-dispatch ((kwd (eql :one-or-more)) fn state)
  "Require at least one character at current position for which fn returns true."
  (setf fn (massage-arg-into-fn fn))
  (let ((pos (get-pos state))
        (string (get-string state))
        (len (get-string-length state)))
    (cond ((< pos len)
           ; keep going
           (if (funcall fn (char string pos))
               ; okay we have one char match
               (progn
                 (loop while (and (< (incf pos) len)
                                  (funcall fn (char string pos))))
                 (values t (copy-state state pos)))
               (values nil state))) ; not even first char matched
          (t (values nil state))))) ; we're out of string but not out of pattern. This means failure.

(defmethod primitive-pattern-dispatch ((kwd (eql :one)) fn state)
  "Require character at current position for which fn returns true.
   If there is a next character, require fn called on it to return false.
   Thus this does single-character lookahead."
  (setf fn (massage-arg-into-fn fn))
  (let ((pos (get-pos state))
        (string (get-string state))
        (len (get-string-length state)))
    (cond ((< pos len)
           ; keep going
           (if (funcall fn (char string pos))
               ; okay we have one char match. Check successor if any.
               (if (< (incf pos) len)
                   (if (not (funcall fn (char string pos)))
                       (values t (copy-state state pos))
                       (values nil state)) ; no need to make a new state here. Initial pos is where it failed.
                   (values t (copy-state state pos))) ; no next character at all. This means success.
                (values nil state))) ; first character didn't match
          (t (values nil state))))) ; we're out of string but not out of pattern. This means failure.

#|
These are the meta-pattern keywords:
:seq -- Perform pattern clauses in order.
        Each updates position and captures before proceeding to next clause.
        If any clause fails, overall pattern fails.
        (The literal :seq keyword is not usually required except to create a sequential pattern inside
         another meta-pattern keyword like :and, :or, or :not.
         In all other cases :seq is implied.)

:capture name -- Perform pattern clauses in order.
        This is identical to :seq except if all the pattern clauses result in a successful match, the substring
        that matches will be pushed onto the captures list of the state object, and #'ppatmatch will return them.
        name can be a string or a non-NIL symbol in which case that name will be consed to the matching string before
        being pushed on the captures list so you can find it quickly.
        name can also be NIL, in which case the matching string will be pushed 'naked' onto the captures list.

:not -- Only a single pattern clause should follow.
        If that clause fails, overall clause succeeds, and vice-versa.

:or  -- Perform pattern clauses in order.
        Each starts at same string position as previous, and updates captures where appropriate.
        If any clause succeeds, overall pattern succeeds.

:and -- Perform pattern clauses in order.
        Each starts at same string position as previous, and updates captures where appropriate.
        If any clause fails, overall pattern fails.

:break -- Throws a (break). Useful for debugging. Breaks always succeed, so continuing after the break just continues.
         :or makes a special provision for :break so it won't short-circuit.
         :not makes no such provision; an enclosed (:break) just causes overall failure.
|#

(defun inner-patmatch (state pattern)
  (flet ((do-sequentially (state pattern)
           ; Execute car and cdr in sequence, with each seeing a (potentially) updated position in the string
           (multiple-value-bind (success? newstate)
                                (inner-patmatch state (car pattern))
             (if success?
                 (inner-patmatch newstate (cdr pattern))
                 (values nil newstate)))))
    (if pattern
        (if (stringp pattern)
            ; syntactic sugar so pattern can be a string to be matched literally
            (primitive-pattern-dispatch :string pattern state)
            (let ((carpat (car pattern)))
              (cond ((keywordp carpat)
                     ; check for meta-pattern keywords and handle them first. Meta-patterns contain other patterns.
                     (case carpat
                       (:seq
                         (do-sequentially state (cdr pattern)))
                       (:capture
                        ; second must be a symbol or string to name the capture group
                        ; cddr is assumed to be a sequential meta-pattern
                        (let ((capture-name (second pattern)))
                          (unless (or (symbolp capture-name)
                                      (stringp capture-name))
                            (error "Improper capture name found: ~S" capture-name))
                          (multiple-value-bind (success? newstate)
                                               (inner-patmatch state (cddr pattern))
                            (cond (success?
                                   ;; Record the match on newstate and return newstate.
                                   ;; Callee cannot do this because it don't know about the :capture clause it's within.
                                   (let ((string-found (subseq (get-string newstate) ;; <-- Here's the magic. Study this and understand it.
                                                               (get-pos state)
                                                               (get-pos newstate))))
                                     (push (if capture-name
                                               (cons capture-name string-found)
                                               string-found) ; capture name is explicitly nil (anonymous). So push just the string, not a cons.
                                           (get-captures newstate))
                                     (values t newstate)))
                                  (t (values nil newstate)))))) ; we already know newstate represents a failed match,
                                                                ; but it contains potentially-valuable position
                                                                ; and capture information, so return it.
                       
                       (:not ; should contain only a single subpattern (which could itself be wrapped in another meta-pattern keyword).
                        (multiple-value-bind (success? newstate)
                                             (inner-patmatch state (second pattern))
                          (if success?
                              (values nil state)
                              (values t newstate))))
                       
                       (:or ; cdr of pattern will be evaluated in sequence but each evaluation starts at the same position
                        ; Any success wins immediately.
                        (some-match state (cdr pattern)))
                       
                       (:and ; cdr of pattern will be evaluated in sequence but each evaluation starts at the same position
                        ; Any failure loses immediately.
                        (every-match state (cdr pattern)))
                       
                       (:break (break) ; just for debugging patterns
                               (values :break state)) ; break always succeeds so we'll go to the next thing
                       
                       (t ; any other keyword indicates a primitive pattern
                        (primitive-pattern-dispatch carpat
                                                    (second pattern) ; fn
                                                    state))))
                    
                    (t ; No opening keyword, so treat pattern as if it opened with :SEQ.
                     (do-sequentially state pattern)
                     ))))
        
        ; pattern is empty. This is success.
        (values t state))))

(defun patmatch (string pattern)
  "Returns (values t state) on success.
  (values nil state) on failure."
  (let ((state (make-instance 'state
                   :pos 0
                   :string string
                   :len (length string))))
    (inner-patmatch state pattern)))

(defun ppatmatch (string pattern)
  "Pretty version patmatch. This is the primary user interface if all you care about
   is whether the match succeeded and what the captures were.
   (If you care about the position within the string where the match succeeded or failed, 
   use #'patmatch and examine the returned state object.)
   Returns (values t captures) on success.
  (values nil captures) on failure."
  (let ((state (make-instance 'state
                   :pos 0
                   :string string
                   :len (length string))))
    (multiple-value-bind (success? newstate)
                         (inner-patmatch state pattern)
      (values success?
              ; reverse captures to reflect true order in which they were found
              (reverse (get-captures newstate))))))

(defun whitep (char)
  "Only whitespace chars will match."
  (member char '(#\Space #\Tab #\Return #\Linefeed)))

;;; CL functions alpha-char-p, alphanumericp, digit-char-p, graphic-char-p, standard-char-p are all also available here.

(defun non-whitep (char)
  "Only non-whitespace chars will match."
  (not (whitep char)))

(defun any-char (char)
  "Any character will match."
  (declare (ignore char))
  t)

(defun any-char-but (char-bag)
  "This one's a little different than the above -- this _returns_ a predicate you
  can use to exclude a character or set of characters.
  char-bag can be a list of chars, a string, or a single character."
  (when (characterp char-bag)
    (setf char-bag (string char-bag)))
  (complement
   (typecase char-bag
     (string (lambda (char)
               (find char char-bag)))
     (cons (lambda (char)
             (member char char-bag))))))


