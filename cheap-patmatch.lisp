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
;;; I'm sick of regular expressions that confuse and intermix the desire for
;;;  1. A single binary result of whether a match happened
;;;  2. A numeric position within the string where the match happened, or where it failed as may be the case.
;;;  3. A captured, named substring that was matched in the string.

(in-package :cl-user)

(defclass state ()
  ((pos :initarg :pos :initform nil :accessor get-pos)
   (string :initarg :string :initform nil :accessor get-string)
   (len :initarg :len :initform nil :accessor get-string-length)
   (matchstrings :initarg :matchstrings :initform nil :accessor get-matchstrings
                 :documentation "An alist of strings collected with :collecting forms.")))

(defmethod copy-state ((self state) &optional newpos)
  (make-instance 'state
    :pos (or newpos (get-pos self))
    :string (get-string self)
    :len (get-string-length self)
    :matchstrings (get-matchstrings self)))

(defmethod incf-pos ((self state))
  (let ((newstate (copy-state self)))
    (incf (get-pos newstate))
    newstate))
  
#|
Three cases:
:seq -- Perform pattern clauses in order.
        Each updates position and matchstrings before proceeding to next clause.
        If any clause fails, overall pattern fails.
        (:seq pattern is not required or expected; :seq is implied unless :or or :and leads pattern)

:or  -- Perform pattern clauses in order.
        Each starts at same position as previous, and updates matchstrings where appropriate.
        If any clause succeeds, overall pattern succeeds.

:and -- Perform pattern clauses in order.
        Each starts at same position as previous, and updates matchstrings where appropriate.
        If any clause fails, overall pattern fails.
|#

(defun some-match (state pattern)
  "Like #'some but expects fn to return two values: A boolean and a second value which is the actual result.
  Decision whether to proceed is made on the first value.
  Overall value returned is (values t result) or (values nil result). So that's the key difference from
  #'some: We always return a result."
  (if (car pattern)
    (multiple-value-bind (success? newstate)
                         (inner-patmatch state (car pattern))
      (if success?
          (values t newstate)
          (some-match state (cdr pattern))))
    (values nil state)))

(defun every-match (state pattern)
  "Like #'every but expects fn to return two values: A boolean and a second value which is the actual result.
  Decision whether to proceed is made on the first value."
  (if (car pattern)
    (multiple-value-bind (success? newstate)
                         (inner-patmatch state (car pattern))
      (if success?
          (every-match state (cdr pattern))
          (values nil newstate)))
    (values t state)))

(defgeneric keyword-dispatch (kwd fn state)
  (:documentation "Fn is a predicate of one argument: a character.
    Returns (values t updated-state) on success.
    (values nil updated-state) on failure."))

(defmethod keyword-dispatch ((kwd (eql :zero-or-more)) fn state)
  "Require at zero or more characters at current position for which fn returns true."
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
          (t (values t state))))) ; we're out of string but for this pattern it means success

(defmethod keyword-dispatch ((kwd (eql :one-or-more)) fn state)
  "Require at least one character at current position for which fn returns true."
  (let ((pos (get-pos state))
        (string (get-string state))
        (len (get-string-length state)))
    (cond ((< pos len)
           ; keep going
           (if (funcall fn (char string pos))
               ; okay we have one char match
               (progn
                 (print (char string pos))
                 (loop while (and (< (incf pos) len)
                                  (funcall fn (char string pos))))
                 (values t (copy-state state pos)))
               (values nil state))) ; not even first char matched
          (t (values nil state))))) ; we're out of string but not out of pattern. This means failure.

(defmethod keyword-dispatch ((kwd (eql :one)) fn state)
  "Require character at current position for which fn returns true.
   If there is a next character, require fn called on it to return false.
   Thus this does single-character lookahead."
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

(defun inner-patmatch (state pattern)
  (if pattern
      (let ((carpat (car pattern)))
        (cond ((keywordp carpat)
               (case carpat
                 (:collecting
                  ; second must be a symbol or string to name the collection
                  ; cddr is assumed to be a sequential pattern
                  (let ((collection-name (second pattern)))
                    (unless (and collection-name
                                 (or (symbolp collection-name)
                                     (stringp collection-name)))
                      (error "Improper or no collection name found: ~S" collection-name))
                    (multiple-value-bind (success? newstate)
                                         (inner-patmatch state (cddr pattern))
                      (cond (success?
                             ; record the match on newstate and return it
                             ;;; Callee cannot do this because they don't know about the :collecting clause they're within
                             (push (cons collection-name
                                         (subseq (get-string newstate)
                                                 (get-pos state)
                                                 (get-pos newstate)))
                                   (get-matchstrings newstate))
                             (values t newstate))
                            (t (values nil newstate))))))
                 
                 (:or ; cdr of pattern will be evaluated in sequence but each evaluation starts at the same position
                  (some-match state (cdr pattern)))
                 
                 (:and ; cdr of pattern will be evaluated in sequence but each evaluation starts at the same position
                  (every-match state (cdr pattern)))
                 
                 (:break (break)
                         (values t state)) ; break always succeeds so we'll go to the next thing
                 
                 (t ; any other keyword indicates a character-by-character predicate
                  (keyword-dispatch carpat
                                    (second pattern) ; fn
                                    state))))
              
              
              (t ; treat pattern as a set of sequential subpatterns
               (multiple-value-bind (success? newstate)
                                    (inner-patmatch state (car pattern))
                 (if success?
                     (inner-patmatch newstate (cdr pattern))
                     (values nil newstate))))))
      
      ; pattern is empty. This is success.
      (values t state)))

(defun patmatch (string pattern)
  "Returns (values t state) on success.
  (values nil state) on failure."
  (let ((state (make-instance 'state
                   :pos 0
                   :string string
                   :len (length string))))
    (inner-patmatch state pattern)))

(defun whitep (char)
  (member char '(#\Space #\Tab #\Return #\Linefeed)))

(defun non-whitep (char)
  (not (whitep char)))

(defun anything (char)
  t)


