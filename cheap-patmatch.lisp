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
;;;    The success/failure boolean -- if true -- might not be literally T, but it will often be a keyword
;;;      or a cons that gives a clue about the last test that succeeded. This is mostly useful for
;;;      debugging; the typical user need only check that it is non-nil.

;;; See tests for examples.
;;; See #'primitive-pattern-dispatch methods for primitive matching keywords and how they work.
;;; Meta-pattern keywords include :seq, :or, :and, :not, and :break.

;;; Note that <true> in docstrings herein simply means non-nil. It may or may not be the literal T.

(in-package :cheap-patmatch)

(defclass state ()
  ((pos :initarg :pos :initform nil :accessor get-pos)
   (string :initarg :string :initform nil :accessor get-string)
   (len :initarg :len :initform nil :accessor get-string-length)
   (captures :initarg :captures :initform nil :accessor get-captures
                 :documentation "An alist of strings collected with :capture forms.")))

(defmethod print-object ((self state) output-stream)
  "For debugging"
  (print-unreadable-object (self output-stream :type t :identity t)
    (format output-stream "Pos=~D" (get-pos self))))

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
  
(defun some-match (state pattern binding-scope)
  "Like #'some but expects fn to return two values: A boolean and a second value which is the actual result.
  Decision whether to proceed is made on the first value.
  Overall value returned is (values <true> result) or (values nil result). So that's the key difference from
  #'some: We always return a result as a second value."
  (if (car pattern)
    (multiple-value-bind (success? newstate)
                         (inner-patmatch state (car pattern) binding-scope)
      (if success?
          (if (eql :break success?) ;; don't let :break short-circuit an :or
              (some-match state (cdr pattern) binding-scope)
              (values success? newstate))
          (some-match state (cdr pattern) binding-scope)))
    (values nil state)))

(defun every-match (state pattern binding-scope)
  "Like #'every but expects fn to return two values: A boolean and a second value which is the actual result.
  Decision whether to proceed is made on the first value.
  Overall value returned is (values <true> result) or (values nil result). So that's the key difference from
  #'every: We always return a result as a second value."
  (if (car pattern)
    (multiple-value-bind (success? newstate)
                         (inner-patmatch state (car pattern) binding-scope)
      (if success?
          (every-match state (cdr pattern) binding-scope)
          (values nil newstate)))
    (values :eop state))) ; :eop means 'end-of-pattern'

(defun massage-arg-into-fn (arg)
  "If arg is a function, just return it.
   If arg is a character, return a function that matches that character.
   If arg is a string, return a function that matches any character in that string, case-sensitively."
  (typecase arg
    (function arg)
    (symbol arg) ; assume it represents a function
    (character (lambda (char) (char= char arg)))
    (string (lambda (char) (find char arg :test #'char=)))
    (cons (case (car arg) ;;; NDY! I don't know if these work yet.
            (:anyof-cs
              (lambda (char)
                (find char (second arg) :test #'char=)))
            (:anyof-ci
             (lambda (char)
                (find char (second arg) :test #'char-equal)))))))

(defgeneric primitive-pattern-dispatch (kwd fn state)
  (:documentation
    "Handles the primitive pattern keywords. Primitive patterns always look like (keyword fn),
     and they never contain subpatterns, which is why this function does not accept a pattern
     argument.
    Fn is a predicate of one argument: a character.
    Fn can also be one of several other things. See #'massage-arg-into-fn for details.
    Returns (values <true> updated-state) on success.
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
               (values :string= (copy-state state endpos))
               (values nil state)))
          (t (values nil state))))) ; we're out of string but not out of pattern. This means failure.

(defmethod primitive-pattern-dispatch ((kwd (eql :case-sensitive)) literal-string state)
  "Require that current string match a literal string case-sensitively. Syntactic sugar; same as :string."
  (primitive-pattern-dispatch :string literal-string state))

(defmethod primitive-pattern-dispatch ((kwd (eql :case-insensitive)) literal-string state)
  "Require that current string match a literal string case-insensitively."
  (let* ((pos (get-pos state))
         (string (get-string state))
         (len (get-string-length state))
         (endpos (+ pos (length literal-string))))
    (cond ((and (< pos len)
                (<= endpos len))
           ; keep going
           (if (string-equal literal-string (subseq string pos endpos))
               (values :string-equal (copy-state state endpos))
               (values nil state)))
          (t (values nil state)))))

(defmethod primitive-pattern-dispatch ((kwd (eql :lookahead-string)) literal-string state)
  "Require that current string match a literal string. Don't move pos forward in any case."
  (let* ((pos (get-pos state))
         (string (get-string state))
         (len (get-string-length state))
         (endpos (+ pos (length literal-string))))
    (cond ((and (< pos len)
                (<= endpos len))
           ; keep going
           (if (string= literal-string (subseq string pos endpos))
               (values :string= state)
               (values nil state)))
          (t (values nil state)))))

(defmethod primitive-pattern-dispatch ((kwd (eql :zero-or-more)) fn state)
  "Require zero or more characters at current position for which fn returns true.
   New state's pos will be one beyond the last char where the fn returned true.
   The key distinction here is that if we reach the end of the string before we even have a chance to call the function, we still succeed."
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
                 (values :more (copy-state state pos)))
               (values :zero state)))
          (t (values :eos state))))) ; we're out of string prematurely but for _this_ pattern it means success!

(defmethod primitive-pattern-dispatch ((kwd (eql :one-or-more)) fn state)
  "Require at least one character at current position for which fn returns true.
  New state's pos will be one beyond the last char where the fn returned true."
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
                 (values :one-or-more (copy-state state pos)))
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
                       (values :one (copy-state state pos))
                       (values nil state)) ; no need to make a new state here. Initial pos is where it failed.
                   (values :eos (copy-state state pos))) ; no next character at all. End-of-string. This means success.
                (values nil state))) ; first character didn't match
          (t (values nil state))))) ; we're out of string but not out of pattern. This means failure.

(defmethod primitive-pattern-dispatch ((kwd (eql :one-nongreedy)) fn state)
  "Require character at current position for which fn returns true.
  Does not do lookahead; this only looks for a single character and succeeds
  if it finds one, and moves the state position one character forward."
  (setf fn (massage-arg-into-fn fn))
  (let ((pos (get-pos state))
        (string (get-string state))
        (len (get-string-length state)))
    (cond ((< pos len)
           ; keep going
           (if (funcall fn (char string pos))
               ; okay we have one char match. 
               (values :one (copy-state state (1+ pos)))
               (values nil state))) ; char didn't match
          (t (values nil state))))) ; we're out of string but not out of pattern. This means failure.

#|
These are the pattern clauses.
In the below, <clause> is one of the pattern clauses.
              <clause>* is one or more pattern clauses.
              <s> must be a literal string.
              <fn> must be one of: (see #'massage-arg-into-fn)
                   -- A single-argument function which is given the next character and is treated as a predicate.
                   -- A symbol, which is assumed to be a single-argument function as above.
                   -- A character, which gets turned into a function that matches just that character, case-sensitively.
                   -- <s>, which gets turned into a function that matches any character in that string, case-sensitively.
                   -- A 2-list of (:anyof-ci <s>), which gets turned into a function that matches any character in that string, case-insensitively.
                   -- A 2-list of (:anyof-cs <s>). This behaves exactly like the case of <s> by itself.
              <name> must be a symbol or the literal NIL.
              <nns>  must be a non-nil symbol.

<s> -- A solitary string can itself be a clause. In which case the characters in that string will be matched sequentially and case-insensitively.

<nns> -- A non-nil symbol can itself be a clause, in which case it is assumed to be the name of a set of clauses previously defined
         with the :named construct.

(:string <s>) -- Match a literal string e.g. (:string "foo"). This form is not really necessary; you can just say "foo" by itself instead.

(:lookahead-string <s>) -- Like :string but this one does not change the current position. Therefore any subsequent pattern will start
        at the same place this pattern did. e.g. (:seq (:lookahead-string "foo") "foobar") will succeed on the string "foobar".

(:one-or-more <fn>) -- Require at least one character at current position for which fn returns true.
        New state's pos will be one beyond the last char where the fn returned true.

(:zero-or-more <fn>) -- Require zero or more characters at current position for which fn returns true.
        New state's pos will be one beyond the last char where the fn returned true.
        The key distinction here is that if we reach the end of the string before we even have a chance to call the function, we still succeed.

(:one <fn>) -- Require a character at current position for which fn returns true.
        If there's no next character after that, we succeed and increment pos by 1.
        If there is a next character after that, require fn called on it to return false, and if so we succeed and increment pos by 1.
        Thus this does single-character lookahead beyond the current position.

(:one-nongreedy <fn>) -- Require a character at current position for which fn returns true.
        Does not do lookahead beyond the current position; this only looks at a single character and succeeds
        if it finds one, and increments the state position by 1.

(:seq <clause>*) -- Perform pattern clauses in order.
        Each updates position and captures before proceeding to next clause.
        If any clause fails, overall pattern fails.
        (The literal :seq keyword is not usually required except to create a sequential pattern inside
         another meta-pattern keyword like :and, :or, or :not.
         In all other cases :seq is implied.)

(:capture <name> <clause>*) -- Perform pattern clauses in order.
        This is identical to :seq except if all the pattern clauses result in a successful match, the substring
        that matches will be pushed onto the captures list of the state object, and #'ppatmatch will return them.
        <name> can be a string or a non-NIL symbol in which case that name will be consed onto the matching string before
        being pushed on the captures list so you can find it quickly.
        <name> can also be NIL, in which case the matching string will be pushed 'naked' onto the captures list.

(:named <nns> <clause>*) -- Perform pattern clauses in order.
        This is identical to :seq except it gives the set of clauses that follow a name. That name can then be
        used _within_ those clauses to create recursive patterns.

(:not <clause>) -- Only a single pattern clause should follow.
        If that clause fails, overall clause succeeds, and vice-versa.

(:or <clause>*)  -- Perform pattern clauses in order.
        Each starts at same string position as previous, and updates captures where appropriate.
        If any clause succeeds, overall pattern succeeds. Has short-circuit behavior.

(:and <clause>*) -- Perform pattern clauses in order.
        Each starts at same string position as previous, and updates captures where appropriate.
        If any clause fails, overall pattern fails. Has short-circuit behavior.

(:eos)   -- End-of-string. Always true if we're at the end of the string.

(:break) -- Throws a break. Useful for debugging. Breaks always succeed, so continuing after the break just continues.
         :or makes a special provision for :break so it won't short-circuit.
         :not makes no such provision; an enclosed (:break) just causes overall failure.
|#

(defclass pattern-binding-scope ()
  ((parent-scope :initarg :parent-scope :initform nil :accessor parent-scope
                 :documentation "Contains parent pattern-binding-scope. Outermost scope will have nil here.")
   (local-table :initarg :local-table :initform nil :accessor local-table))
  (:documentation "Strictly for named patterns. A typical binding scope data structure."))

(defmethod lookup-key (key (self pattern-binding-scope))
  (or (and (local-table self)
           (gethash key (local-table self)))
      (when (parent-scope self)
        (lookup-key key (parent-scope self)))))

(defun make-pattern-binding-scope-table ()
  "Make a table for a pattern-binding-scope object.
   Keys will always be symbols."
  (make-hash-table :test #'eql))

(defmethod (setf lookup-key) (newvalue key (self pattern-binding-scope))
  (with-slots (local-table) self
    (if local-table
        (let ((current-value (gethash key local-table)))
          (when current-value
            (warn "Redefining pattern ~S in current scope." key))) ;; not sure if this is always a bad thing but let user know it happened
        (setf local-table (make-pattern-binding-scope-table))) ;; don't cons up a table unless we actually need one
    (setf (gethash key local-table) newvalue)))

(defun new-binding-scope (&optional bs)
  "Always returns a new binding scope whose parent is bs, which could either be another binding-scope or nil."
  (make-instance 'pattern-binding-scope
    :parent-scope bs))

; Policy decision: meta-pattern keywords introduce new binding scopes EXCEPT for :seq and :named.
; Primitive-pattern keywords never introduce new binding scopes because they cannot contain subpatterns.
(defun inner-patmatch (state pattern &optional binding-scope)
  #+debug (declare (optimize (debug 3)))
  (flet ((do-sequentially (state pattern binding-scope)
           ; Execute car and cdr in sequence, with each seeing a (potentially) updated position in the string.
           ; Do NOT create a new binding scope herein, because sequentially means "not nested"
           (multiple-value-bind (success? newstate)
                                (inner-patmatch state (car pattern) binding-scope)
             (if success?
                 (if (cdr pattern) ; minor short circuit that changes nothing semantically but it avoids a redundant recursive call
                     (inner-patmatch newstate (cdr pattern) binding-scope)
                     (values success? newstate))
                 (values nil newstate))))
         (ensure-binding-scope ()
           "Ensure the local variable binding-scope contains a binding-scope, i.e. don't cons one up unless we need it."
           (unless binding-scope
             (setf binding-scope (new-binding-scope)))))
    (if pattern
        (cond ((stringp pattern)
               ; syntactic sugar so pattern can be a string to be matched literally
               (primitive-pattern-dispatch :string pattern state))
              ((symbolp pattern) ; it's a named pattern
               ;(format t "~%looking up pattern named ~S" pattern)
               (unless binding-scope
                 (error "Pattern named ~A encountered when no patterns (at all) have been defined" pattern))
               (let ((named-pattern (lookup-key pattern binding-scope))) ;;; NDY should we memorize current binding-scopes along with named patterns?? Probably
                 (if named-pattern
                     (inner-patmatch state named-pattern binding-scope)
                     (error "Pattern named ~A not in scope" pattern))))
              ((consp pattern)
               (let ((carpat (car pattern)))
                 (cond ((keywordp carpat)
                        ; check for meta-pattern keywords and handle them first. Meta-patterns contain other patterns.
                        (case carpat
                          (:seq
                           (do-sequentially state (cdr pattern) binding-scope))
                          
                          (:named ; A named pattern. Useful for recursive patterns.
                           (ensure-binding-scope)
                           (let ((pattern-name (second pattern)))
                             (unless (and pattern-name
                                          (symbolp pattern-name))
                               (error "Improper pattern name found: ~S. Must be a non-nil symbol." pattern-name))
                             ;(format t "~%New named pattern ~S" pattern-name)
                             (setf (lookup-key pattern-name binding-scope) (cddr pattern))
                             (inner-patmatch state (cddr pattern) binding-scope)))
                          
                          (:capture
                           ; second must be a symbol or string to name the capture group
                           ; cddr is assumed to be a sequential meta-pattern
                           (let ((capture-name (second pattern)))
                             (unless (or (symbolp capture-name)
                                         (stringp capture-name))
                               (error "Improper capture name found: ~S" capture-name))
                             (multiple-value-bind (success? newstate)
                                                  (do-sequentially state (cddr pattern) (new-binding-scope binding-scope))
                               (cond (success?
                                      ;; Record the match on newstate and return newstate.
                                      ;; Callee cannot do this because it don't know about the :capture clause it's within.
                                      (if (and (consp success?)
                                               (eql :token (car success?))) ; Don't get cute. Token might be literally nil, thus we need the :token keyword here
                                          (push (if capture-name ; it's a token, not a string. So push the token
                                                    (cons capture-name (second success?))
                                                    (second success?)) ; capture name is explicitly nil (anonymous). So push just the string, not a cons.
                                                (get-captures newstate))
                                          
                                          (let ((string-found (subseq (get-string newstate) ;; <-- Here's the magic. Study this and understand it.
                                                                      (get-pos state)
                                                                      (get-pos newstate))))
                                            (push (if capture-name
                                                      (cons capture-name string-found)
                                                      string-found) ; capture name is explicitly nil (anonymous). So push just the string, not a cons.
                                                  (get-captures newstate))))
                                      (values success? newstate))
                                     (t (values nil newstate)))))) ; we already know newstate represents a failed match,
                          ; but it contains potentially-valuable position
                          ; and capture information, so return it.
                          
                          (:not ; should contain only a single subpattern (which could itself be wrapped in another meta-pattern keyword).
                           (multiple-value-bind (success? newstate)
                                                (inner-patmatch state (second pattern) (new-binding-scope binding-scope))
                             (if success?
                                 (values nil state)
                                 (values :not newstate))))
                          
                          (:or ; cdr of pattern will be evaluated in sequence but each evaluation starts at the same position
                           ; Any success wins immediately.
                           (some-match state (cdr pattern) (new-binding-scope binding-scope)))
                          
                          (:and ; cdr of pattern will be evaluated in sequence but each evaluation starts at the same position
                           ; Any failure loses immediately.
                           (every-match state (cdr pattern) (new-binding-scope binding-scope)))

                          (:read-token ; uses Lisp reader to read a token at current position
                           (multiple-value-bind (result newpos)
                                                       (handler-case
                                                           (let ((*read-eval* nil))
                                                             (read-from-string (get-string state) t nil :start (get-pos state)))
                                                         (error () nil))
                             (if newpos ; the read was successful. This is how we can tell the difference between a nil result and reading from the string "nil"
                               (values `(:token ,result) (copy-state state newpos))
                               (values nil state))))
                          
                          (:break (break) ; just for debugging patterns
                                  (values :break state)) ; break always succeeds so we'll go to the next thing
                          
                          (:eos ; End-of-string. Always true if we're at the end of the string.
                           (if (>= (get-pos state) (get-string-length state))
                               (values :eos state)
                               (values nil state)))
                                                    
                          (t ; any other keyword indicates a primitive pattern
                           (primitive-pattern-dispatch carpat
                                                       (second pattern) ; fn
                                                       state))))
                       
                       (t ; No opening keyword, so treat pattern as if it opened with :SEQ.
                        (do-sequentially state pattern binding-scope)
                        ))))
              (t (error "Pattern must be of type string, symbol, or cons. ~S" pattern)))
        
        ; pattern is empty. This is success.
        (values :empty state))))

(defun patmatch (string pattern)
  "Returns (values <true> state) on success.
  (values nil state) on failure."
  (let ((state (make-instance 'state
                   :pos 0
                   :string string
                   :len (length string))))
    (inner-patmatch state pattern)))

(defun ppatmatch (string pattern)
  "Pretty version of patmatch. This is the primary user interface if all you care about
   is whether the match succeeded and what the captures were.
   (If you care about the position within the string where the match succeeded or failed, 
   use #'patmatch and examine the returned state object.)
   Returns (values <true> captures) on success.
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

;;; Character Predicates

(defun newlinep (char)
  "Matches the newline character in this implementation. Note this
   won't work on systems that use 2-character newlines."
  (eql char #\newline))

(defun whitep (char)
  "Only whitespace chars will match."
  (member char '(#\Space #\Tab #\Return #\Linefeed)))

;; Note that CL standard predicates like alpha-char-p, alphanumericp, digit-char-p, graphic-char-p, standard-char-p are all also available here.

(defun hex-char-p (char)
  (digit-char-p char 16))

(defun non-whitep (char)
  "Only non-whitespace chars will match."
  (not (whitep char)))

;; wildcards
(defun any-char (char)
  "Any character will match."
  (declare (ignore char))
  t)

(defun any-char-but (char-bag &optional (test #'char=))
  "This one's a little different than the above -- this _returns_ a predicate you
  can use to exclude a character or set of characters.
  char-bag can be a list of chars, a string, or a single character."
  (when (characterp char-bag)
    (setf char-bag (string char-bag)))
  (complement
   (typecase char-bag
     (string (lambda (char)
               (find char char-bag :test test)))
     (cons (lambda (char)
             (member char char-bag :test test))))))

;;; NDY DON'T NEED THIS. ONE-OR-MORE WILL DO THIS
(defun anyof (char-bag &optional (test #'char=))
  "Returns a predicate that matches any character in char-bag"
  (when (characterp char-bag)
    (setf char-bag (string char-bag)))
  (typecase char-bag
    (string (lambda (char)
              (find char char-bag :test test)))
    (cons (lambda (char)
            (member char char-bag :test test)))))


