;;; cheap-patmatch.lisp
;;; 05-Mar-2023 SVS

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
               (values nil (incf-pos state))))
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
  "Returns (values t state) on success.
   (values nil state) on failure."
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

(patmatch  "(defun" `(:one ,(lambda (char) (char= #\( char))))

(patmatch  "(defun" `(:collecting opening
                        (:one ,(lambda (char) (char= #\( char)))))

(patmatch "(ddefun" `((:one ,(lambda (char) (char= #\( char)))
                     (:one ,(lambda (char) (char= #\d char)))))

(patmatch "( defun" `((:one ,(lambda (char) (char= #\( char)))
                     (:one ,(lambda (char) (char= #\space char)))))

(patmatch "(  defun" `((:one ,(lambda (char) (char= #\( char)))
                      (:zero-or-more whitep)
                      (:collecting defform
                                   (:one-or-more non-whitep))
                       (:one-or-more whitep)
                       (:collecting defname
                                   (:one-or-more non-whitep))))


(patmatch "(defun foobar 35) ; keep track of foobars"
          `((:one ,(lambda (char) (char= #\( char)))
            (:zero-or-more whitep)
            (:collecting defform
                         (:one-or-more non-whitep))
            (:one-or-more whitep)
            (:collecting defname
                         (:one-or-more non-whitep))
            (:one-or-more whitep)
            (:collecting value
                         (:one-or-more non-whitep))
            (:one-or-more whitep)
            (:one ,(lambda (char) (char= #\) char)))
            ;(:zero-or-more whitep)
            (:collecting comment
                         (:one-or-more anything))))

(patmatch "  " `(:one-or-more whitep))

#|
Pattern: We need for string to be

(:one #\()
(:zero-or-more #'whitep)
(:collecting ':defword
  (:one-or-more #'non-whitep))
(:one-or-more #'whitep)
(:collecting ':constant-name
  (:one-or-more #'non-whitep))
(:one-or-more #'whitep)
(:collecting ':value
  (:one-or-more (lambda (char)
                 (and (non-whitep char)
                      (char/= #\))))))
(:one #\))
(:zero-or-more #'whitespacep)
(:collecting comment
             :zero-or-more #'anything))

    

|#