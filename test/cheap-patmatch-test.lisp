;;; cheap-patmatch-test.lisp
;;; 06-Mar-2023 SVS
;;; Tests for cheap-patmatch

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

(in-package :cheap-patmatch)

(defparameter *debug* nil
  "True if you want intermediate timings to print out and show inspector on failures")

(eval-when (:load-toplevel :execute)
  (lisp-unit:remove-tests :all))

(defmacro deftest (testname
                   string
                   pattern
                   desired-result)
  `(lisp-unit:define-test ,testname
                          (lisp-unit:assert-equality #'equalp
                                                     ,desired-result
                                                     (multiple-value-list
                                                      (ppatmatch  ,string
                                                                  ,pattern)))))

#+IGNORE ; Without the macro. Too much typing.
(lisp-unit:define-test basic-match
             (lisp-unit:assert-equality #'equalp
                              '(t nil)
                              (multiple-value-list
                              (ppatmatch  "(defun"
                                          `(:one ,(lambda (char) (char= #\( char)))))))

(deftest basic-match
         "(defun"
         `(:one ,(lambda (char) (char= #\( char)))
  '(t nil))

(deftest single-opening-paren
         "(defun"
  `(:capture opening
             (:one ,(lambda (char) (char= #\( char))))
  '(T
    ((CHEAP-PATMATCH::OPENING . "("))))

(deftest single-paren-plus-d
         "(ddefun"
  `((:one ,(lambda (char) (char= #\( char)))
    (:one ,(lambda (char) (char= #\d char))))
  '(nil nil))

(deftest single-paren-plus-space
         "( defun" `((:one ,(lambda (char) (char= #\( char)))
                     (:one ,(lambda (char) (char= #\space char))))
  '(t nil))

(deftest single-paren-plus-two-spaces
         "(  defun"
  `((:one ,(lambda (char) (char= #\( char)))
    (:one ,(lambda (char) (char= #\space char))))
  '(nil nil))
  
(deftest single-paren-plus-two-spaces-with-captures
         "(  defun" 
  `((:one ,(lambda (char) (char= #\( char)))
    (:zero-or-more whitep)
    (:capture defform
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture defname
              (:one-or-more non-whitep)))
  '(NIL   ; fails because we ran out of string before we ran out of pattern. But captures may still be useful.
    ((DEFFORM . "defun"))))


(deftest double-parens
         "((  defun"
  `((:one ,(lambda (char) (char= #\( char)))
    (:zero-or-more whitep)
    (:capture defform
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture defname
              (:one-or-more non-whitep)))
  '(NIL   ; fails because there are two opening parens
    NIL))

(deftest simple-defconstant
         "(defconstant foobar 35) ; compute foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture defname
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture value
              (:one-or-more ,(lambda (char)
                               (and (non-whitep char)
                                    (not (char= #\) char))))))
    (:zero-or-more whitep)
    (:one #\))
    (:zero-or-more whitep)
    (:capture comment
              (:one-or-more any-char)))
  '(T
    ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (COMMENT . "; compute foobars"))))

(deftest docstring1
         "(defconstant foobar 35) ; compute foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture defname
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture value
              (:one-or-more ,(lambda (char)
                               (and (non-whitep char)
                                    (not (char= #\) char))))))
    (:zero-or-more whitep)
    (:or (:capture docstring
                   (:one #\")
                   (:zero-or-more any-char)
                   (:one #\"))
         (:one #\)))
    (:zero-or-more whitep)
    (:capture comment
              (:one-or-more any-char)))
  '(T  ;  Good. Still works on original pattern and doesn't capture docstring because there isn't one.
    ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (COMMENT . "; compute foobars"))))

(deftest docstring2
         "(defconstant foobar 35 ) ; compute foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture defname
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture value
              (:one-or-more ,(lambda (char)
                               (and (non-whitep char)
                                    (not (char= #\) char))))))
    (:zero-or-more whitep)
    (:or (:capture docstring
                   (:one #\")
                   (:zero-or-more any-char)
                   (:one #\"))
         (:one #\)))
    (:zero-or-more whitep)
    (:capture comment
              (:one-or-more any-char)))
  '(T   ; Still works if there are spaces before the close paren
    ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (COMMENT . "; compute foobars"))))

(deftest docstring3
         "(defconstant foobar 35 \"my docstring\") ; compute foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture defname
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture value
              (:one-or-more ,(lambda (char)
                               (and (non-whitep char)
                                    (not (char= #\) char))))))
    (:zero-or-more whitep)
    (:or (:capture docstring
                   (:one #\")
                   (:zero-or-more any-char)
                   (:one #\"))
         (:one #\)))
    (:zero-or-more whitep)
    (:capture comment
              (:one-or-more any-char)))
  '(NIL    ; OOPS! Fails because (:zero-or-more any-char) was grabbing the last #\"
    ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35"))))

(deftest docstring4
         "(defconstant foobar 35 \"my docstring\") ; compute foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture defname
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture value
              (:one-or-more ,(lambda (char)
                               (and (non-whitep char)
                                    (not (char= #\) char))))))
    (:zero-or-more whitep)
    (:or (:capture docstring
                   (:one #\")
                   (:zero-or-more ,(lambda (char) (not (char= #\" char))))
                   (:one #\"))
         (:one #\)))
    (:zero-or-more whitep)
    (:capture comment
              (:one-or-more any-char)))
  '(T    ; Now it works! But it's still not quite right because final closing paren is included in comment
    ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (DOCSTRING . "\"my docstring\"") (COMMENT . ") ; compute foobars"))))

(deftest docstring-with-seq1
         "(defconstant foobar 35 \"my docstring\") ; compute foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture defname
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture value
              (:one-or-more ,(lambda (char)
                               (and (non-whitep char)
                                    (not (char= #\) char))))))
    (:zero-or-more whitep)
    (:or (:seq (:capture docstring
                         (:one #\")
                         (:zero-or-more ,(lambda (char) (not (char= #\" char))))
                         (:one #\"))
               (:zero-or-more whitep)
               (:one #\)))
         (:one #\)))
    (:zero-or-more whitep)
    (:capture comment
              (:one-or-more any-char)))
  '(T    ; Fixed! Note the comment doesn't contain the closing paren any more
    ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (DOCSTRING . "\"my docstring\"") (COMMENT . "; compute foobars"))))

; Improve the above with less clumsy syntax for dealing with the closing quote (see comment)
(deftest docstring-with-seq2
         "(defconstant foobar 35 \"my docstring\") ; compute foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture defname
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture value
              (:one-or-more ,(lambda (char)
                               (and (non-whitep char)
                                    (not (char= #\) char))))))
    (:zero-or-more whitep)
    (:or (:seq (:capture docstring
                         (:one #\")
                         (:zero-or-more ,(any-char-but #\")) ; <-- easier syntax than docstring-with-seq1
                         (:one #\"))
               (:zero-or-more whitep)
               (:one #\)))
         (:one #\)))
    (:zero-or-more whitep)
    (:capture comment
              (:one-or-more any-char)))
  '(T
    ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (DOCSTRING . "\"my docstring\"") (COMMENT . "; compute foobars"))))




; (lisp-unit:run-tests :all :cheap-patmatch)