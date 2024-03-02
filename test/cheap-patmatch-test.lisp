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

; NDY: Examples of (:not with a :seq clause).
; NDY: Examples of (:break). Tricky to test.
; NDY: Examples of how a literal string as a clause is like a :seq followed by individual characters.
; NDY: Do something with *debug* below
; NDY: Test case-insensitive matching, including with #'any-char-but
; NDY: Test read-token capability
; NDY: Test #'newlinep
; NDY: Test #'anyof or get rid of it.

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
  '(:one nil))

(deftest single-opening-paren
         "(defun"
  `(:capture opening
             (:one ,(lambda (char) (char= #\( char))))
  '(:one
    ((CHEAP-PATMATCH::OPENING . "("))))

(deftest single-paren-plus-d
         "(ddefun"
  `((:one ,(lambda (char) (char= #\( char)))
    (:one ,(lambda (char) (char= #\d char))))
  '(nil nil))

(deftest single-paren-plus-space
         "( defun" `((:one ,(lambda (char) (char= #\( char)))
                     (:one ,(lambda (char) (char= #\space char))))
  '(:one nil))

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
    (:break)
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
  '(:ONE-OR-MORE
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
  '(:ONE-OR-MORE  ;  Good. Still works on original pattern and doesn't capture docstring because there isn't one.
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
  '(:ONE-OR-MORE   ; Still works if there are spaces before the close paren
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
  '(:ONE-OR-MORE    ; Now it works! But it's still not quite right because final closing paren is included in comment
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
  '(:ONE-OR-MORE    ; Fixed! Note the comment doesn't contain the closing paren any more
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
  '(:ONE-OR-MORE
    ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (DOCSTRING . "\"my docstring\"") (COMMENT . "; compute foobars"))))

(deftest basic-whitespace
         "  "
  `(:one-or-more whitep)
  '(:ONE-OR-MORE
    NIL))

; Detect definition symbols at beginning of an sexp.
;  (Don't use this in real life because it won't keep track of matched parens in body.)
(deftest definition-symbols1
         "(defun foobar 35) ; process the foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:string "def")
              (:one-or-more non-whitep))
    (:capture body
              (:one-or-more ,(lambda (char)
                               (not (char= #\) char))))))
  '(:ONE-OR-MORE
    ((DEFFORM . "defun") (BODY . " foobar 35"))))

; Like above but we don't have to spell out the keyword :string. This way is nicer.
(deftest definition-symbols2
         "(defun foobar 35) ; process the foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              "def"
              (:one-or-more non-whitep))
    (:capture body
              (:one-or-more ,(lambda (char)
                               (not (char= #\) char))))))
  '(:ONE-OR-MORE
    ((DEFFORM . "defun") (BODY . " foobar 35"))))

; Now search for any defining form EXCEPT "defun"
(deftest definition-symbols3
         "(defun foobar 35) ; process the foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:and "def" ; note nice lookahead
                    (:not "defun"))
              (:one-or-more non-whitep))
    (:capture body
              (:one-or-more ,(lambda (char)
                               (not (char= #\) char))))))
  '(NIL  ; failure because it's "defun"
    NIL))

(deftest too-short-string1
         "(defu"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:and "def" ; note nice lookahead
                    (:not "defun"))
              (:one-or-more non-whitep))
    (:capture body
              (:one-or-more ,(lambda (char)
                               (not (char= #\) char))))))
  '(NIL   ; Fails because we ran out of string before we ran out of pattern. But note the defform part succeeded.
    ((DEFFORM . "defu")) ; Lesson: Even overall failure of the pattern can still partially succeed and be useful.
    ))

(deftest too-short-string2
         "(defu"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:and "def" ; note nice lookahead
                    (:not "defun"))
              (:one-or-more non-whitep)))
  '(:ONE-OR-MORE    ; Success because we shortened the pattern. Now the overall pattern can succeed.
    ((DEFFORM . "defu"))))

(deftest def&body
         "(defconstant foobar 35) ; process the foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:and "def"
                    (:not "defun"))
              (:one-or-more non-whitep))
    (:capture body
              (:one-or-more ,(lambda (char)
                               (not (char= #\) char))))))
  '(:ONE-OR-MORE
    ((DEFFORM . "defconstant") (BODY . " foobar 35"))))

(deftest defmacro1
         "(defconstant foobar 35) ; process the foobars"
  `("defmacro")
  '(NIL   ; Fails because we were looking for "defmacro"
    NIL))

(deftest no-opening-paren
         "(defconstant foobar 35) ; process the foobars"
  `("defconstant")
  '( NIL   ; Fails because we didn't include an opening paren in the pattern
    NIL))

(deftest defconstant-match
         "(defconstant foobar 35) ; process the foobars"
  `("(defconstant")
  '(:STRING=
    NIL))

(deftest defconstant-capture
         "(defconstant foobar 35) ; process the foobars"
  `(:capture defform
             "(defconstant")
  '(:STRING=
    ((DEFFORM . "(defconstant"))))

(deftest dabcdef
         "dabcdef"
  `(:capture match
             (:one "abcd")
             (:zero-or-more any-char))
  '(NIL
    NIL))

(deftest zdabcdef
         "zdabcdef"
  `(:capture match
             (:one "xyz")
             (:zero-or-more any-char))
  '(:MORE
    ((MATCH . "zdabcdef"))))

(deftest zxdabcdef
         "zxdabcdef"
  `(:capture match
             (:one-or-more "xyz")
             (:zero-or-more any-char))
  '(:MORE
    ((MATCH . "zxdabcdef"))))

(deftest zxdabcdef-anonymous
         "zxdabcdef"
  `(:capture nil ; anonymous capture
             (:one-or-more "xyz")
             (:zero-or-more any-char))
  '(:MORE
    ("zxdabcdef")))

(deftest named-plus-anonymous
         "(defconstant foobar 35) ; compute foobars"
  `((:one #\()
    (:zero-or-more whitep)
    (:capture defform
              (:one-or-more non-whitep))
    (:one-or-more whitep)
    (:capture nil ; anonymous
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
  '(:ONE-OR-MORE
    ((DEFFORM . "defconstant") "foobar" (VALUE . "35") (COMMENT . "; compute foobars"))))

(defparameter *balanced-paren-matcher*
  `(:named toplevel
           (:zero-or-more ,(any-char-but "("))
           (:named match-parens
                   (:capture nil
                             ; match-parens starts with pos pointing at a #\( and should end with pos pointing one beyond #\)
                             (:one-nongreedy #\() ; skip past the #\(
                             (:named match-loop
                                     (:or (:seq (:lookahead-string "(")
                                                match-parens
                                                match-loop)
                                          (:one-nongreedy #\))
                                          (:seq (:one-or-more ,(any-char-but "()"))
                                                match-loop)))))
           (:or (:eos) ; this just makes the first value returned (the success boolean) correct. Doesn't affect the captures.
                toplevel))
  "Captures all sets of balanced parens in string")

(defparameter *paren-matching-tests*
  '(
    ("(())" .  ("()" "(())"))
    ("(x y z)" . ("(x y z)"))
    ("((x y z))" . ("(x y z)" "((x y z))"))
    ("((x y z) (foo) )" . ("(x y z)" "(foo)" "((x y z) (foo) )"))
    ("((x y z) (a b c))" . ("(x y z)" "(a b c)" "((x y z) (a b c))"))
    ("bar" . NIL)
    ("bar()" . ("()"))
    ("(boo" . NIL)
    ("(boo))" . ("(boo)"))
    ("(boo)" . ("(boo)"))
    ("abcdef (foo)" . ("(foo)"))
    ("((boo) (baz))" . ("(boo)" "(baz)" "((boo) (baz))"))
    ("((boo) (baz) (barf))" . ("(boo)" "(baz)" "(barf)" "((boo) (baz) (barf))"))
    ("(((boo) (baz)) (barf))" . ("(boo)" "(baz)" "((boo) (baz))" "(barf)" "(((boo) (baz)) (barf))"))
    ("(((boo)))" . ("(boo)" "((boo))" "(((boo)))"))
    ("((boo) ((baz) ((barf))))" . ("(boo)" "(baz)" "(barf)" "((barf))" "((baz) ((barf)))" "((boo) ((baz) ((barf))))"))
    ("((asdf)asdf)" . ("(asdf)" "((asdf)asdf)"))
    ("(defconstant foobar 35 \"my docstring\") ; compute foobars" . ("(defconstant foobar 35 \"my docstring\")"))
    )
  "A-list of correct (input . output) pairs")

(defun run-paren-matching-tests ()
  (flet ((do-test (input output)
           (multiple-value-bind (success? captures)
                                (cpat::ppatmatch input cpat::*balanced-paren-matcher*)
             (declare (ignore success?))
             (unless (equalp captures output)
               (error "Input ~S doesn't produce output ~S" input output))
             t)))
    (every (lambda (correctpair)
             (do-test (car correctpair)
                      (cdr correctpair)))
           *paren-matching-tests*)))

(lisp-unit:define-test paren-match-tests
                        (lisp-unit:assert-true
                         (run-paren-matching-tests)))

; (lisp-unit:run-tests :all :cheap-patmatch)
; (let ((lisp-unit::*print-failures* t)) (lisp-unit:run-tests :all :cheap-patmatch))
; (let ((lisp-unit::*use-debugger* t)) (lisp-unit:run-tests :all :cheap-patmatch))