;;; cheap-patmatch-tests.lisp
;;; 06-Mar-2023 SVS

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


; Look for a single opening paren
(ppatmatch  "(defun" `(:one ,(lambda (char) (char= #\( char))))
;; T    ; success, but nothing was captured. Sometimes that's exactly what you want.
;; NIL

; Look for a single opening paren and capture it with the name "OPENING"
(ppatmatch  "(defun" `(:capture opening
                        (:one ,(lambda (char) (char= #\( char)))))
;; T
;; ((OPENING . "("))

; Look for a single opening paren followed by a single 'd'
(ppatmatch "(ddefun" `((:one ,(lambda (char) (char= #\( char)))
                     (:one ,(lambda (char) (char= #\d char)))))
;; NIL  ; failure, because there are two d's and we specified one
;; NIL

; Look for a single opening paren followed by one space
(ppatmatch "( defun" `((:one ,(lambda (char) (char= #\( char)))
                     (:one ,(lambda (char) (char= #\space char)))))
;; T
;; NIL

(ppatmatch "(  defun" `((:one ,(lambda (char) (char= #\( char)))
                     (:one ,(lambda (char) (char= #\space char)))))
;; NIL   ; failed because there are two spaces after the paren
;; NIL

(ppatmatch "(  defun" `((:one ,(lambda (char) (char= #\( char)))
                       (:zero-or-more whitep)
                       (:capture defform
                                    (:one-or-more non-whitep))
                       (:one-or-more whitep)
                       (:capture defname
                                    (:one-or-more non-whitep))))
;; NIL   ; failed because we ran out of string before we ran out of pattern. But captures may still be useful.
;; ((DEFFORM . "defun"))

(ppatmatch "((  defun" `((:one ,(lambda (char) (char= #\( char)))
                         (:zero-or-more whitep)
                         (:capture defform
                                   (:one-or-more non-whitep))
                         (:one-or-more whitep)
                         (:capture defname
                                   (:one-or-more non-whitep))))
;; NIL   ; failed because there are two opening parens
;; NIL

; Look for a typical pattern of a defconstant form, with comment following
(ppatmatch "(defconstant foobar 35) ; compute foobars"
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
                       (:one-or-more any-char))))
;; T
;; ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (COMMENT . "; compute foobars"))

; Try and handle docstrings
(ppatmatch "(defconstant foobar 35) ; compute foobars"
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
                       (:one-or-more any-char))))
;; T  ;  Good. Still works on original pattern and doesn't capture docstring because there isn't one.
;; ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (COMMENT . "; compute foobars"))


(ppatmatch "(defconstant foobar 35 ) ; compute foobars"
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
                       (:one-or-more any-char))))
;; T   ; Still works if there are spaces before the close paren
;; ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (COMMENT . "; compute foobars"))

(ppatmatch "(defconstant foobar 35 \"my docstring\") ; compute foobars"
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
                       (:one-or-more any-char))))
;; NIL    ; OOPS! Failed because (:zero-or-more any-char) was grabbing the last #\"
;; ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35"))

; Now it handles docstrings
(ppatmatch "(defconstant foobar 35 \"my docstring\") ; compute foobars"
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
                       (:one-or-more any-char))))
;; T    ; Now it works! But it's still not quite right because final closing paren is included in comment
;; ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (DOCSTRING . "\"my docstring\"") (COMMENT . ") ; compute foobars"))

; Fix the above by using an explicit :SEQ inside the :OR
(ppatmatch "(defconstant foobar 35 \"my docstring\") ; compute foobars"
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
                       (:one-or-more any-char))))
;; T    ; Fixed! Note the comment doesn't contain the closing paren any more
;; ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (DOCSTRING . "\"my docstring\"") (COMMENT . "; compute foobars"))

; Improve the above with less clumsy syntax for dealing with the closing quote (see comment)
(ppatmatch "(defconstant foobar 35 \"my docstring\") ; compute foobars"
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
                            (:zero-or-more ,(any-char-but #\")) ; <-- easier syntax than above*
                            (:one #\"))
                        (:zero-or-more whitep)
                        (:one #\)))
                  (:one #\)))
             (:zero-or-more whitep)
             (:capture comment
                       (:one-or-more any-char))))
;; T
;; ((DEFFORM . "defconstant") (DEFNAME . "foobar") (VALUE . "35") (DOCSTRING . "\"my docstring\"") (COMMENT . "; compute foobars"))


#|
*Note: Here's where things start to get a bit weird with regexes, at least for me.
What exactly does "zero or more occurrences of any character but foo" actually mean?
One way to think about it is to start with a character set with all the characters but foo.
Let's say foo is "DEFG".
Then all the characters but those might be "ABCHIJKLMNOPQRSTUVWXYZ1234567890" [ignoring lowercase, punctuation, etc for this example]
In this case "zero or more occurrences of any character but foo" means
'zero or more occurrences of "ABCHIJKLMNOPQRSTUVWXYZ1234567890"'
|#

(ppatmatch "  " `(:one-or-more whitep))
;; T
;; NIL

; Detect definition symbols at beginning of an sexp.
;  (Don't use this in real life because it won't keep track of matched parens in body.)
(ppatmatch "(defun foobar 35) ; process the foobars"
          `((:one #\()
            (:zero-or-more whitep)
            (:capture defform
                         (:string "def")
                         (:one-or-more non-whitep))
            (:capture body
                         (:one-or-more ,(lambda (char)
                                               (not (char= #\) char)))))))
;; T
;; ((DEFFORM . "defun") (BODY . " foobar 35"))

; Like above but we don't have to spell out the keyword :string. This way is nicer.
(ppatmatch "(defun foobar 35) ; process the foobars"
          `((:one #\()
            (:zero-or-more whitep)
            (:capture defform
                         "def"
                         (:one-or-more non-whitep))
            (:capture body
                         (:one-or-more ,(lambda (char)
                                               (not (char= #\) char)))))))
;; T
;; ((DEFFORM . "defun") (BODY . " foobar 35"))
           
; Now search for any defining form EXCEPT "defun"
(ppatmatch "(defun foobar 35) ; process the foobars"
          `((:one #\()
            (:zero-or-more whitep)
            (:capture defform
                         (:and "def" ; note nice lookahead
                               (:not "defun"))
                         (:one-or-more non-whitep))
            (:capture body
                         (:one-or-more ,(lambda (char)
                                               (not (char= #\) char)))))))
;; NIL  ; failure because it's "defun"
;; NIL

(ppatmatch "(defu"
          `((:one #\()
            (:zero-or-more whitep)
            (:capture defform
                         (:and "def" ; note nice lookahead
                               (:not "defun"))
                         (:one-or-more non-whitep))
            (:capture body
                         (:one-or-more ,(lambda (char)
                                               (not (char= #\) char)))))))
;; NIL   ; Failed because we ran out of string before we ran out of pattern. But note the defform part succeeded.
;; ((DEFFORM . "defu")) ; Lesson: Even overall failure of the pattern can still partially succeed and be useful.

(ppatmatch "(defu"
          `((:one #\()
            (:zero-or-more whitep)
            (:capture defform
                         (:and "def" ; note nice lookahead
                               (:not "defun"))
                         (:one-or-more non-whitep))))
;; T    ; Success because we shortened the pattern. Now the overall pattern can succeed.
;; ((DEFFORM . "defu"))

(ppatmatch "(defconstant foobar 35) ; process the foobars"
          `((:one #\()
            (:zero-or-more whitep)
            (:capture defform
                         (:and "def"
                               (:not "defun"))
                         (:one-or-more non-whitep))
            (:capture body
                         (:one-or-more ,(lambda (char)
                                               (not (char= #\) char)))))))
;; T
;; ((DEFFORM . "defconstant") (BODY . " foobar 35"))

(ppatmatch "(defconstant foobar 35) ; process the foobars"
          `("defmacro"))
;; NIL   ; Failed because we were looking for "defmacro"
;; NIL

(ppatmatch "(defconstant foobar 35) ; process the foobars"
          `("defconstant"))
;; NIL   ; Failed because we(didn't include an opening paren in the pattern
;; NIL

(ppatmatch "(defconstant foobar 35) ; process the foobars"
          `("(defconstant"))
;; T
;; NIL

(ppatmatch "(defconstant foobar 35) ; process the foobars"
          `(:capture defform
                        "(defconstant"))
;; T
;; ((DEFFORM . "(defconstant"))

(ppatmatch "dabcdef"
          `(:capture match
                        (:one "abcd")
                        (:zero-or-more any-char)))
;; NIL
;; NIL

(ppatmatch "zdabcdef"
          `(:capture match
                        (:one "xyz")
                        (:zero-or-more any-char)))
;; T
;; ((MATCH . "zxdabcdef"))

(ppatmatch "zxdabcdef"
          `(:capture match
                        (:one-or-more "xyz")
                        (:zero-or-more any-char)))
;; T
;; ((MATCH . "zxdabcdef"))

(ppatmatch "zxdabcdef"
          `(:capture nil ; anonymous capture
                        (:one-or-more "xyz")
                        (:zero-or-more any-char)))
;; T
;; ("zxdabcdef")

;; Demonstrates what happens if you mix named and anonymous captures. 
(ppatmatch "(defconstant foobar 35) ; compute foobars"
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
                         (:one-or-more any-char))))
;; T
;; ((DEFFORM . "defconstant") "foobar" (VALUE . "35") (COMMENT . "; compute foobars"))
