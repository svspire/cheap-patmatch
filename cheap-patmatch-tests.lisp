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
                         (:one-or-more ,(lambda (char)
                                          (and (non-whitep char)
                                               (not (char= #\) char))))))
            (:zero-or-more whitep)
            (:one ,(lambda (char) (char= #\) char)))
            (:zero-or-more whitep)
            (:collecting comment
                         (:one-or-more anything))))

(patmatch "  " `(:one-or-more whitep))