;;; package.lisp
;;; for cheap-patmatch
;;; 06-Mar-2023 SVS

(defpackage :cheap-patmatch
  (:nicknames :cpat)
  (:use :cl)
  (:export
   #:patmatch
   #:ppatmatch
   #:whitep
   #:non-whitep
   #:any-char
   #:any-char-but))