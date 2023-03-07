(asdf:defsystem :cheap-patmatch-test
  :name "Tests for Cheap Pattern Matcher"
  :author "Shannon Spires <ssos@bearlanding.com>"
  :version "0.1"
  :maintainer "Shannon Spires <ssos@bearlanding.com>"
  :licence "BSD 3-clause"
    :depends-on ("cheap-patmatch"
                 "lisp-unit")
  :serial t
  :components ((:module tests
                        :pathname "test/"
                        :components ((:file "cheap-patmatch-test")))))