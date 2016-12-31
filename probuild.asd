;;;; probuild.asd

(defpackage #:app-config
  (:export #:*base-directory*
           #:*version*))

(defparameter app-config:*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defparameter app-config:*version* "0.0.9")

(asdf:defsystem #:probuild
  :serial t
  :description "Describe probuild here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :version #.app-config:*version*
  :depends-on (#:asdf
               #:closer-mop
               #:cl-annot
               #:uiop
               #:cl-fad
               #:cl-nanomsg
               #:trivial-download
               #:quri
               #:split-sequence
               #:cl-date-time-parser
               #:cffi)
  :components ((:file "package")
               (:file "nanomsg")
               (:file "util")
               (:file "builder")
               #+(or windows mswindows win32)
               (:file "program-file-win32")
               (:file "probuild")))
