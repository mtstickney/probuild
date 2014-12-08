;;;; probuild.asd

(defpackage #:app-config
  (:export #:*base-directory*
           #:*version*))

(defparameter app-config:*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defparameter app-config:*version* "0.0.4")

(asdf:defsystem #:probuild
  :serial t
  :description "Describe probuild here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :version #.app-config:*version*
  :depends-on (#:asdf #:cl-annot #:uiop #:cl-fad #:cl-nanomsg)
  :components ((:file "package")
               (:file "nanomsg")
               (:file "util")
               (:file "builder")
               (:file "probuild")))
