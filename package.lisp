;;;; package.lisp

(defpackage #:probuild
  (:use #:cl #:annot.eval-when)
  ;; ASDF components
  (:export #:abl-system
           #:abl-file
           #:procedure-file
           #:class-file
           #:window-file
           #:abl-module
           #:abl-system
           #:dist-op
           #:clean-op
           #:make-clean-op)

  ;; ASDF extension protocol
  (:export #:component-databases
           #:get-builder)

  ;; Builder functions and protocols
  (:export #:base-builder
           #:server-builder
           #:exec-builder
           ;; Errors
           #:build-error
           #:build-failure
           #:missing-source
           #:build-server-exited
           ;; The protocol
           #:build-file
           #:startedp
           #:start-server
           #:submit-job
           #:shutdown-server
           #:*progress-cmd*)

  ;; Utilities
  (:export #:output-directory
           #:set-output-dir)

  ;; Application bits
  (:export #:invalid-args
           #:invalid-option
           #:main))
