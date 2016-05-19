(in-package #:probuild)

(defconstant +max-path+ 260)

(cffi:defcfun (get-module-file-name "GetModuleFileNameA" :convention :stdcall)
    :long
  (module :pointer)
  (filename-buf :pointer)
  (buf-len :long))

(defun program-file ()
  (cffi:with-foreign-object (buf :char (1+ +max-path+))
    (let ((res (get-module-file-name (cffi:null-pointer) buf (1+ +max-path+))))
      (when (= res 0)
        (error "Error getting program name."))
      (cffi:foreign-string-to-lisp buf :count res))))
