(in-package #:probuild)

;; Note: this won't handle .. properly (sbcl translated .. as :UP,
;; which CANONICAL-PATHNAME can't handle).
(defun relative-path (path base)
  (check-type path pathname)
  (check-type base pathname)
  (flet ((compare-components (path defaults)
           (loop for c1 in path
              for c2 in defaults
              when (not (#+(or win32 mswindows windows) equalp
                         #-(or win32 mswindows windows) equal
                         c1
                         c2))
              return nil
              finally (return t))))
    (let ((path (cl-fad:canonical-pathname path))
          (base (cl-fad:canonical-pathname base)))
      (when (or (cl-fad:pathname-relative-p path)
                (not (equalp (pathname-device path)
                             (pathname-device base))))
        (return-from relative-path path))
      (let* ((path-directory (pathname-directory path))
             (base-directory (pathname-directory base))
             (prefix-len (length base-directory))
             (path-prefix (subseq path-directory
                                  0
                                  prefix-len)))
        (if (compare-components path-prefix base-directory)
            ;; We have a shared prefix, return a relative path
            (make-pathname :device nil
                           :directory (cons :relative (nthcdr prefix-len path-directory))
                           :defaults path)
            path)))))
