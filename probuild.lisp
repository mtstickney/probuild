;;;; probuild.lisp

(in-package #:probuild)

(annot:enable-annot-syntax)

(defclass abl-file (asdf:source-file)
  ())

@eval-always
(defclass procedure-file (abl-file)
  ((type :initform "p")))

@eval-always
(defclass window-file (abl-file)
  ((type :initform "w")))

@eval-always
(defclass class-file (abl-file)
  ((type :initform "cls")))

;; .st files
@eval-always
(defclass database-file (asdf:source-file)
  ((block-size :initarg :block-size :accessor block-size)
   (schema :initarg :schema :accessor db-schema)
   (data :initarg :data :accessor db-data))
  (:default-initargs
   :block-size 4 ;; 4kb block size, default in existing compass
    ;; install
    :schema nil
    :data nil))

@eval-always
(defclass abl-module (asdf:module)
  ((databases :initarg :databases :accessor databases)
   (inherit-databases :initarg :inherit-databases :accessor inherit-databases)
   (builder :accessor builder :initform nil))
  (:default-initargs
   :databases '()
    :inherit-databases t))

@eval-always
(defclass abl-system (asdf:system abl-module)
  ((progress-args :initarg :progress-args :accessor progress-args)
   (builder-class :initarg :builder-class :accessor builder-class)
   ;; Note: ASDF bypasses initialize-instance (it uses some
   ;; change-class magic), so :default-initargs won't work here.
   (databases :initarg :databases :accessor databases :initform '())
   (inherit-databases :initarg :inherit-databases :accessor inherit-databases :initform t)))

;; Allow the use of bare keyword class names in system defs
@eval-always
(setf (find-class 'asdf::procedure-file) (find-class 'procedure-file)
      (find-class 'asdf::window-file) (find-class 'window-file)
      (find-class 'asdf::class-file) (find-class 'class-file)
      (find-class 'asdf::database-file) (find-class 'database-file)
      (find-class 'asdf::abl-module) (find-class 'abl-module)
      (find-class 'asdf::abl-system) (find-class 'abl-system))

(defmethod asdf:output-files ((op asdf:compile-op) (component abl-file))
  (let ((output-file (merge-pathnames (make-pathname :type "r")
                                      (asdf:component-pathname component))))
    (list output-file)))

(defmethod asdf:output-files ((op asdf:compile-op) (component class-file))
  (let ((output-file (merge-pathnames (make-pathname :type "r")
                                      (asdf:component-pathname component))))
    (list output-file)))

(defmethod asdf:output-files ((op asdf:compile-op) (component database-file))
  (let ((output-file (merge-pathnames (make-pathname :type "db")
                                      (asdf:component-pathname component))))
    (list output-file)))

(defgeneric component-databases (op component)
  (:documentation "Return a list of database specifications in effect when OP is applied to COMPONENT."))

(defmethod component-databases (op (component (eql nil)))
  nil)

(defmethod component-databases (op component)
  ;; If the component doesn't potentially introduce a database, just
  ;; use the parent's list
  (let ((parent (asdf:component-parent component)))
    (component-databases op parent)))

(defmethod component-databases (op (component abl-module))
  (let ((local-dbs (databases component))
        (parent (asdf:component-parent component)))
    (if (not (inherit-databases component))
        local-dbs
        (append (component-databases op parent) local-dbs))))

(defmethod asdf:component-depends-on ((op asdf:compile-op) (component abl-module))
  (cons (list 'asdf:prepare-op)
        (mapcar (lambda (c) (list 'asdf:compile-op c))
                (asdf:component-children component))))

(defmethod asdf:component-depends-on ((op asdf:compile-op) (component abl-file))
  '())

(defun changes-dbs-p (component)
  (and (typep component 'abl-module)
       ;; If it has a non-NIL db list or dis-inherits dbs, it changes
       ;; the db set.
       (or (and (slot-boundp component 'databases)
                (databases component))
           (null (inherit-databases component)))))

(defun db-module (component)
  (check-type component asdf:component)
  (loop with c = component
     while (and (asdf:component-parent c)
                (not (changes-dbs-p c)))
     do (setf c (asdf:component-parent c))
     finally (return c)))


(defun db-connection-info (logical-name &rest opts &key singleuser (pathname (concatenate 'string logical-name ".db")) host port username password (alias nil aliasp))
  (cond
    ;; Can have the :alias keyword and one argument
    ((and aliasp (third opts))
     (error "Cannot specify other options for an :ALIAS db."))
    (aliasp (list :alias logical-name alias))
    (t (cons :db (append
                  (list "-db" (format nil "~A" pathname))
                  (list "-ld" (format nil "~A" logical-name))
                  (and singleuser (list "-1"))
                  (and host (list "-H" (format nil "~A" host)))
                  (and port (list "-S" (format nil "~A" port)))
                  (and username (list "-U" (format nil "~A" username)))
                  (and password (list "-P" (format nil "~A" password))))))))

(defun output-directory ()
  (asdf:apply-output-translations #P"."))

;; Component building protocol
(defgeneric get-builder (component builder-class)
  (:method (component (builder-class symbol))
    (get-builder component (find-class builder-class))))

(defun component-builder-args (component)
  (let* ((dbs (component-databases 'asdf:compile-op component))
         (connect-args (apply #'append (mapcar (lambda (db-spec)
                                                 (cdr (apply #'db-connection-info db-spec)))
                                               dbs))))
    (append (progress-args (asdf:component-system component))
            connect-args)))

;; Exec-builder stuff
(defmethod get-builder ((component abl-file) (builder-class (eql (find-class 'exec-builder))))
  (make-instance builder-class :pro-args (component-builder-args component)))


;; Server-builder stuff
(defmethod get-builder ((component abl-file) (builder-class (eql (find-class 'server-builder))))
  (let* ((db-module (db-module component))
         (builder (or (builder db-module)
                      (make-instance 'server-builder
                                     :pro-args (component-builder-args component)))))
    ;; Remember, it returns the value
    (setf (builder db-module) builder)))

(defmethod asdf:perform ((op asdf:compile-op) (component abl-file))
  (let* ((builder-class (builder-class (asdf:component-system component)))
         (builder (get-builder component builder-class))
         (code-dir (asdf:component-pathname (asdf:component-system component)))
         (output-file (first (asdf:output-files 'asdf:compile-op component))))
    (build-file builder code-dir (asdf:component-pathname component)
                output-file
                :save-into (if (typep component 'class-file)
                               ;; COMPILE does funny things with
                               ;; SAVE-INTO for classes.
                               (output-directory)
                               (cl-fad:pathname-directory-pathname output-file)))))

;; After all the children have been compiled, shut down the builder if
;; necessary.
(defmethod asdf:perform :after ((op asdf:compile-op) (component abl-module))
  (let ((system (asdf:component-system component))
        (builder (builder component)))
    (when (and (eq (builder-class system) 'server-builder)
               builder)
      (shutdown-server builder)
      (setf (builder component) nil))))

(defun set-output-dir (path)
  (check-type path pathname)
  (asdf:clear-output-translations)
  (asdf:initialize-output-translations
   `(:output-translations (,(merge-pathnames #P"**/*.*")
                            ,(merge-pathnames (merge-pathnames #P"**/*.*" path)))
                          :ignore-inherited-configuration
                          :disable-cache)))

;;;; Main application stuff
(defmethod asdf:perform :around ((op asdf:compile-op) (component abl-file))
  (declare (special *print-status*))
  (let ((printp (and (boundp '*print-status*) *print-status*))
        (system (asdf:component-system component)))
    (when printp
      (format t "~&Compiling ~A..." (enough-namestring (asdf:component-pathname component)
                                                       (asdf:component-pathname system)))
      (finish-output *standard-output*))
    (call-next-method op component)
    (when printp
      (format t "done~%"))))

(define-condition invalid-args (error)
  ((reason :initarg :reason :accessor reason))
  (:report (lambda (c s)
             (format s "Invalid arguments: ~A" (reason c)))))
(define-condition invalid-option (error)
  ((opt :initarg :opt :accessor option))
  (:report (lambda (c s)
             (format s "'~A' is not a valid option." (option c)))))

(defun process-argument (opt val)
  (cond
    ((equalp opt "--output-dir")
     (set-output-dir (cl-fad:pathname-as-directory val)))
    (t (error 'invalid-option :opt opt))))

(defun process-args (args)
  (when (null (second args))
    (error 'invalid-args :reason "at least two arguments are required."))
  (let ((opts (nthcdr 2 args)))
    (loop for (opt . rest) on opts by #'cddr
       when (null rest)
       do (error 'invalid-args :reason (format nil "option '~A' is missing a value." opt))
       do (process-argument opt (car rest)))
    (list (first args) (second args))))

(defun print-usage (&optional (stream *standard-output*))
  (format stream "This is doozer v~A~%~
Usage: doozer <operation> <system> [--output-dir output]~%~%~
~4T<operation> -- currently just 'compile'.
~4T<system> -- the name of the system to operate on.
~4Toutput -- the directory to use for output files."
          app-config:*version*))

(defun get-asdf-op (op)
  (check-type op string)
  (cond
    ((equalp op "compile")
     'asdf:compile-op)
    (t (error "Unknown system operation ~S." op))))

(defun system-present-p (system)
  (handler-case
      (asdf:find-system system)
    (asdf:missing-component ()
      (return-from system-present-p nil))))

(defun shutdown-builders (system)
  (check-type system asdf:system)
  (labels ((shutdown-component (c)
             (when (and (typep c 'abl-module)
                        (builder c))
               (shutdown-server (builder c)))
             (when (typep c 'asdf:module)
               (loop for child in (asdf:component-children c)
                  do (shutdown-component child)))))
    (shutdown-component system)))

(defun dbg (msg &rest args)
  (format *debug-io* "~A~%" (apply #'format nil msg args))
  (finish-output *debug-io*))

(defun quietly-oos (op system)
  (let ((*load-verbose* nil)
        (*compile-verbose* nil)
        (*load-print* nil)
        (*compile-print* nil))
    (handler-bind ((warning #'muffle-warning))
      (asdf:oos op system))))

(defun run-app (argv)
  (let* (;; This is SBCL, so *d-p-d* will be the current directory
         ;; (also (truename ".") has a bug)
         ;; (*default-pathname-defaults* (truename ".")
         (asdf:*central-registry* (cons *default-pathname-defaults* asdf:*central-registry*))
         (*print-status* t)
         (app-config:*base-directory* (cl-fad:pathname-directory-pathname (cl-fad:pathname-as-file (first argv))))
         ;; ASDF sometimes prints compilation info to
         ;; *error-output*, so swallow it
         (*error-output* (make-broadcast-stream))
         op
         system)
    (declare (special *print-status*))

    ;; Add the binary's directory to PATH so progress can load the
    ;; nanomsg dll.
    (let ((path (sb-posix:getenv "PATH"))
          (binary-path (cl-fad:pathname-as-file (first argv))))
      (sb-posix:putenv (format nil "PATH=~A;~A"
                               (uiop:native-namestring (cl-fad:pathname-directory-pathname binary-path))
                               path)))

    ;; set a default output directory (will be overridden by
    ;; process-args if --output-dir was supplied)
    (set-output-dir *default-pathname-defaults*)
    (handler-case
        (let ((args (process-args (rest argv))))
          (setf op (first args)
                system (second args)))
      ((or invalid-args invalid-option) (c)
        (format t "~A~%" c)
       (print-usage)
       (return-from run-app -1)))

    (unless (system-present-p system)
      (error "Couldn't find the system ~S (are you sure there's an ~A.asd file in this directory?)"
             system
             system))
    (let ((system (asdf:find-system system))
          (asdf-op (get-asdf-op op)))
      (unwind-protect
           (quietly-oos asdf-op system)
        (shutdown-builders system)))
    ;; Success error code
    0))

(defun main (argv)
  (handler-case
      (let ((code (run-app argv)))
        (sb-ext:exit :code code))
    (serious-condition (c)
      ;; It's not paranoia if they're actually out to get you.
      (handler-case
          (progn (format t "~&Error: ~A" c)
                 (sb-ext:exit :code -1))
        (t ()
          (sb-ext:exit :code -1))))))
