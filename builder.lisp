(in-package #:probuild)

(defvar *progress-cmd* #P"C:\\Progress\\OpenEdge\\bin\\prowin32.exe")

(defun delimited-list (args &optional (char #\,))
  (check-type args list)
  (flet ((check-arg (arg)
           (let ((strarg (format nil "~A" arg)))
             (when (find char strarg :test #'eql)
               (error "Procedure parameter ~S contains a delimiter character '~C'" arg char))
             strarg)))
    (format nil "~{~}"
            (if (eql char #\~)
                "~A~^~~"
                (format nil "~~A~~^~C" char))
            (mapcar #'check-arg args))))

(defun run-abl-procedure (proc args &key progress-args input (output :string))
  (check-type proc string)
  (let ((args (append (list "-p" proc
                            "-param" (delimited-list args))
                      progress-args)))
    (uiop:run-program (cons *progress-cmd* args) :output output :input input)))

(defun pipe-subprocess (command &rest keys)
  (apply #'sb-ext:run-program (car command) (cdr command) :input :stream :wait nil keys))

(defun pipe-abl-procedure (proc args progress-args)
  (let ((args (append (list "-p" proc "-param" (delimited-list args))
                      progress-args)))
    (pipe-subprocess (cons *progress-cmd* args))))

(define-condition build-error (error) ())

(define-condition build-failure (build-error)
  ((source-file :initarg :source :accessor source-file)
   (output-file :initarg :output :accessor output-file)
   (msgs :initarg :msgs :accessor compiler-output))
  (:default-initargs
   :msgs "")
  (:report (lambda (c s)
             (format s "Output file ~A does not exist, build of ~A failed.~&Compiler messages:~%~A"
                     (output-file c)
                     (source-file c)
                     (compiler-output c)))))

(define-condition missing-source (build-error)
  ((source-file :initarg :source :accessor source-file))
  (:report (lambda (c s)
             (format s "Source file ~A does not exist." (source-file c)))))

(defclass base-builder () ())

(defgeneric build-file (builder base-dir source-file output-file &key save-into))

(defclass exec-builder ()
  ((args :initarg :pro-args :accessor progress-args))
  (:default-initargs :pro-args '()))

(defmethod build-file ((builder exec-builder) base-dir source-file output-file &key (save-into (cl-fad:pathname-directory-pathname output-file)))
  (check-type base-dir (or pathname string))
  (check-type source-file (or pathname string))
  (check-type output-file (or pathname string))
  (let* ((source-file (cl-fad:pathname-as-file source-file))
         (base-dir (cl-fad:pathname-as-directory base-dir))
         output)
    (unless (probe-file source-file)
      (error 'missing-source :source source-file))
    (ensure-directories-exist save-into)
    (setf output (run-abl-procedure (namestring (merge-pathnames #P"compile-file.p" app-config:*base-directory*))
                                    (list base-dir source-file save-into)
                                    :progress-args (progress-args builder)))
    (unless (probe-file output-file)
      (error 'build-failure :source source-file :output output-file :msgs (or output "")))
    (values)))

(define-condition build-server-exited (error) ())

(defclass server-builder ()
  ((proc :accessor server-proc :initform nil)
   (socket :accessor server-socket :initform nil)
   (eid :accessor server-eid :initform nil)
   (id :accessor server-id :initarg :id)
   (args :initarg :pro-args :accessor progress-args))
  ;; NOTE: a random name isn't as good as e.g. mkstemp, but on windows
  ;; you can't get a uniqueness guarantee out of GetTempFileName()
  ;; without actually creating the file, which won't work with our
  ;; path.
  (:default-initargs :id (format nil "BUILDSERVER~A" (random 100))
    :pro-args '()))

(defgeneric startedp (builder)
  (:method ((builder server-builder))
    (let ((proc (server-proc builder)))
      (and proc (sb-ext:process-alive-p proc)))))

;; TODO: Use the windows JOB thingy to make sure spawned subprocesses
;; get closed when the main application closes.
(defgeneric start-server (builder)
  (:method ((builder server-builder))
    (unless (startedp builder)
      (let ((message-url (format nil "ipc:///tmp/probuild/~A" (server-id builder)))
            proc
            socket
            eid
            success)
        (unwind-protect
             (progn
               (setf socket (nn:socket nn:+af-sp+ nn:+nn-pair+))
               (when (< socket 0)
                 (error "Failed to create socket for server."))
               (setf eid (nn:connect socket message-url))
               (when (< eid 0)
                 (error "Failed to connect socket ~S to endpoint ~S." socket eid))
               (setf proc (pipe-abl-procedure (namestring (merge-pathnames #P"build-server.p"
                                                                           app-config:*base-directory*))
                                              (list message-url)
                                              (progress-args builder)))
               (setf success t))
          (unless success
            ;; Workaround for race condition on
            ;; nn_close()/nn_shutdown()
            (when socket
              (sleep .100)
              (when eid
                (nn:shutdown socket eid))
              (nn:close socket))))

        ;; Now that we have our elements, store them in the class
        (setf (server-proc builder) proc
              (server-socket builder) socket
              (server-eid builder) eid)
        (values)))))

(defun get-response-output (socket proc &key (poll-seconds 1))
  (let ((original-rcv-timeout (rcv-timeout socket)))
    (setf (rcv-timeout socket) (* poll-seconds 1000))
    (unwind-protect
         (loop for msg = (rcv-or-timeout socket)
            for timeoutp = (null msg)
            while (and timeoutp (sb-ext:process-alive-p proc))
            finally (if timeoutp
                        ;; Crashed, report the condition (may get
                        ;; restarted)
                        (error 'build-server-exited)
                        (return-from get-response-output msg)))
      (setf (rcv-timeout socket) original-rcv-timeout))))

(defgeneric submit-job (builder data)
  (:method ((builder server-builder) data)
    (check-type data string)
    (let ((bytes (babel:string-to-octets data))
          (socket (server-socket builder))
          (proc (server-proc builder)))
      (send-ubv socket bytes)
      (babel:octets-to-string (get-response-output socket proc)))))

(defgeneric shutdown-server (builder)
  (:method ((builder server-builder))
    (when (startedp builder)
      ;; An empty job signals shutdown
      (send-ubv (server-socket builder)
                (make-sequence '(vector (unsigned-byte 8)) 0)))

    ;; Shutdown the message queue
    (when (server-socket builder)
      (sleep .100) ; workaround for race condition in nn_shutdown()/nn_close()
      (when (server-eid builder)
        (nn:shutdown (server-socket builder) (server-eid builder)))
      (nn:close (server-socket builder)))

    ;; Close the now defunct process
    (when (server-proc builder)
      (sb-ext:process-close (server-proc builder)))

    ;; Set all the elements back to nil
    (setf (server-proc builder) nil
          (server-socket builder) nil
          (server-eid builder) nil)))

;; FIXME: output-file is a lie!
(defmethod build-file ((builder server-builder) base-dir source-file output-file &key (save-into (cl-fad:pathname-directory-pathname output-file)))
  (check-type base-dir (or string pathname))
  (check-type source-file (or string pathname))
  (check-type output-file (or string pathname))
  (let ((base-dir (cl-fad:pathname-as-directory base-dir))
        (source-file (cl-fad:pathname-as-file source-file))
        (output-file (cl-fad:pathname-as-file output-file))
        output)
    (unless (startedp builder)
      (start-server builder))
    (unless (probe-file source-file)
      (error 'missing-source :source source-file))
    (setf output (submit-job builder (delimited-list (list base-dir source-file save-into))))
    (when (or (not (probe-file output-file))
              (> (length output) 0))
      (error 'build-failure :source source-file :output output-file :msgs (or output "")))
    (values)))
