(in-package :cl-user)

(defpackage replayer-server.web
  (:use :cl
        :caveman2
        :replayer-server.config
        :replayer-server.view
        :replayer-server.db
        :datafly
        :sxql)
  (:export :*web*))

(in-package :replayer-server.web)

;; for @route annotation

(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())

(defvar *web* (make-instance '<web>))

(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"
          `(:repeat
            ,(ecase replayer:*repeat*
               ((:one :all) replayer:*repeat*)
               ((nil) "false")))))

(defroute ("/play/file" :method :post) (&key file)
  (if (atom file)
      (handler-case (replayer:play file)
        (replayer:missing-file (c)
          `(404 #|Not Found|# nil (,(princ-to-string c))))
        (:no-error (&rest args)
          (declare (ignore args)) `(200 #|ok|# nil ("Done"))))
      ;; Otherwise form data is binary file with multipart/form-data.
      (destructuring-bind
          (stream filename mime-type)
          file
        (cond
         ((equal "audio/x-wav" mime-type)
          (replayer:play (wav-parser:wav stream)) `(200 #|ok|# nil ("Done")))
         ((equal "audio/mpeg" mime-type)
          (let* ((pathname
                  (ensure-directories-exist
                    (merge-pathnames (format nil "audio/~A" filename)
                                     *application-root*)))
                 (exist? (probe-file pathname)))
            (if exist?
                (progn (replayer:play pathname) `(200 #|ok|# nil ("Done")))
                (uiop:with-current-directory ((uiop:pathname-directory-pathname
                                                pathname))
                  (with-open-file (out pathname :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-does-not-exist :create
                                   :if-exists :error)
                    (uiop:copy-stream-to-stream stream out
                                                :element-type '(unsigned-byte
                                                                8)
                                                :buffer-size #x1000))
                  (replayer:play pathname)
                  `(200 #|ok|# nil ("Done"))))))
         (t
          `(400 #|Bad request|# nil
            (,(format nil "~S ~S ~S" stream filename mime-type))))))))

(defroute "/stop" () (replayer:stop) `(200 #|ok|# nil ("Stop")))

(defroute "/toggle/shuffle" ()
  (setf replayer:*shuffle* (not replayer:*shuffle*))
  `(200 #|ok|# nil ("Done")))

(defroute "/toggle/pause" ()
  (ecase (replayer:pause)
    (:paused `(200 #|ok|# nil ("Paused")))
    ((t) `(200 #|ok|# nil ("Unpause")))))

(defroute /repeat.post ("/repeat" :method :post)
  (&key mode)
  (cond
   ((equal "one" mode) (setq replayer:*repeat* :one)
    `(200 #|ok|# nil ("repeat=one")))
   ((equal "all" mode) (setq replayer:*repeat* :all)
    `(200 #|ok|# nil ("repeat=all")))
   ((equal "false" mode) (setq replayer:*repeat* nil)
    `(200 #|ok|# nil ("repeat=false")))
   (t
    `(400 #|Bad request|# nil
      (,(format nil "Unknown repeat mode: ~S~%" mode))))))

(defroute /play/push.post ("/play/push" :method :post)
  (&key file)
  (handler-case (truename file)
    (error (c)
      `(404 #|Not found|# nil (,(princ-to-string c))))
    (:no-error (pathname)
      (replayer:play (list pathname)) `(200 #|ok|# nil ("Pushed")))))

(defroute /play/tag.post ("/play/tag" :method :post)
  (&key tag)
  (handler-case
      (replayer:play
        (replayer:make-tag :exp (let ((*read-eval*)
                                      (*readtable* (copy-readtable nil)))
                                  (setf (readtable-case *readtable*) :preserve)
                                  (read-from-string tag))))
    (error (c)
      `(400 #|Bad request|# nil (,(princ-to-string c))))
    (:no-error (&rest args)
      (declare (ignore args)) `(200 #|ok|# nil ("Done")))))

(defroute /tag/file ("/tag/file" :method :post)
  (&key tag file)
  (handler-case (truename file)
    (error (c)
      `(404 #|Not found|# nil (,(princ-to-string c))))
    (:no-error (pathname)
      (replayer:tag tag (list pathname)) `(200 #|ok|# nil ("Done")))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html" *template-directory*))
