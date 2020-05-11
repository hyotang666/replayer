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

(defroute "/" () (render #P"index.html"))

(defroute ("/play/file" :method :post) (&key file)
  (handler-case (replayer:play file)
    (replayer:missing-file (c)
      `(404 #|Not Found|# nil (,(princ-to-string c))))
    (:no-error (&rest args)
      (declare (ignore args)) `(200 #|ok|# nil ("Done")))))

(defroute /play/push.post ("/play/push" :method :post)
  (&key file)
  (handler-case (truename file)
    (error (c)
      `(404 #|Not found|# nil (,(princ-to-string c))))
    (:no-error (pathname)
      (replayer:play (list pathname)) `(200 #|ok|# nil ("Pushed")))))

(defroute /play/tag.post ("/play/tag" :method :post)
  (&key tag)
  (handler-case (replayer:play (replayer:make-tag :exp tag))
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