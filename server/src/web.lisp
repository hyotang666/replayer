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

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html" *template-directory*))
