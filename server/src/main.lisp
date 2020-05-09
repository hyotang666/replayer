(in-package :cl-user)

(defpackage replayer-server
  (:use :cl)
  (:import-from :replayer-server.config :config)
  (:import-from :clack :clackup)
  (:export :start :stop))

(in-package :replayer-server)

(defvar *appfile-path*
  (asdf:system-relative-pathname :replayer-server #P"app.lisp"))

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (when (mixalot:mixer-shutdown-flag replayer:*mixer*)
    (setf replayer:*mixer* (replayer:create-player)))
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server () :report "Restart the server" (stop))))
  (setf *handler* (apply #'clackup *appfile-path* args)))

(defun stop () (prog1 (clack:stop *handler*) (setf *handler* nil)))