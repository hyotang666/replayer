; vim: ft=lisp et
(in-package :asdf)
(defsystem "replayer.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "replayer")
  :components
  ((:file "replayer"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :replayer args)))