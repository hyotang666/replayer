(defsystem "replayer-server-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("replayer-server"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "replayer-server"))))
  :description "Test system for replayer-server"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
