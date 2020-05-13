; vim: ft=lisp et
(in-package :asdf)
(defsystem "replayer"
  :description "Music player in REPL."
  :author "SATO Shinichi"
  :license "MIT"
  :version
  "3.7.0"
  :depends-on
  (
   "mixalot" ; Audio mixer.
   "mixalot-mp3" ; MP3 streamer.
   "wav-parser" ; Wav file reader/writer.
   "datafly" ; Database.
   "sxql" ; SQL generator.
   )
  :pathname
  "src/"
  :components
  ((:file "replayer")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "replayer").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "replayer"))))
  (append (call-next-method) '((test-op "replayer.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "replayer")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after ((o load-op) (c (eql (find-system "replayer"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (symbol-call :jingoh.documentizer :import c)))))
