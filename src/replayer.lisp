(in-package :cl-user)

(defpackage :replayer
  (:use :cl)
  (:export))

(in-package :replayer)

;;;; SPECIALS

(defvar *mixer* (mixalot:create-mixer))

;;;; PLAY

(defgeneric play
    (thing))

(defmethod play ((string string)) (play (truename string)))

(defmethod play ((pathname pathname))
  (setf pathname (truename pathname))
  (play (wav-parser:wav pathname)))

(defmethod play ((wav r-iff:group))
  (mixalot:mixer-add-streamer *mixer*
                              (mixalot:make-fast-vector-streamer-interleaved-stereo
                                (r-iff:data<-chunk
                                  (car (r-iff:retrieve "data" wav))))))
