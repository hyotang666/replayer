(in-package :cl-user)

(defpackage :replayer
  (:use :cl)
  (:export))

(in-package :replayer)

;;;; QUEUE

(defvar *queue* '(nil . nil))

(defun q-push (item)
  (if (null (car *queue*))
      (setf (cdr *queue*) (setf (car *queue*) (list item)))
      (setf (cdar *queue*) (setf (car *queue*) (list item))))
  *queue*)

(defun q-pop ()
  (if (eq (car *queue*) (cdr *queue*)) ; Last one.
      (prog1 (cadr *queue*) (setf *queue* (cons nil nil)))
      (prog1 (cadr *queue*) (setf (cdr *queue*) (cddr *queue*)))))

(defun q-append (items) (mapc #'q-push items) *queue*)

(defun have-item-p () (car *queue*))

(defun q-clear () (setf *queue* (cons nil nil)))

;;;; PLAYER-MIXER

(defstruct (player (:include mixalot:mixer)))

(defvar *play* nil)

(declaim (type (member :one :all nil) *repeat*))

(defparameter *repeat* nil)

(defmethod mixalot:mixer-remove-streamer :after ((player player) streamer)
  (setq *play* nil)
  (ecase *repeat*
    (:one
     (mixalot:streamer-seek streamer player 0)
     (mixalot:mixer-add-streamer player streamer))
    (:all
     (mixalot:streamer-seek streamer player 0)
     (q-push streamer)
     (play (q-pop)))
    ((nil) (play (q-pop)))))

;;;; SPECIALS

(defvar *mixer* (mixalot:create-mixer :constructor 'make-player))

;;;; PLAY

(defgeneric play
    (thing))

(defmethod play ((do-nothing null)) do-nothing)

(defmethod play ((string string)) (play (truename string)))

(defmethod play ((pathname pathname))
  (setf pathname (truename pathname))
  (play (wav-parser:wav pathname)))

(defmethod play ((wav r-iff:group))
  (setf *play*
          (mixalot:mixer-add-streamer *mixer*
                                      (mixalot:make-fast-vector-streamer-interleaved-stereo
                                        (r-iff:data<-chunk
                                          (car (r-iff:retrieve "data" wav)))))))

(defmethod play ((list list))
  (if (have-item-p)
      (q-append list)
      (progn (q-append (cdr list)) (play (car list)))))

(defmethod play ((streamer mixalot:vector-streamer))
  (setf *play* (mixalot:mixer-add-streamer *mixer* streamer)))

;;;; SKIP

(defun skip ()
  (when *play*
    (mixalot:mixer-remove-streamer *mixer* *play*)))

;;;; STOP

(defun stop ()
  (when *play*
    (setq *repeat* nil)
    (q-clear)
    (skip)))
