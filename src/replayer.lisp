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

(defmethod mixalot:mixer-remove-streamer :after ((player player) streamer)
  (play (q-pop)))

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
  (mixalot:mixer-add-streamer *mixer*
                              (mixalot:make-fast-vector-streamer-interleaved-stereo
                                (r-iff:data<-chunk
                                  (car (r-iff:retrieve "data" wav))))))

(defmethod play ((list list))
  (if (have-item-p)
      (q-append list)
      (progn (q-append (cdr list)) (play (car list)))))
