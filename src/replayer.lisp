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

(defun q-random-pop (&optional index)
  (labels ((entry (index list)
             (if (zerop index)
                 (q-pop)
                 (body index list)))
           (body (index list)
             (if (zerop (1- index))
                 (if (cddr list) ; (car . (cdr . (cddr ...)))
                     (prog1 (cadr list) (rplacd list (cddr list)))
                     ;; (car . (cdr . nil))
                     (prog1 (cadr list)
                       (rplacd list nil)
                       (rplaca *queue* list)))
                 (body (1- index) (cdr list)))))
    (if index
        (entry index (cdr *queue*))
        (let ((length (length (cdr *queue*))))
          (if (zerop length)
              nil
              (entry (random length) (cdr *queue*)))))))

(defun q-append (items) (mapc #'q-push items) *queue*)

(defun have-item-p () (car *queue*))

(defun q-clear () (setf *queue* (cons nil nil)))

;;;; PLAYER-MIXER

(defstruct (player (:include mixalot:mixer)))

(defvar *play* nil)

(declaim (type (member :one :all nil) *repeat*))

(defvar *repeat* nil)

(defvar *shuffle* nil)

(defvar *played* nil)

(defmethod mixalot:mixer-remove-streamer :after ((player player) streamer)
  (setq *play* nil)
  (ecase *repeat*
    (:one
     (mixalot:streamer-seek streamer player 0)
     (mixalot:mixer-add-streamer player streamer))
    (:all
     (mixalot:streamer-seek streamer player 0)
     (push streamer *played*)
     (play
       (if *shuffle*
           (q-random-pop)
           (q-pop))))
    ((nil)
     (play
       (if *shuffle*
           (q-random-pop)
           (q-pop))))))

;;;; SPECIALS

(defvar *mixer* (mixalot:create-mixer :constructor 'make-player))

;;;; PLAY

(defgeneric play
    (thing))

(defmethod play ((thing null))
  (when *shuffle*
    (if (eq :all *repeat*)
        (let ((list *played*))
          (setq *played* nil)
          (play list))
        (setq *played* nil))))

(defmethod play ((string string)) (play (pathname string)))

(defmethod play ((pathname pathname))
  (handler-case (truename pathname)
    (error (c)
      (warn (princ-to-string c)))
    (:no-error (pathname)
      (if (uiop:directory-pathname-p pathname)
          (warn "Ignore directory pathname ~S" pathname)
          (play (wav-parser:wav pathname))))))

(defmethod play ((wav r-iff:group))
  (setf *play*
          (mixalot:mixer-add-streamer *mixer*
                                      (mixalot:make-fast-vector-streamer-interleaved-stereo
                                        (r-iff:data<-chunk
                                          (car (r-iff:retrieve "data" wav)))))))

(defmethod play ((list list))
  (q-append list)
  (play
    (if *shuffle*
        (q-random-pop)
        (q-pop))))

(defmethod play ((streamer mixalot:vector-streamer))
  (setf *play* (mixalot:mixer-add-streamer *mixer* streamer)))

;;;; SKIP

(defun skip ()
  (when *play*
    (mixalot:mixer-remove-streamer *mixer* *play*)))

;;;; STOP

(defun stop ()
  (when *play*
    (let ((*repeat* nil))
      (setq *played* nil)
      (q-clear)
      (skip))))

;;;; Database

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-db (&body body)
    `(let ((datafly:*connection*
            (datafly:connect-cached :sqlite3
                                    :database-name (merge-pathnames "db/db"
                                                                    (asdf:system-source-directory
                                                                      (asdf:find-system
                                                                        :replayer))))))
       ,@body)))

(with-db
  (datafly:execute
    (sxql:create-table (file :if-not-exists t)
      ((pathname :type 'tinytext :not-null t :primary-key t))))
  (datafly:execute
    (sxql:create-table (tag :if-not-exists t)
      ((name :type 'tinytext :not-null t :primary-key t))))
  (datafly:execute
    (sxql:create-table (tag-map :if-not-exists t)
      ((tag :type 'integer)
       (file :type 'tinytext))
      (sxql:foreign-key '(tag) :references '(tag name))
      (sxql:foreign-key '(file) :references '(file pathname)))))

(defun delete-tables ()
  (with-db
    (dolist (table '(file tag tag-map))
      (datafly:execute (sxql:drop-table table :if-exists t)))))

(defun tag (tag files)
  (with-db
    (let ((tag?
           (datafly:retrieve-one
             (sxql:select :*
               (sxql:from 'tag)
               (sxql:where (:= 'name tag))))))
      (unless tag?
        (datafly:execute
          (sxql:insert-into :tag
            (sxql:set= :name tag))))
      (dolist (file (mapcar #'namestring files))
        (let ((file?
               (datafly:retrieve-one
                 (sxql:select :*
                   (sxql:from 'file)
                   (sxql:where (:= 'pathname file))))))
          (unless file?
            (datafly:execute
              (sxql:insert-into :file
                (sxql:set= :pathname file))))
          (let ((tag-map
                 (datafly:retrieve-one
                   (sxql:select :*
                     (sxql:from 'tag-map)
                     (sxql:where
                      (:and (:= 'tag tag)
                            (:= 'file file)))
                     (sxql:limit 1)))))
            (unless tag-map
              (datafly:execute
                (sxql:insert-into :tag-map
                  (sxql:set= :tag tag :file file))))))))))

(defun tag-files (tag)
  (with-db
    (mapcar (lambda (elt) (getf elt :file))
            (datafly:retrieve-all
              (sxql:select :tag-map.file
                (sxql:from 'tag-map)
                (sxql:where (:= 'tag tag)))))))

(defun file-tags (file)
  (with-db
    (mapcar (lambda (elt) (getf elt :tag))
            (datafly:retrieve-all
              (sxql:select :tag-map.tag
                (sxql:from 'tag-map)
                (sxql:where (:= 'file (namestring file))))))))
