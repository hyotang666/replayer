(in-package :cl-user)

(defpackage :replayer
  (:use :cl)
  (:export ;; Special vars.
           #:*mixer*
           #:*repeat*
           #:*shuffle*
           ;; Conditions
           #:missing-file
           ;; player.
           #:create-player
           ;; Main APIs.
           #:play
           #:stop
           #:skip
           ;; Tag operations
           #:make-tag
           #:tag-files
           #:file-tags
           #:tag))

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

(defun create-player () (mixalot:create-mixer :constructor 'make-player))

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

(defvar *mixer* (create-player))

;;;; CONDITIONS

(define-condition missing-file (warning) ((file :initarg :file))
  (:report
   (lambda (condition stream)
     (format stream "Missing file. ~S" (slot-value condition 'file)))))

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
    (error ()
      (warn 'missing-file :file pathname))
    (:no-error (pathname)
      (if (uiop:directory-pathname-p pathname)
          (warn "Ignore directory pathname ~S" pathname)
          (let ((type (pathname-type pathname)))
            (cond ((string= "wav" type) (play (wav-parser:wav pathname)))
                  ((string= "mp3" type)
                   (play (mixalot-mp3:make-mp3-streamer (namestring pathname))))
                  (t (warn "NIY file type ~S" type))))))))

(defmethod play ((mp3 mixalot-mp3:mp3-streamer))
  (setf *play* (mixalot:mixer-add-streamer *mixer* mp3)))

(defmethod play ((wav r-iff:group))
  (setf *play*
          (mixalot:mixer-add-streamer *mixer*
                                      (mixalot:make-fast-vector-streamer-interleaved-stereo
                                        (r-iff:data<-chunk
                                          (car (r-iff:retrieve "data" wav)))))))

(defmethod play ((list list))
  (q-append list)
  (unless *play*
    (play
      (if *shuffle*
          (q-random-pop)
          (q-pop)))))

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
                                    :database-name (ensure-directories-exist
                                                     (merge-pathnames "db/db"
                                                                      (asdf:system-source-directory
                                                                        (asdf:find-system
                                                                          :replayer)))))))
       ,@body)))

(defun list-all-tags ()
  (with-db
    (datafly:retrieve-all
      (sxql:select :*
        (sxql:from 'tag)))))

(defun list-all-tag-maps ()
  (with-db
    (datafly:retrieve-all
      (sxql:select :*
        (sxql:from 'tag-map)))))

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
      (dolist (file (mapcar (lambda (exp) (namestring (truename exp))) files))
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

(defvar *not* nil)

(defun tag-files (tag)
  (if (atom tag)
      (with-db
        (mapcar (lambda (elt) (getf elt :file))
                (datafly:retrieve-all
                  (sxql:select :tag-map.file
                    (sxql:from 'tag-map)
                    (sxql:where
                     `(,(if *not*
                            ':!=
                            ':=)
                       tag ,(princ-to-string tag)))))))
      (ecase (car tag)
        (and
         (reduce (lambda (a b) (intersection a b :test #'equal))
                 (mapcar #'tag-files (cdr tag))))
        (or
         (reduce (lambda (a b) (union a b :test #'equal))
                 (mapcar #'tag-files (cdr tag))))
        (not
         (let ((*not* t))
           (assert (typep tag '(cons (eql not) (cons * null))))
           (tag-files (cadr tag)))))))

(defun file-tags (file)
  (with-db
    (mapcar (lambda (elt) (getf elt :tag))
            (datafly:retrieve-all
              (sxql:select :tag-map.tag
                (sxql:from 'tag-map)
                (sxql:where (:= 'file (namestring file))))))))

(defun delete-tag (tag)
  (setf tag (princ-to-string tag))
  (with-db
    (let ((tag-maps
           (datafly:execute
             (sxql:select :*
               (sxql:from 'tag-map)
               (sxql:where (:= 'tag tag))))))
      (datafly:execute
        (sxql:delete-from :tag
          (sxql:where (:= 'name tag))))
      (datafly:execute
        (sxql:delete-from :tag-map
          (sxql:where (:= 'tag tag))))
      (dolist (map tag-maps)
        (unless (datafly:execute
                  (sxql:select :*
                    (sxql:from 'tag-map)
                    (sxql:where (:= 'file (getf map :file)))))
          (datafly:execute
            (sxql:delete-from :file
              (sxql:where (:= 'pathname (getf map :file))))))))))

;;;; TAG

(defstruct tag exp)

(defmethod play ((tag tag))
  (let ((files (tag-files (tag-exp tag))))
    (if files
        (play files)
        (warn "Not exists tag ~S" tag))))
