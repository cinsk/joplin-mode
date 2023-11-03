;;; joplin-struct.el --- joplin data structure       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: convenience, data, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defvar joplin-error-buffer (get-buffer-create "*joplin-debug*"))

(cl-defstruct JFOLDER
  id title parent_id
  ;; these are not supported yet (Joplin 2.12.18)
  created_time updated_time user_created_time user_updated_time is-shared icon)

(cl-defstruct JNOTE
                                        ; slot names start with underscore is to keep the internal state.
  _readall                              ; nil if partially loaded
  _marked                               ; marked(selected) status
  _tags                                 ; list of tags?
  id
  parent_id
  title
  created_time
  updated_time
  is_conflict
  latitude
  longitude
  altitude
  author
  source_url
  is_todo
  todo_due
  todo_completed
  source
  source_application
  application_data
  order
  user_updated_time
  user_created_time
  encryption_cipher_text
  encryption_applied
  markup_language
  is_shared
  share_id
  conflict_original_id
  master_key_id
  user_data)

;; (cl-defstruct JNOTE id title parent_id updated_time markup_language created_time user_updated_time user_created_time source source_application order)

(defun build-JNOTE (src &optional complete)
  "return new JNOTE struct from the alist, SRC

If COMPLETE is boolean to mark JNOTE has all members"
  ;; Note 'body member is not included in JNOTE
  (let-alist src
    (let ((note (make-JNOTE :id .id
                            :parent_id .parent_id
                            :title .title
                            :created_time .created_time
                            :updated_time .updated_time
                            :is_conflict .is_conflict
                            :latitude .latitude
                            :longitude .longitude
                            :altitude .altitude
                            :author .author
                            :source_url .source_url
                            :is_todo .is_todo
                            :todo_due .todo_due
                            :todo_completed .todo_completed
                            :source .source
                            :source_application .source_application
                            :application_data .application_data
                            :order .order
                            :user_updated_time .user_updated_time
                            :user_created_time .user_created_time
                            :encryption_cipher_text .encryption_cipher_text
                            :encryption_applied .encryption_applied
                            :markup_language .markup_language
                            :is_shared .is_shared
                            :share_id .share_id
                            :conflict_original_id .conflict_original_id
                            :master_key_id .master_key_id
                            :user_data .user_data)))
      (setf (JNOTE-_readall note) complete)
      note)))

(cl-defstruct JRES
  _embeded
  _readall
  id
  title
  mime
  filename
  created_time
  updated_time
  user_updated_time
  user_created_time
  file_extension
  encryption_cipher_text
  encryption_applied
  encryption_blob_encrypted
  size
  is_shared
  share_id
  master_key_id
  user_data)

(defun build-JRES (src &optional complete)
  "return new JRES struct from the alist, SRC"
  (let-alist src
    (let ((res (make-JRES :id .id
                          :title .title
                          :mime .mime
                          :created_time .created_time
                          :updated_time .updated_time
                          :user_created_time .user_created_time
                          :user_updated_time .user_updated_time
                          :filename .filename
                          :file_extension .file_extension
                          :encryption_cipher_text .encryption_cipher_text
                          :encryption_applied .encryption_applied
                          :encryption_blob_encrypted .encryption_blob_encrypted
                          :size .size
                          :is_shared .is_shared
                          :share_id .share_id
                          :master_key_id .master_key_id
                          :user_data .user_data)))
      (setf (JRES-_readall res) complete)
      res)))

(cl-defstruct JTAG
  id
  title
  created_time
  updated_time
  user_created_time
  user_updated_time
  encryption_cipher_text
  encryption_applied
  is_shared
  parent_id
  user_data)

(defun build-JTAG (src)
  "return new JTAG struct from the alist, SRC"
  (let-alist src
    (let ((res (make-JTAG :id .id
                          :title .title
                          :created_time .created_time
                          :updated_time .updated_time
                          :user_created_time .user_created_time
                          :user_updated_time .user_updated_time
                          :encryption_cipher_text .encryption_cipher_text
                          :encryption_applied .encryption_applied
                          :is_shared .is_shared
                          :parent_id .parent_id
                          :user_data .user_data)))
      res)))


(defun joplin--error (level msg &rest args)
  (let ((m (apply 'format (cons msg args))))
    (let ((s (format "%s: %s" level m)))
      (with-current-buffer joplin-error-buffer
        (save-restriction
          (goto-char (point-max))
          (insert s)
          (newline)))
      (when (eq level 'error)
        (error s)))))

(defmacro joplin-with-wait (sec &rest body)
  (declare (indent 1) (debug t))
  (let ((t1 (make-symbol "t1"))
        (delta (make-symbol "delta"))
        (wait (make-symbol "wait"))
        (ret (make-symbol "ret")))
    `(let ((,t1 (current-time))
           (,wait (time-convert ,sec)))
       (setq ,ret (progn ,@body)
             ,delta (time-subtract (current-time) ,t1))
       (when (time-less-p ,delta ,wait)
         (setq ,wait (time-subtract ,wait ,delta))
         (sit-for 0)
         (sleep-for (nth 1 ,wait)
                    (1+ (/ (nth 2 ,wait) 1000))))
       ,ret)))

(defmacro joplin-with-diff-time (&rest body)
  (declare (indent 0) (debug t))
  (let ((t1 (make-symbol "t1"))
        (t2 (make-symbol "t2")))
    `(let ((,t1 (current-time)) ,t2)
       ,@body
       (setq ,t2 (current-time))
       (time-subtract ,t2 ,t1))))

(provide 'joplin-struct)

;;; joplin-struct.el ends here
