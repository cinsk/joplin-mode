;;; joplin-gen.el ---                               -*- lexical-binding: t; -*-

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

;; Using Emacs's generator (e.g. iter-*) set requires lexical
;; bindings, so I'm separating Joplin functions that uses it.  Not
;; sure whether I should enable lexical bindings for all Joplin
;; sources.

(require 'joplin-http)
(require 'joplin-struct)
(require 'generator)

(iter-defun joplin--gen-search-notes (text)
  (let ((context (copy-alist
                  `((page . 0)
                    (limit . 10)
                    (order_by . "order")
                    (order_dir . "desc")
                    (fields . "id,parent_id,title,created_time,updated_time,is_conflict,latitude,longitude,altitude,author,source_url,is_todo,todo_due,todo_completed,source,source_application,application_data,order,user_updated_time,user_created_time,encryption_cipher_text,encryption_applied,markup_language,is_shared,share_id,conflict_original_id,master_key_id,user_data,source")
                    (type . note)
                    (query . ,(url-hexify-string text))
                    )))
        (items [])
        (resp '((items . []) (has_more . t))))
    (while (not (eq (alist-get 'has_more resp) :json-false))
      (cl-incf (alist-get 'page context))
      (setq resp (joplin--http-get "/search" context))
      (let ((items (alist-get 'items resp))
            notes)
        (dotimes (i (length items))
          (push (build-JNOTE (aref items i)) notes))

        (setq notes (nreverse notes))
        ;;(setq notes (sort notes (lambda (a b) (> (JNOTE-order a) (JNOTE-order b)))))

        (dolist (n notes)
          (iter-yield n))))))

(iter-defun joplin--gen-notes (fid)
  (let ((context (copy-alist
                  `((page . 0)
                    (limit . 10)
                    (order_by . "user_updated_time")
                    (order_dir . "desc")
                    (fields . "id,parent_id,title,created_time,updated_time,is_conflict,latitude,longitude,altitude,author,source_url,is_todo,todo_due,todo_completed,source,source_application,application_data,order,user_updated_time,user_created_time,encryption_cipher_text,encryption_applied,markup_language,is_shared,share_id,conflict_original_id,master_key_id,user_data,source")
                    (type . note)
                    )))
        (items [])
        (resp '((items . []) (has_more . t))))
    (while (not (eq (alist-get 'has_more resp) :json-false))
      (cl-incf (alist-get 'page context))
      (setq resp (joplin--http-get (concat "/folders/" fid "/notes") context))
      (let ((items (alist-get 'items resp))
            notes)
        (dotimes (i (length items))
          (push (build-JNOTE (aref items i)) notes))

        (setq notes (nreverse notes))
        ;;(setq notes (sort notes (lambda (a b) (> (JNOTE-order a) (JNOTE-order b)))))

        (dolist (n notes)
          (iter-yield n))))))

(iter-defun joplin--gen-note-resources (id)
  ;; TODO: I'm not sure whether this is the right approach.
  ;; If we just care about the embedded images, it's better to get the
  ;; id of the resources from the note buffer, as the API /notes/ID/resources
  ;; is not able to tell whether the resources is for an embedded image.
  (let ((context (copy-alist
                  `((page . 0)
                    (limit . 10)
                    (fields . "id,title,mime,updated_time,filename,file_extension,size"))))
        (items [])
        (resp '((items . []) (has_more . t))))
    (while (not (eq (alist-get 'has_more resp) :json-false))
      (cl-incf (alist-get 'page context))
      (setq resp (joplin--http-get (format "/notes/%s/resources" id) context))
      (let ((items (alist-get 'items resp)))
        (dotimes (i (length items))
          (iter-yield (build-JRES (aref items i))))))))


(iter-defun joplin--gen-tags ()
  ;; TODO: I'm not sure whether this is the right approach.
  ;; If we just care about the embedded images, it's better to get the
  ;; id of the resources from the note buffer, as the API /notes/ID/resources
  ;; is not able to tell whether the resources is for an embedded image.
  (let ((context (copy-alist
                  `((page . 0)
                    (limit . 10)
                    (fields . "id,title"))))
        (items [])
        (resp '((items . []) (has_more . t))))
    (while (not (eq (alist-get 'has_more resp) :json-false))
      (cl-incf (alist-get 'page context))
      (setq resp (joplin--http-get "/tags" context))
      (let ((items (alist-get 'items resp)))
        (dotimes (i (length items))
          (iter-yield (build-JTAG (aref items i))))))))


(provide 'joplin-gen)
;;; joplin-gen.el ends here
