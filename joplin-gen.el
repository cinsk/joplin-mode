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

(iter-defun joplin--search-notes-by-text (text)
  (let ((context (copy-alist
                  `((page . 0)
                    (limit . 10)
                    (fields . "id,parent_id,title,created_time,updated_time,is_conflict,latitude,longitude,altitude,author,source_url,is_todo,todo_due,todo_completed,source,source_application,application_data,order,user_updated_time,user_created_time,encryption_cipher_text,encryption_applied,markup_language,is_shared,share_id,conflict_original_id,master_key_id,user_data,source")
                    (type . note)
                    (query . ,(url-hexify-string text))
                    )))
        (items [])
        (resp '((items . []) (has_more . t))))
    (while (not (eq (alist-get 'has_more resp) :json-false))
      (cl-incf (alist-get 'page context))
      (setq resp (joplin--http-get "/search" context))
      (let ((items (alist-get 'items resp)))
        (dotimes (i (length items))
          (iter-yield (build-JNOTE (aref items i))))))))

(provide 'joplin-gen)
;;; joplin-gen.el ends here
