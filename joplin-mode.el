;;; joplin.el --- Joplin interface
;;;
;;; Copyright (C) 2005  Seong-Kook Shin <cinsky@gmail.com>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;
;;; Author: Seong-Kook Shin <cinsky@gmail.com>
;;; Keywords: convenience,

(require 'plz)

;;(make-variable-buffer-local 'joplin-note)

(defvar joplin-endpoint "http://127.0.0.1:41184")
(defvar joplin-curl-args '("--proxy" "http://172.22.144.1:8080")
  "Additional arguments to curl(1).  Useful for accessing Joplin through the proxy")

(defvar joplin-context nil)

(cl-defstruct JFOLDER id title parent created-time updated-time user-created-time user-updated-time is-shared icon)
(cl-defstruct JNOTE id title parent_id updated_time markup_language created_time user_updated_time user_created_time source source_application order)
;; (cl-defstruct JNOTE id title parent_id updated_time markup_language created_time user_updated_time user_created_time source source_application order)

(defun joplin--call (method url &optional context)
  (let ((plz-curl-default-args (append joplin-curl-args plz-curl-default-args)))
    (plz method (joplin--build-url url (append context joplin-context)) :as #'json-read)))

(defun joplin--build-url (&optional path ctx)
  (let (qslst url)
    (dolist (p ctx)
      (setq qslst (cons (format "%s=%s" (car p) (cdr p)) qslst)))
    (let ((qs (mapconcat 'identity qslst "&")))
      (setq url (concat joplin-endpoint (or path "")
                        (if (string-equal qs "")
                            ""
                          (concat "?" qs))))
      (joplin-debug "joplin-build-url: %s" url)
      url)))

(defun joplin--load-folders ()
  "Read all folders in a vector of ALIST"
  (let ((context (list (cons 'page 0)))
        (items [])
        (resp '((items . []) (has_more . t))))
    (while (not (eq (cdr (assoc 'has_more resp)) :json-false))
      (setcdr (assoc 'page context) (1+ (cdr (assoc 'page context))))
      ;; (setq resp (plz 'get (joplin-build-url "/folders" context) :as #'json-read))
      (setq resp (joplin--call 'get "/folders" context))
      (setq items (vconcat items (cdr (assoc 'items resp)))))
    items))

(defun joplin---build-pcmap (folders)
  "DEPRECATED -- Build a ALIST of (FOLDER-ID . (CHILD-ID ...)) from a vector, FOLDERS"
  (let (pcmap)
    (dotimes (i (length folders))
      (let ((item (aref folders i)))
        (let ((parent (cdr (assoc 'parent_id item))))
          (let ((record (assoc parent pcmap)))
            (if record
                ; (setcdr record (cons (cdr (assoc 'id item)) (cdr record)))
                (setcdr record (append (cdr record) (list (cdr (assoc 'id item)))))
              (setq pcmap (append pcmap (list (cons (cdr (assoc 'parent_id item))
                                                    (list (cdr (assoc 'id item))))))))))))
    pcmap))

(defun joplin--build-idmap (folders)
  "Build an alist of (FOLDER-ID . JFOLDER)"
  (let (idmap)
    (dotimes (i (length folders))
      (let ((item (aref folders i)))
        (let ((fdr (make-JFOLDER :id (cdr (assoc 'id item))
                                 :parent (cdr (assoc 'parent_id item))
                                 :title (cdr (assoc 'title item)))))
          (setq idmap (cons (cons (cdr (assoc 'id item)) fdr) idmap)))))
    idmap))

(defun joplin--build-pcmap (folders idmap)
  "Build an ALIST of (FOLDER-ID . (CHILD-JFOLDER ...)) from a vector, FOLDERS"
  (let (pcmap)
    (dotimes (i (length folders))
      (let ((item (aref folders i)))
        (let ((parent (cdr (assoc 'parent_id item))))
          (let ((record (assoc parent pcmap)))
            (if record
                ; (setcdr record (cons (cdr (assoc 'id item)) (cdr record)))
                (setcdr record (append (cdr record) (list (cdr (assoc (cdr (assoc 'id item)) idmap)))))
              (setq pcmap (append pcmap (list (cons (cdr (assoc 'parent_id item))
                                                    (list (cdr (assoc (cdr (assoc 'id item)) idmap)))
                                                    )))))))))
    pcmap))

(defvar joplin-folders nil
  "ALIST of (FOLDER-ID . FOLDER) where FOLDER-ID is a string, and FOLDER is a JFOLDER struct")

(defvar joplin-child-folders nil
  "ALIST of (FOLDER-ID . CHILDREN) where FOLDER-ID is a string, and CHILDREN is a list of JFOLDER struct.
An empty string is used for the root folder id.")

(defun joplin--init-folders ()
  (let ((folders (joplin--load-folders)))
    (joplin--parse-folders folders)))

(defun joplin--parse-folders (folders)
  (let ((idmap (joplin--build-idmap folders)))
    (setq joplin-child-folders (joplin--build-pcmap folders idmap))
    (setq joplin-folders idmap)))

(defun joplin--dump-folders (&optional parent level)
  (setq level (or level 0))
  (setq parent (or parent ""))
  ;; (message (format "DUMP %s [%s] %s" pcmap parent level))
  (let ((children (cdr (assoc parent joplin-child-folders))))
    (dolist (c children)
      ;; visit

      (joplin-debug "visit: %s%s" (make-string (* level 2) ?\ )
                    (JFOLDER-title c))
      (joplin--dump-folders (JFOLDER-id c) (1+ level)))))

(defvar joplin-debug-buffer (get-buffer-create "*joplin-debug*"))

(defun joplin--get-note (id)
  (let ((info-ctx (list '(fields . "id,parent_id,title,updated_time,created_time")))
        src note)
    (setq src (joplin--call 'get (concat "/notes/" id) info-ctx))
    (make-JNOTE :id (cdr (assoc 'id src))
                :parent_id (cdr (assoc 'parent_id src))
                :title (cdr (assoc 'title src))
                :updated_time (cdr (assoc 'updated_time src))
                :created_time (cdr (assoc 'created_time src))
                )))

(defun joplin-note-buffer (id &optional buffer)
  "Return the Joplin note buffer.

The buffer contains local variable 'joplin-note, pointing the JNOTE struct of the buffer"
  (let ((note (joplin--get-note id))
        (body-ctx (cons '(fields . "body") joplin-context))
        src body)
    (setq buffer (or buffer (format "%s.md" id)))
    (setq src (plz 'get (joplin-build-url
                         (concat "/notes/" id) body-ctx)
                :as #'json-read))
    (setq body (cdr (assoc 'body src)))

    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (and (fboundp 'markdown-mode)
           (markdown-mode))
      (insert body)
      (goto-char (point-min))
      (setq-local joplin-note note)
      (joplin-note-mode)
      (message "note: %s" note)
      (set-buffer-modified-p nil)
      (current-buffer))))

(defun joplin--completing-folder-name (prompt)
  "Read a user selected folder name in the minibuffer, return the folder ID or nil"
  (let (sel resp)
    (cl-dolist (f joplin-folders)
      (let ((folder (cdr f)))
        (setq sel (cons (cons (JFOLDER-title folder)
                              (JFOLDER-id folder))
                        sel))))
    (setq resp (completing-read prompt sel nil 'require-match nil 'joplin-folder-name-history))
    (and resp
         (cdr (assoc resp sel)))))


(defun joplin-debug (msg &rest args)
  (let ((m (apply 'format (cons msg args))))
    (with-current-buffer joplin-debug-buffer
      (save-restriction
        (goto-char (point-max))
        (insert m)
        (newline)))))

(defun joplin--get-folder (&optional id)
  (or id
      (if (boundp 'joplin-note)
          (setq id (JNOTE-parent_id joplin-note))))
  (and id
       (cdr (assoc id joplin-folders))))

(defun joplin--time-string (time)
  ;; Joplin time is an integer in milliseconds.
  (format-time-string "%c" (1+ (/ time 1000))))




(defun joplin-show-properties ()
  (interactive)
  (if (not (boundp 'joplin-note))
      (message "Note information not found")
    (let ((folder (joplin--get-folder)))
      (message (format "Folder: %s\nTitle: %s\nUpdated: %s\nCreated: %s\nId: %s"
                       (JFOLDER-title folder)
                       (JNOTE-title joplin-note)
                       (joplin--time-string (JNOTE-updated_time joplin-note))
                       (joplin--time-string (JNOTE-created_time joplin-note))
                       (JNOTE-id joplin-note))))))


(define-minor-mode joplin-note-mode
  "a minor mode for Jopline note"
  :lighter "JPL"
  :keymap
  '(([(control ?c) ?j ?i] . joplin-show-properties)))
