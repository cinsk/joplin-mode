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

(defvar joplin-folder-name-history nil)
(defvar joplin-context nil)

(cl-defstruct JFOLDER
  id title parent_id
  ;; these are not supported yet (Joplin 2.12.18)
  created_time updated_time user_created_time user_updated_time is-shared icon)

(cl-defstruct JNOTE
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

(defun joplin--http-get (url &optional context)
  "GET method with CONTEXT (query-string)"
  (let ((plz-curl-default-args (append joplin-curl-args plz-curl-default-args)))
    (plz 'get (joplin--build-url url (append context joplin-context)) :as #'json-read)))

(defun joplin--http-put (url body &optional context)
  "PUT method with an alist BODY to URL with CONTEXT (query-string)"
  (let ((plz-curl-default-args (append joplin-curl-args plz-curl-default-args)))
    (plz 'put
         (joplin--build-url url (append context joplin-context))
         :as #'json-read
         :body (json-encode body)
         )))

(defun joplin--http-post (url body &optional context)
  "PUT method with an alist BODY to URL with CONTEXT (query-string)"
  (let ((plz-curl-default-args (append joplin-curl-args plz-curl-default-args)))
    (plz 'post
         (joplin--build-url url (append context joplin-context))
         :as #'json-read
         :body (json-encode body)
         )))

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
      (setq resp (joplin--http-get "/folders" context))
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
                                 :parent_id (cdr (assoc 'parent_id item))
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
  "ALIST of (FOLDER-ID . CHILDREN) where FOLDER-ID is a string, and CHILDREN
is a list of JFOLDER struct.
An empty string is used for the root folder id.")

(defun joplin--init-folders ()
  ;; init(sync) joplin, build folder structure, etc.
  (let ((folders (joplin--load-folders)))
    (joplin--parse-folders folders)))

(defun joplin-sync-folders ()
  (interactive)
  (joplin--init-folders)
  (joplin--render-joplin-buffer))

(defun joplin--parse-folders (folders)
  (let ((idmap (joplin--build-idmap folders)))
    (setq joplin-child-folders (joplin--build-pcmap folders idmap))
    (setq joplin-folders idmap)))

(defun joplin--walk-folders (proc &optional parent level)
  (setq level (or level 0))
  (setq parent (or parent ""))
  ;; (message (format "DUMP %s [%s] %s" pcmap parent level))
  (let ((children (cdr (assoc parent joplin-child-folders))))
    (dolist (c children)
      ;; visit
      (funcall proc c level)
      (joplin--walk-folders proc (JFOLDER-id c) (1+ level)))))

(defun joplin--dump-folders (&optional parent level)
  (joplin--walk-folders (lambda (folder lev)
                          (joplin-debug "visit: %s%s" (make-string (* lev 2) ?\ )
                                        (JFOLDER-title folder)))))

(defvar joplin-debug-buffer (get-buffer-create "*joplin-debug*"))

(defun build-JNOTE (src)
  "return new JNOTE struct from the alist, SRC"
  ;; Note 'body member is not included in JNOTE
  (make-JNOTE :id (cdr (assoc 'id src))
              :parent_id (cdr (assoc 'parent_id src))
              :title (cdr (assoc 'title src))
              :created_time (cdr (assoc 'created_time src))
              :updated_time (cdr (assoc 'updated_time src))
              :is_conflict (cdr (assoc 'is_conflict src))
              :latitude  (cdr (assoc 'latitude src))
              :longitude  (cdr (assoc 'longitude src))
              :altitude  (cdr (assoc 'altitude src))
              :author  (cdr (assoc 'author src))
              :source_url  (cdr (assoc 'source_url src))
              :is_todo  (cdr (assoc 'is_todo src))
              :todo_due  (cdr (assoc 'todo_due src))
              :todo_completed  (cdr (assoc 'todo_completed src))
              :source  (cdr (assoc 'source src))
              :source_application  (cdr (assoc 'source_application src))
              :application_data  (cdr (assoc 'application_data src))
              :order  (cdr (assoc 'order src))
              :user_updated_time (cdr (assoc 'user_updated_time src))
              :user_created_time (cdr (assoc 'user_created_time src))
              :encryption_cipher_text  (cdr (assoc 'encryption_cipher_text src))
              :encryption_applied  (cdr (assoc 'encryption_applied src))
              :markup_language  (cdr (assoc 'markup_language src))
              :is_shared  (cdr (assoc 'is_shared src))
              :share_id  (cdr (assoc 'share_id src))
              :conflict_original_id  (cdr (assoc 'conflict_original_id src))
              :master_key_id  (cdr (assoc 'master_key_id src))
              :user_data  (cdr (assoc 'user_data src))
              ))

(defun joplin--get-note (id)
  (let ((info-ctx (list '(fields . "id,parent_id,title,created_time,updated_time,is_conflict,latitude,longitude,altitude,author,source_url,is_todo,todo_due,todo_completed,source,source_application,application_data,order,user_updated_time,user_created_time,encryption_cipher_text,encryption_applied,markup_language,is_shared,share_id,conflict_original_id,master_key_id,user_data,source")))
        src note)
    (setq src (joplin--http-get (concat "/notes/" id) info-ctx))
    (build-JNOTE src)))

(defun joplin-note-buffer (id &optional buffer)
  "Return the Joplin note buffer.

The buffer contains local variable 'joplin-note, pointing the JNOTE struct of the buffer"
  (let ((note (joplin--get-note id))
        (body-ctx (list '(fields . "body")))
        src body)
    (setq buffer (or buffer (format "%s.md" id)))
    (setq src (joplin--http-get (concat "/notes/" id) body-ctx))
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

(defun joplin--folder-id (&optional id)
  (or id
      (if (boundp 'joplin-note)
          (setq id (JNOTE-parent_id joplin-note)))))

(defun joplin--folder (&optional id)
  "Return the folder struct by ID"
  (let ((fid (joplin--folder-id id)))
    (and fid
         (cdr (assoc fid joplin-folders)))))

(defun joplin--time-string (time)
  ;; Joplin time is an integer in milliseconds.
  (format-time-string "%c" (1+ (/ time 1000))))



(defun joplin-show-properties ()
  (interactive)
  (if (not (boundp 'joplin-note))
      (message "Note information not found")
    (let ((folder (joplin--folder)))
      (message (format "Folder: %s\nTitle: %s\nUpdated: %s\nCreated: %s\nId: %s"
                       (JFOLDER-title folder)
                       (JNOTE-title joplin-note)
                       (joplin--time-string (JNOTE-updated_time joplin-note))
                       (joplin--time-string (JNOTE-created_time joplin-note))
                       (JNOTE-id joplin-note))))))


(defvar joplin-folder-buffer-name "*Joplin*")

(define-derived-mode joplin-mode fundamental-mode "Joplin" "docstring...")

(setq joplin-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map [(control ?n)] #'joplin-next-folder)
        (define-key map [(control ?p)] #'joplin-previous-folder)
        (define-key map [(?n)] #'joplin-next-folder)
        (define-key map [(?p)] #'joplin-previous-folder)
        (define-key map [(?i)] #'joplin-show-folder-properties)
        (define-key map [(?g)] #'joplin-sync-folders)
        ;;(define-key map [(?c)] #'joplin-new-note)
        map))

(defun joplin-jump-to-folder (&optional folder-id)
  (interactive)
  (let (found done)
    (setq folder-id (joplin--folder-id folder-id))

    (if (null folder-id)
        (switch-to-buffer (joplin--buffer))
      (with-current-buffer (joplin--buffer)
        (let ((pos (point-max)))
          (while (not done)
            (setq pos (previous-overlay-change pos))
            (let* ((ol (car (overlays-at pos)))
                   (folder (and ol (overlay-get ol 'joplin-folder))))
              (if (and folder
                       (string-equal (JFOLDER-id folder) folder-id))
                  (setq found pos done t)))
            (if (= pos (point-min))
                (setq done t)))))
      (switch-to-buffer (joplin--buffer))
      (if found
          (progn (goto-char found)
                 (re-search-forward "^ *\\[[^\\]]*\\] *" (line-end-position) t))
        (message "error: folder[%s] not found." folder-id)))))

(defun joplin-next-folder (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (cl-dotimes (i arg)
    (beginning-of-line)
    (let ((pos (next-overlay-change (point))))
      (message "point(%d) next(%d)" (point) pos)
      (goto-char pos)
      (re-search-forward "^ *\\[[^\\]]*\\] *" (line-end-position) t)
      )))

(defun joplin-previous-folder (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (cl-dotimes (i arg)
    (beginning-of-line)
    (let ((pos (previous-overlay-change (point))))
      (message "point(%d) previous(%d)" (point) pos)
      (goto-char pos)
      (re-search-forward "^ *\\[[^\\]]*\\] *" (line-end-position) t)
      )))

(defun joplin-show-folder-properties ()
  (interactive)
  (let* ((ol (car (overlays-at (point))))
         (folder (and ol
                      (overlay-get ol 'joplin-folder))))
    (if folder
        ;; TODO: display more information?
        (message "%s: %s" (JFOLDER-id folder) (JFOLDER-title folder))
      (message "no folder found in the current position"))))

(defun joplin--buffer ()
  "Return the buffer of top-level joplin buffer.  If not, create it."
  (and (get-buffer joplin-folder-buffer-name)
       (joplin--render-joplin-buffer)))


(defun joplin ()
  (interactive)
  ;; TODO: get token, then set `joplin-context'.

  (switch-to-buffer (joplin--buffer)))

(defun joplin--render-joplin-buffer ()
  (or joplin-folders
      (joplin--init-folders))
  (with-current-buffer (get-buffer-create joplin-folder-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; TODO: set the major mode here
      (joplin-mode)
      (let ((mkr (point-min-marker)))
        (set-marker-insertion-type mkr nil)

        (joplin--walk-folders
         (lambda (folder lev)
           (let ((indent (make-string (* lev 4) ?\ )))
             (insert (format "%s[ ] %s" indent
                             (JFOLDER-title folder)))
             (let ((col (current-column)))
               (insert (format "%s%s\n" (if (< col 60) (make-string (- 60 col) ?\ ) " ")
                               (JFOLDER-id folder))))
             ;; (insert (format "%s  - id: %s\n" indent (JFOLDER-id folder)))

             (let ((ol (make-overlay mkr (point-max))))
               (overlay-put ol 'joplin-folder folder))
             (set-marker mkr (point-max))
             )))))
    (setq buffer-read-only t)
    (current-buffer)))


(defun joplin--update-note ()
  (let ((id (JNOTE-id joplin-note))
        resp)
    (save-restriction
      (widen)
      ;; TODO: how to change the title of the note?
      (setq resp (joplin--http-put
                  (concat "/notes/" id)
                  (list (cons 'body
                              (buffer-substring-no-properties (point-min) (point-max)))))))

    (set-buffer-modified-p nil)
    (setq joplin-note (build-JNOTE resp))))


;; (let ((title (read-from-minibuffer "Note title: "))
;;       (folder (joplin--completing-folder-name "Folder: "))
(defun joplin--register-note (&optional title folder)
  ;; upload current buffer as a new note
  (let (resp data)
    ;; (and (buffer-modified-p) (save-buffer))
    (save-restriction
      (widen)
      (setq data (list (cons 'title (if (> (length title) 0)
                                        title
                                      (if buffer-file-name
                                          (format "%s" buffer-file-name)
                                        "Untitled")))))

      (and folder
           (setq data (cons (cons 'parent_id folder) data)))

      (setq data (cons (cons 'body (buffer-substring-no-properties (point-min) (point-max))) data))
      (setq resp (joplin--http-post "/notes/" data)))
    ;; not all fields are present on HTTP POST.
    ;;
    ;; body, created_time, id, markup_language, order, parent_id, source, source_application,
    ;; title, updated_time, user_created_time, user_updated_time
    (setq-local joplin-note (build-JNOTE resp))))


(defun joplin-save-note ()
  (interactive)
  (and buffer-file-name (save-buffer))

  (if (boundp 'joplin-note)
      (joplin--update-note)
    (let ((title (read-from-minibuffer "Note title: "))
          (folder (joplin--completing-folder-name "Folder: ")))
      (joplin--register-note title folder))))


(define-minor-mode joplin-note-mode
  "a minor mode for Jopline note"
  :lighter "JPL"
  :keymap
  '(([(control ?c) ?j ?i] . joplin-show-properties)
    ([(control ?c) ?j ?j] . joplin-jump-to-folder)
    ([(control ?c) ?j ?s] . joplin-save-note)
    ))


;;(add-hook 'markdown-mode-hook 'joplin-note-mode)
