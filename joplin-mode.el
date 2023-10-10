;;; joplin-mode.el --- Joplin interface

;; Copyright (C) 2005  Seong-Kook Shin <cinsky@gmail.com>

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


(require 'joplin-struct)
(require 'joplin-http)
(require 'joplin-gen)


;; Entry points
;;
;; Most of Joplin requests requires token, which stored in the
;; variable `joplin-context'.  Also folder structure should be parsed and
;; stored in the variable `joplin-folders'.   These are done by `joplin--init',
;; and will be called on these entry points.
;;
;; \\[joplin],
;; \\[joplin-search],
;; \\[joplin-save-note],
;; \\[joplin-jump-to-folder]


(defface joplin-note-id-face
  '((t :inherit shadow))
  "Face for Joplin Note id"
  :group 'joplin-faces)
(defface joplin-note-created-time-face
  '((t :inherit font-lock-constant-face))
  "Face for Joplin Note created_time"
  :group 'joplin-faces)
(defface joplin-note-updated-time-face
  '((t :inherit flock-lock-type-face))
  "Face for Joplin Note updated_time"
  :group 'joplin-faces)
(defface joplin-note-title-face
  '((t :inherit font-lock-string-face))
  "Face for Joplin Note title"
  :group 'joplin-faces)
(defface joplin-note-mark-face
  '((t :inherit dired-mark))
  "Face for Joplin Note mark"
  :group 'joplin-faces)


;;
;; Internal gloval variable.  Not expected to be overriden by users.
;;
(defvar joplin-folder-buffer-name "*Joplin*")
(defvar joplin-search-buffer-name "*JoplinSearch*")

(defvar joplin-folder-name-history nil
  "History variable for `joplin--completing-folder-name'.")
(defvar joplin-limit-per-search 100
  "The amount of items rendered on *JoplinSearch* buffer.

It should never be larger than 100.
See URL `https://joplinapp.org/api/references/rest_api/#pagination'.")

(defvar joplin-folders nil
  "ALIST of (FOLDER-ID . FOLDER) where FOLDER-ID is a string, and FOLDER is a JFOLDER struct")
(defvar joplin-child-folders nil
  "ALIST of (FOLDER-ID . CHILDREN) where FOLDER-ID is a string, and CHILDREN
is a list of JFOLDER struct.
An empty string is used for the root folder id.")


(defvar joplin-temp-note-file (make-temp-file "joplin"))

(defun joplin--get-api-token ()
  "Get Joplin API token from JoplinApp"
  (condition-case e
      (progn
        (let ((auth (alist-get 'auth_token (joplin--http-post "/auth" nil)))
              token)
          (while (not token)
            (read-from-minibuffer
             "Switch to Joplin App, then accept authorization request [RET]: ")
            (setq token (alist-get 'token
                                   (joplin--http-get "/auth/check"
                                                     `((auth_token . ,auth))))))
          token))
    (error (joplin--error 'error "%s" e)
           ;; (signal (car e) (cdr e))  ; to re-rasise
           nil)))

(defun joplin--save-token (token)
  (let ((path (concat (file-name-as-directory user-emacs-directory)
                      joplin-token-file)))
    (with-temp-buffer
      (insert token)
      (with-file-modes #o600
        (write-region (point-min) (point-max) path)))))

(defun joplin--load-folders ()
  "Read all folders in a vector of ALIST"
  (let ((context (list (cons 'page 0)))
        (items [])
        (resp '((items . []) (has_more . t))))
    (while (not (eq (alist-get 'has_more resp) :json-false))
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
                ;; (setcdr record (cons (cdr (assoc 'id item)) (cdr record)))
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
                ;; (setcdr record (cons (cdr (assoc 'id item)) (cdr record)))
                (setcdr record (append (cdr record) (list (cdr (assoc (cdr (assoc 'id item)) idmap)))))
              (setq pcmap (append pcmap (list (cons (cdr (assoc 'parent_id item))
                                                    (list (cdr (assoc (cdr (assoc 'id item)) idmap)))
                                                    )))))))))
    pcmap))

(defun joplin--init-folders ()
  ;; init(sync) joplin, build folder structure, etc.
  (let ((folders (joplin--load-folders)))
    (joplin--parse-folders folders)))

(defun joplin-sync-folders ()
  (interactive)
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
                          (joplin--error 'debug "visit: %s%s" (make-string (* lev 2) ?\ )
                                         (JFOLDER-title folder)))))

(defvar joplin-error-buffer (get-buffer-create "*joplin-debug*"))


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
      ;;(message "note: %s" note)
      (set-buffer-modified-p nil)

      (setq-local buffer-file-name joplin-temp-note-file
                  joplin-temp-file t
                  write-file-functions '(joplin-save-note)))
    (current-buffer)))

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

(defun joplin--time-string-short (time)
  (format-time-string "%m/%d/%yT%H:%M" (1+ (/ time 1000))))


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

;;;###autoload
(define-derived-mode joplin-mode special-mode "Joplin" "docstring..."
  (hl-line-mode)
  )
(setq joplin-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map [(control ?n)] #'joplin-next-folder)
        (define-key map [(control ?p)] #'joplin-previous-folder)
        (define-key map [(?n)] #'joplin-next-folder)
        (define-key map [(?p)] #'joplin-previous-folder)
        (define-key map [(?i)] #'joplin-show-folder-properties)
        (define-key map [(?g)] #'joplin-sync-folders)
        (define-key map [(?s)] #'joplin-search)
        ;;(define-key map [(?c)] #'joplin-new-note)
        (define-key map [?q] #'(lambda ()
                                 (interactive)
                                 (quit-window 'kill)))
        map))

(defun joplin-jump-to-folder (&optional folder-id)
  "Switch to Joplin folder buffer.

It will move the point to the corresponding folder"
  (interactive)
  (joplin--init)
  (let (found done)
    ;; below line assumes that current buffer is the note buffer.
    (setq folder-id (joplin--folder-id folder-id))
    (unless folder-id                   ; try search current note
      (let ((note (joplin--search-note-from-overlay)))
        (and note
             (setq folder-id (JNOTE-parent_id note)))))
    (joplin--error 'debug "folder-id: %s" folder-id)
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
  (unless (get-buffer joplin-folder-buffer-name)
    (joplin--render-joplin-buffer))
  (get-buffer joplin-folder-buffer-name))


;; (condition-case e
;;   (plz 'post (joplin--build-url "/auth" nil)
;;     :as #'json-read
;;     :then 'sync)
;;   (error e))

(defun joplin-refresh-token ()
  "Save Joplin api token in a file retrived from JoplinApp."
  (let ((token (joplin--get-api-token)))
    (when token
      (joplin--save-token token))))

(defun joplin--init()
  (unless joplin-context
    (joplin--init-context))
  (unless joplin-folders
    (joplin--init-folders)))

(defun joplin--init-context ()
  (let ((path (concat (file-name-as-directory user-emacs-directory)
                      joplin-token-file))
        token)
    ;; insert-file-contents filename
    (if (not (file-readable-p path))
        (joplin-refresh-token))

    (with-temp-buffer
      (insert-file-contents path)
      (setq token (string-trim (buffer-string))))

    (if (= (length token) 0)
        (joplin--error 'error "empty token in %s" path))

    (when token
      (setq joplin-context (list (cons 'token token))))))

(defmacro joplin--context (&rest args)
  ;; Return a context(alist) in ARGS with `joplin-context'.
  ;; ARGS is in the form of [SYM VAL]...
  ;; Note that joplin-context is not modified

  (if (eq (mod (length args) 2) 1)
      (signal 'wrong-number-of-arguments (list 'joplin--context (length args))))
  (let ((var (make-symbol "context"))
        (a (make-symbol "first"))
        (b (make-symbol "second"))
        (lst (make-symbol "lst")))
    `(let (,var ,a ,b (,lst (quote ,args)))
       (unless joplin-context
         (joplin--init-context))
       (while ,lst
         (setq ,var (cons (cons (eval (pop ,lst)) (eval (pop ,lst))) ,var)))
       ,var)))

(defun joplin--get-api-token ()
  (condition-case e
      (let ((auth (cdr (assoc 'auth_token (joplin--http-post "/auth" nil))))
            token)
        (while (not token)
          (read-from-minibuffer
           "Switch to Joplin App, then accept authorization request [RET]: ")
          (setq token (alist-get 'token
                                 (joplin--http-get "/auth/check"
                                                   `((auth_token . ,auth))))))
        token)
    (error (joplin--error 'error "%s" e)
           ;; (signal (car e) (cdr e))
           nil
           )))

(defun joplin--save-token (token)
  (let ((path (concat (file-name-as-directory user-emacs-directory)
                      joplin-token-file)))
    (with-temp-buffer
      (insert token)
      (with-file-modes #o600
        (write-region (point-min) (point-max) path)))))

;;;###autoload
(defun joplin (&optional arg)
  (interactive "p")
  ;; TODO: get token, then set `joplin-context'.
  (joplin--init)
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
               (insert (format "%s%s\n" (if (< col 40)
                                            (make-string (- 40 col) ?\ ) " ")
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
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))))

    (let ((newnote (build-JNOTE resp t)))
      (setq joplin-note newnote)
      ;; TODO: is there any data structure that need to be updated
      ;;       with newnote?
      (if (not (eq (JNOTE-is_conflict newnote) 0))
          (message "This note has a conflict, check in JoplinApp"))
      (set-buffer-modified-p nil)
      (message "note updated - %s" (JNOTE-id newnote))
      (JNOTE-id newnote))))

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

      (setq data (cons (cons 'body
                             (buffer-substring-no-properties (point-min)
                                                             (point-max)))
                       data))
      (setq resp (joplin--http-post "/notes/" data)))
    ;; not all fields are present on HTTP POST.
    ;;
    ;; body, created_time, id, markup_language, order, parent_id,
    ;; source, source_application, title, updated_time,
    ;; user_created_time, user_updated_time
    (let* ((note (build-JNOTE resp))
           (noteid (JNOTE-id note)))
      (setq-local joplin-note note)
      (message "node posted - %s" noteid)
      noteid)))


(defun joplin-save-note (&optional arg)
  (interactive "p")
  (joplin--init)
  (if (and (boundp 'joplin-temp-file)
           joplin-temp-file)
      nil
    (save-buffer))

  (if (boundp 'joplin-note)
      (joplin--update-note)
    (let ((title (read-from-minibuffer "Note title: "))
          (folder (joplin--completing-folder-name "Folder: ")))
      (joplin--register-note title folder)

      (setq-local joplin-source-file buffer-file-name
                  buffer-file-name joplin-temp-note-file
                  joplin-temp-file t
                  write-file-functions '(joplin-save-note))

      (rename-buffer (joplin--note-buffer-name joplin-note))
      (JNOTE-id joplin-note)))
  ;; Reminder.  This function should return non-nil as it is part of
  ;; variable `write-file-functions'.  Since `joplin--update-note' and
  ;; `joplin--register-note' both return note id, it should work.
)

;;;###autoload
(define-minor-mode joplin-note-mode
  "a minor mode for Jopline note"
  :lighter "JPL"
  :keymap
  '(([(control ?c) ?j ?i] . joplin-show-properties)
    ([(control ?c) ?j ?j] . joplin-jump-to-folder)
    ([(control ?c) ?j ?s] . joplin-save-note)
    ))


;;
;; Search
;;
(define-derived-mode joplin-search-mode special-mode "JoplinSearch"
  "docstring..."

  (make-local-variable 'joplin-notes)
  (make-local-variable 'joplin-visible-fields)
  (make-local-variable 'joplin-search-text)
  (make-local-variable 'joplin-search-count)
  (make-local-variable 'joplin-search-limit)
  (make-local-variable 'joplin-search-done)

  (toggle-truncate-lines 1)
  (hl-line-mode)
  (setq-local buffer-read-only t
              joplin-notes ()
              joplin-visible-fields (copy-alist '((id . t)
                                                  (created_time . t)
                                                  (updated_time . t)
                                                  (title . t)))
              joplin-eob-marker (point-min-marker)
              joplin-search-iter nil
              joplin-search-count 0
              joplin-search-limit joplin-limit-per-search
              joplin-search-text "")
  )
(setq joplin-search-mode-map
      (let ((map (make-sparse-keymap)))
        ;; (define-key map [(control ?n)] #'joplin-next-folder)
        ;; (define-key map [(control ?p)] #'joplin-previous-folder)
        ;; (define-key map [(?n)] #'joplin-next-folder)
        ;; (define-key map [(?p)] #'joplin-previous-folder)
        ;; (define-key map [(?i)] #'joplin-show-folder-properties)
        ;; (define-key map [(?g)] #'joplin-sync-folders)
        ;;(define-key map [(?c)] #'joplin-new-note)

        ;; id field show/hide
        ;; created_time show/hide
        ;; updated_time show/hide
        ;; mark current line(note)
        ;; visit the note
        ;; jump to joplin folder buffer
        ;; quit
        (define-key map [?/] #'joplin-search)
        (define-key map [?s] #'joplin-search)
        ;; (define-key map [?\r] #'joplin-search) ; return key
        (define-key map [?g] #'joplin-search-revert)
        (define-key map [?n] #'joplin-search-next-line)
        (define-key map [?p] #'joplin-search-previous-line)
        (define-key map [?d] #'joplin-search-debug)
        (define-key map [?t ?i] #'joplin-search-toggle-id)
        (define-key map [?t ?c] #'joplin-search-toggle-created-time)
        (define-key map [?t ?u] #'joplin-search-toggle-updated-time)
        (define-key map [?^] #'joplin-jump-to-folder)
        (define-key map [(control ?c) (control ?j)] #'joplin-jump-to-folder)
        (define-key map [(control ?c) ?j ?j] #'joplin-jump-to-folder)

        (define-key map [?q] #'quit-window)
        (define-key map [?o] #'joplin-search-visit-note-other-window)
        (define-key map [?\r] #'joplin-search-visit-note)
        map))

(defun joplin--note-buffer-name (note)
  (format "*JoplinNote:%s*" (JNOTE-id note)))

(defun joplin-search-visit-note (&optional arg)
  (interactive)
  ;; (joplin-note-buffer "id")
  (let* ((note (joplin--search-note-from-overlay))
         (bufname (joplin--note-buffer-name note)))
    (when note
      (let ((buf (get-buffer bufname)))
        (if buf
            (switch-to-buffer buf)
          (setq buf (get-buffer-create bufname))
          (joplin-note-buffer (JNOTE-id note) buf)
          (switch-to-buffer buf)
          )))))


(defun joplin-clear-search (&optional arg)
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (set-marker joplin-eob-marker (point-min))
    (setq joplin-notes ()
          joplin-search-iter nil
          joplin-search-count 0
          joplin-search-limit joplin-limit-per-search
          joplin-search-text "")))

(defun joplin--search-buffer ()
  "Return Joplin Search buffer, create one if none exists."
  (if (eq major-mode 'joplin-search-mode)
      (current-buffer)
    (let ((buf (get-buffer-create joplin-search-buffer-name)))
      (with-current-buffer buf
        (joplin-search-mode)
        (current-buffer)))))

;;;###autoload
(defun joplin-search (&optional prefix text)
  (interactive "p\nssearch: ")

  (joplin--init)
  (let ((buf (joplin--search-buffer)))
    (when (> (length text) 0)
      (let ((iter (joplin--search-notes-by-text text)))

        (with-current-buffer buf
          (joplin-clear-search)
          (setq joplin-search-iter iter
                joplin-search-text text)
          (joplin--search-load)
          (goto-char (point-min))
          (joplin-search-point-to-title))))
    (pop-to-buffer buf)))

(defun joplin-search-revert (&optional arg)
  "Replace current search buffer text with JoplinApp search result."
  (interactive "p")

  (let (noteid)
    (let ((note (joplin--search-note-from-overlay)))
      (when note
        (setq noteid (JNOTE-id note))))

    (when (> (length joplin-search-text) 0)
      (let ((iter (joplin--search-notes-by-text joplin-search-text)))
        (setq joplin-notes ()
              joplin-search-iter iter
              joplin-search-count 0))

      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-marker joplin-eob-marker (point-min)))

      (joplin--search-load))

    (let ((pos (joplin--search-note-position noteid)))
      (when pos
        (goto-char pos)))))

(defun joplin--render-note (note)
  (let (text items)
    (set-marker joplin-eob-marker (point))

    (when (alist-get 'id joplin-visible-fields)
      (let ((jid (JNOTE-id note))
            short ps)
        (setq short (format "%-7s" (if (> (length jid) 7)
                                       (substring jid 0 7)
                                     ""))
              ps (propertize short
                             'face 'joplin-note-id-face
                             'jfield 'id)
              items (cons ps items))))

    (when (alist-get 'created_time joplin-visible-fields)
      (let ((tm (JNOTE-created_time note))
            tmstr ps)
        (setq tmstr (format "%-14s" (if tm
                                        (joplin--time-string-short tm) ""))
              ps (propertize tmstr
                             'face 'joplin-note-created-time-face
                             'jfield 'created_time)
              items (cons ps items))))

    (when (alist-get 'updated_time joplin-visible-fields)
      (let ((tm (JNOTE-updated_time note))
            tmstr ps)
        (setq tmstr (format "%-14s" (if tm
                                        (joplin--time-string-short tm) ""))
              ps (propertize tmstr
                             'face 'joplin-note-updated-time-face
                             'jfield 'updated_time)
              items (cons ps items))))

    (setq text (string-join (nreverse items) " "))
    (insert (format "%s  %s\n" text
                    (propertize (JNOTE-title note)
                                'face 'joplin-note-title-face
                                'jfield 'title
                                'jnote note
                                )
                    ))
    (let ((ol (make-overlay joplin-eob-marker (point))))
      (overlay-put ol 'joplin-note note))
    (set-marker joplin-eob-marker (point))))


;; TODO: enable third-party minor modes may add additional overlay to
;; the buffer.  Should handle properly
(defun joplin--search-note-from-overlay ()
  (let (note)
    (cl-loop for o in (overlays-at (point))
             when (setq note (overlay-get o 'joplin-note)) return note)))


(defun joplin--search-next-note-pos (pos)
  (if (get-text-property pos 'jnote)
      pos
    (next-single-property-change pos 'jnote)))


(defun joplin--search-note-position (id &optional startpos)
  "buffer position of the note title matched with ID starting from STARTPOS"
  (let ((pos (or startpos (point-min))) done jnote)
    (while (and (not done)
                (setq pos (joplin--search-next-note-pos pos)))
      (setq jnote (get-text-property pos 'jnote))
      (if (string-equal (JNOTE-id jnote) id)
          (setq done t)
        (setq pos (next-single-property-change pos 'jnote))))
    pos))

(defun joplin-search-toggle-id ()
  (interactive)
  (let ((val (alist-get 'id joplin-visible-fields)) noteid)
    (setf (alist-get 'id joplin-visible-fields) (not val))

    (let ((note (joplin--search-note-from-overlay)))
      (when note
        (setq noteid (JNOTE-id note))))

    (joplin--search-rerender)

    (let ((pos (joplin--search-note-position noteid)))
      (when pos
        (goto-char pos)))))

(defun joplin-search-toggle-created-time ()
  (interactive)
  (let ((val (alist-get 'created_time joplin-visible-fields)) noteid)
    (setf (alist-get 'created_time joplin-visible-fields) (not val))

    (let ((note (joplin--search-note-from-overlay)))
      (when note
        (setq noteid (JNOTE-id note))))

    (joplin--search-rerender)

    (let ((pos (joplin--search-note-position noteid)))
      (when pos
        (goto-char pos)))))

(defun joplin-search-toggle-updated-time ()
  (interactive)
  (let ((val (alist-get 'updated_time joplin-visible-fields)) noteid)
    (setf (alist-get 'updated_time joplin-visible-fields) (not val))

    (let ((note (joplin--search-note-from-overlay)))
      (when note
        (setq noteid (JNOTE-id note))))

    (joplin--search-rerender)

    (let ((pos (joplin--search-note-position noteid)))
      (when pos
        (goto-char pos)))))

(defun joplin--search-rerender ()
  ;; render the buffer using `joplin-notes' list
  (let ((inhibit-read-only t))
    (erase-buffer)
    (set-marker joplin-eob-marker (point-min))

    (save-restriction
      (widen)
      (goto-char (point-max))
      (cl-dolist (note joplin-notes)
        ;; render
        (joplin--render-note note)))))

(defun joplin--search-load ()
  ;; render the buffer & fill buffer local data
  (let ((inhibit-read-only t)
        note)
    (when joplin-search-iter
      (if (>= joplin-search-count joplin-search-limit)
          (setq joplin-search-limit
                (+ joplin-search-limit joplin-limit-per-search)))
      (save-restriction
        (widen)
        (goto-char (point-max))
        (let (l)
          (condition-case x
              (while (< joplin-search-count joplin-search-limit)
                (setq note (iter-next joplin-search-iter))
                (setq l (cons note l))

                ;; render
                (joplin--render-note note)

                (setq joplin-search-count (1+ joplin-search-count)))
            (iter-end-of-sequence
             (setq joplin-search-iter nil)))

          (setq joplin-notes (append joplin-notes (nreverse l)))
          ))
      (message "%d notes found(s). %s" joplin-search-count
               (if joplin-search-iter "--more" "")))))


;; (defun joplin--search-norm-point (&optional arg)
(defun joplin-search-point-to-title (&optional arg)
  "Move the point to the beginning of the note title"
  ;; Assumes that note title does not starts at the beginning of the line
  (let ((bol (line-beginning-position))
        (eol (line-end-position))
        pos)
    ;; TODO: use text-property-search-forward?
    ;; NO I don't think so, it looks bulkier than next-single-property-change
    (if (get-text-property bol 'jnote)
        (goto-char bol)
      (setq pos (next-single-property-change bol 'jnote nil eol))
      (if pos
          (goto-char pos)))))

(defun joplin-search-next-line (&optional arg)
  "Move cursor vertically down ARG lines.

Similar to \\[next-line], tuned to JoplinSearch buffer"
  (interactive "p")
  (or arg (setq arg 1))

  ;; check if there's more note to load
  ;; TODO: async processing would be nice
  (when (and joplin-search-iter
             ;; below may not correct if narrow-to-region is in effect.
             (pos-visible-in-window-p (point-max)))
    (save-excursion
      (joplin--search-load)))

  (forward-line arg)
  (joplin-search-point-to-title)
  )

(defun joplin-search-previous-line (&optional arg)
  "Move cursor vertically up ARG lines.

Similar to \\[previous-line], tuned to JoplinSearch buffer"
  (interactive "p")
  (or arg (setq arg 1))
  (forward-line (- arg))
  (joplin-search-point-to-title))

(defun joplin-search-debug (&optional arg)
  (interactive)
  (message "limit(%d) count(%d/%d) iter(%s)"
           joplin-limit-per-search
           joplin-search-count
           joplin-search-limit
           (if joplin-search-iter "t" "nil")))

;;(add-hook 'markdown-mode-hook 'joplin-note-mode)

;;(defun joplin-resource-post (file)
;;  (when (file-readable-p file)




(provide 'joplin-mode)

;;; joplin-mode.el ends here
