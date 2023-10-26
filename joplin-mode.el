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
;; variable `joplin-context'.  Also folder structure should be parsed
;; and stored in the variable `joplin-folders'.  These are done by
;; `joplin--init', and should be called on all entry points.  Usually
;; any user-exposed command for joplin-buffer, joplin-search-buffer,
;; joplin-note-buffer.

;;
;; Buffer local variables
;;
;;
;; --Joplin Note Buffer--
;; a note buffer retrived from JoplinApp alreadys has joplin-note
;; defined, but several functions need to work on vanilla markdown
;; buffer, which does not have any joplin-related buffer local
;; variables.
;;
;;  - joplin-note: a JNOTE struct
;;  - joplin-resources: list of JRES struct
;;  - joplin-resources-files: alist of (title-pathname . JRES)
;;
;;    title-pathname is (concat title
;;                              (char-to-string ?\u0000)
;;                              absolute-pathname).
;;
;;  Even if markdown link target shares the same pathname, if the
;;  title of the link is different, then joplin-mode treat them as
;;  different resources not as a same resource.

(defface joplin-folder-id-face
  '((t :inherit shadow))
  "Face for Joplin folder id"
  :group 'joplin-faces)
(defface joplin-folder-title-face
  '((t :inherit font-lock-string-face))
  "Face for Joplin Note title"
  :group 'joplin-faces)

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

(defvar joplin-no-unicode-symbol nil)

(defvar joplin-note)
(defvar joplin-temp-file)
(defvar joplin-source-file)
(defvar joplin-visible-fields)
(defvar joplin-eob-marker)
(defvar joplin-search-iter)
(defvar joplin-search-limit)
(defvar joplin-search-count)
(defvar joplin-search-func)
(defvar joplin-search-args)
(defvar joplin-search-type)

;;
;; Internal gloval variable.  Not expected to be overriden by users.
;;
(defvar joplin-toplev-buffer-name "*Joplin*")
(defvar joplin-search-buffer-name "*Jsearch*")
(defvar joplin-folder-buffer-format "*Jfolder:%s*")

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

(defconst joplin-folder-id-column 46)
(defconst joplin-folder-char #x01f4c1)
(defconst joplin-folder-symbol (if (and (not joplin-no-unicode-symbol)
                                        (char-displayable-p joplin-folder-char))
                                   (char-to-string joplin-folder-char)
                                 "[ ]"))


;; All note buffers will bound their `buffer-file-name' to
;; `joplin-temp-note-file'.  See the comment in `joplin-save-note'.
(defvar joplin-temp-note-file
  (let ((tmpfile (make-temp-file "joplin")))
    (add-to-list 'kill-emacs-hook 'joplin--cleanup)
    tmpfile))

(defun joplin--cleanup ()
  (delete-file joplin-temp-note-file))

(defun joplin--get-api-token ()
  "Get Joplin API token from JoplinApp"
  (condition-case e
      (let ((auth (alist-get 'auth_token (joplin--http-post "/auth" nil)))
            token)
        (while (not token)
          (read-from-minibuffer
           "Switch to JoplinApp, then accept authorization request [RET]: ")
          (setq token (alist-get 'token
                                 (joplin--http-get "/auth/check"
                                                   `((auth_token . ,auth))))))
        token)
    (error (joplin--error 'error "%s" e)
           ;; (signal (car e) (cdr e))  ; to re-rasise
           nil)))

(defun joplin--save-token (token)
  "Save JoplinApp token for later uses"
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


(defun joplin--build-idmap (folders)
  "Build an alist of (FOLDER-ID . JFOLDER) from FOLDERS

FOLDERS is a vector of alist of Joplin folder fields, available
from `joplin--load-folders'."
  (let (idmap)
    (dotimes (i (length folders))
      (let ((item (aref folders i)))
        (let ((fdr (make-JFOLDER :id (alist-get 'id item)
                                 :parent_id (alist-get 'parent_id item)
                                 :title (alist-get 'title item))
                   ))
          (setq idmap (cons (cons (alist-get 'id item) fdr)
                            idmap)))))
    idmap))

(defun joplin--build-pcmap (folders idmap)
  "Build an ALIST of (FOLDER-ID . (CHILD-JFOLDER ...)) from a vector, FOLDERS"
  (let (pcmap)
    (dotimes (i (length folders))
      (let* ((item (aref folders i))
             (parent (alist-get 'parent_id item))
             (record (assoc parent pcmap)))
        (if record
            (setcdr record
                    (nconc (cdr record)
                           (list (cdr (assoc (alist-get 'id item)
                                             idmap)))))
          (setq pcmap
                (nconc pcmap
                       (list (cons (alist-get 'parent_id item)
                                   (list (cdr (assoc (alist-get 'id item)
                                                     idmap)))
                                   )))))))
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
    (let ((n (build-JNOTE src)))
      (setf (JNOTE-_readall n) t)
      n)))

(defun joplin-note-buffer (id &optional buffer parent-buffer)
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
      (joplin-note-mode)
      (setq-local joplin-note note)
      (joplin--buffer-resources)
      ;;(message "note: %s" note)
      (set-buffer-modified-p nil)

      (setq-local buffer-file-name joplin-temp-note-file
                  joplin-parent-buffer parent-buffer
                  joplin-temp-file t
                  buffer-stale-function (lambda (&optional noconfirm) nil)
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
  (joplin--init)
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
        (define-key map [(?\r)] #'joplin-visit-folder)
        (define-key map [(?s)] #'joplin-search)
        (define-key map [(?/)] #'joplin-search)
        ;;(define-key map [(?c)] #'joplin-new-note)
        (define-key map [?q] #'(lambda ()
                                 (interactive)
                                 (quit-window 'kill)))
        map))

(defun joplin-point-at-folder ()
  "Move the point to the beginning of the folder title"
  (let ((bol (point))
        (eol (line-end-position))
        pos)
    (setq pos (next-single-property-change bol 'jfolder nil eol))
    (if pos
        (goto-char pos))))

(defun joplin-move-point-to-folder (fid)
  "Move point to the FOLDER line that has id, FID"
  (let ((pos (point-min)))
    (setq pos
          (save-excursion
            (cl-loop while (setq pos (next-single-property-change pos 'jfolder))
                     do
                     (goto-char pos)
                     (let ((folder (get-text-property pos 'jfolder)))
                       (if (and folder
                                (string-equal (JFOLDER-id folder) fid))
                           (cl-return pos))))))
    (if pos
        (goto-char pos))))


(defun joplin--switch-or-pop-to-buffer (buffer &optional switch)
  "If SWITCH is non-nil, `switch-to-buffer'. Otherwise `pop-to-buffer'."
  (if switch
      (switch-to-buffer buf)
    (setq win (get-buffer-window buffer))
    (if win
        (select-window win)
      (pop-to-buffer buffer))))

(defun joplin-jump-to-parent (&optional arg)
  (interactive "P")
  (cond ((eq major-mode 'joplin-search-mode)
         ;; goto toplev buffer
         (let ((note (joplin--search-note-at-point))
               (buf (joplin--buffer))
               fid win)
           (and note (setq fid (JNOTE-parent_id note)))
           (joplin--switch-or-pop-to-buffer buf arg)
           (if fid
               (with-current-buffer buf
                 (joplin-move-point-to-folder fid)))))

        ((and (boundp 'joplin-note-mode) joplin-note-mode)
         ;; goto either search or notebook buffer
         (let (nid fid)
           (if (and (boundp 'joplin-note) joplin-note)
               (setq nid (JNOTE-id joplin-note)
                     fid (JNOTE-parent_id joplin-note)))
           (if (and (boundp 'joplin-parent-buffer)
                    (buffer-live-p joplin-parent-buffer))
               (let ((parent joplin-parent-buffer))
                 (joplin--switch-or-pop-to-buffer parent arg)
                 (if nid
                     (with-current-buffer parent
                       (joplin-search-move-point-to-note nid))))
             ;; no parent buffer; must be registered recently.
             ;; try to switch to the folder buffer, if any.
             (if fid
                 (joplin-folder fidarg)))))

        ))

(defun joplin-next-folder (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (forward-line arg)
  (joplin-point-at-folder))

(defun joplin-previous-folder (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (forward-line (- arg))
  (joplin-point-at-folder))

(defun joplin-show-folder-properties ()
  (interactive)
  (let ((folder (joplin--folder-at-point)))
    (if folder
        ;; TODO: display more information?
        (message "%s: %s" (JFOLDER-id folder) (JFOLDER-title folder))
      (message "no folder found in the current position"))))

(defun joplin--buffer ()
  "Return the buffer of top-level joplin buffer.  If not, create it."
  (unless (get-buffer joplin-toplev-buffer-name)
    (joplin--render-joplin-buffer)
    (with-current-buffer (get-buffer joplin-toplev-buffer-name)
      (goto-char (point-min))))
  (get-buffer joplin-toplev-buffer-name))


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

;;;###autoload
(defun joplin (&optional arg)
  (interactive "p")
  ;; TODO: get token, then set `joplin-context'.
  (joplin--init)
  (switch-to-buffer (joplin--buffer)))

(defun joplin--render-joplin-buffer ()
  (or joplin-folders
      (joplin--init-folders))
  (with-current-buffer (get-buffer-create joplin-toplev-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; TODO: set the major mode here
      (joplin-mode)
      (let ((mkr (point-min-marker)))
        (set-marker-insertion-type mkr nil)

        (joplin--walk-folders
         (lambda (folder lev)
           (let ((indent (make-string (* lev 4) ?\ ))
                 fields text)
             (push indent fields)

             (push joplin-folder-symbol fields)

             (push (propertize (JFOLDER-title folder)
                               'face 'joplin-folder-title-face
                               'jfield 'title
                               'jfolder folder)
                   fields)

             (let ((col 0))
               (mapc (lambda (s) (cl-incf col (1+ (length s))))
                     fields)
               (push (if (< col joplin-folder-id-column)
                         (make-string (- joplin-folder-id-column col) ?\s) " ")
                     fields))

             (push (propertize (JFOLDER-id folder)
                               'face 'joplin-folder-id-face
                               'jfield 'id)
                   fields)

             (setq text (string-join (nreverse fields) " "))

             (insert text)
             (insert "\n")
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

;; Why bother to bound the note buffer's file name to the temporary
;; file name?
;;
;; I want to override the default behavior of save-buffer for joplin
;; note buffer so that I just use default emacs save action to update
;; the buffer contents in JoplinApp.
;;
;; I was able to do that by installing my own function in the local
;; variable, `write-file-functions', but it only works when the
;; `buffer-file-name' is actually bound to a file.  Otherwise, it will
;; ask the user to provide the filename which is not what I want.
;;
;; This create multiple problems at the moment.
;; (1) Emacs still believes that the joplin note buffer is bound to
;; the original file name, which is not what I want.  Perhaps I should
;; override `buffer-file-truename' as well?
;; (2) sometimes, Emacs belives that the temp file was modified so
;; that asking to revert the buffer, which is not what I want. I was
;; experiment with `buffer-stale-function' and
;; `set-visited-file-modtime' and it seems that it resolved.  But I
;; haven't figured which was actually solving the problem so I left both
;; of them.
;; (3) Ideally, once the buffer registered to JoplinApp, it should not
;; have any relation with any file, but the user can use Emacs default
;; saving action to update the buffer to Joplin.  I need a help on this.
(defun joplin-save-note (&optional arg)
  (interactive "p")
  (joplin--init)

  (unless (and (boundp 'joplin-temp-file)
               joplin-temp-file)
    ;; This means the buffer was not originally created by
    ;; joplin-mode.  We do not know whether the user want to keep the
    ;; file in the file system or not.  So, it's better to save it
    ;; first, then upload it to JoplinApp.
    (save-buffer))

  (joplin-resource-upload-all)

  (if (boundp 'joplin-note)
      (joplin--update-note)
    (let ((title (read-from-minibuffer "Note title: "))
          (folder (joplin--completing-folder-name "Folder: ")))
      (joplin--register-note title folder)

      (setq-local joplin-source-file buffer-file-name
                  buffer-file-name joplin-temp-note-file
                  joplin-temp-file t
                  buffer-stale-function (lambda (&optional noconfirm) nil)
                  write-file-functions '(joplin-save-note))

      (rename-buffer (joplin--note-buffer-name joplin-note))
      (set-buffer-modified-p nil)
      (set-visited-file-modtime)
      (JNOTE-id joplin-note)))

  ;; Reminder.  This function should return non-nil as it is part of
  ;; variable `write-file-functions'.  Since `joplin--update-note' and
  ;; `joplin--register-note' both return note id, it should work.
  t)

;;;###autoload
(define-minor-mode joplin-note-mode
  "a minor mode for Jopline note"
  :lighter "JPL"
  :keymap
  '(([(control ?c) ?j ?i] . joplin-show-properties)
    ([(control ?c) ?j ?j] . joplin-jump-to-parent)
    ([(control ?c) ?j ?s] . joplin-save-note)
    ([(control ?c) ?j ?l] . joplin-resource-upload-at-point)
    ([(control ?c) ?j ?L] . joplin-resource-upload-all)
    ([(control ?c) ?j ?r] . joplin-note-list-resources)
    ))


;;
;; Search
;;
(define-derived-mode joplin-search-mode special-mode "JoplinSearch"
  "docstring..."

  (make-local-variable 'joplin-notes)
  (make-local-variable 'joplin-visible-fields)
  (make-local-variable 'joplin-search-limit)
  (make-local-variable 'joplin-search-done)
  (make-local-variable 'joplin-search-func)
  (make-local-variable 'joplin-search-args)
  (make-local-variable 'joplin-search-type) ; search or folder

  (toggle-truncate-lines 1)
  (hl-line-mode)
  (setq-local buffer-read-only t
              joplin-notes ()
              joplin-visible-fields (copy-alist '((id . t)
                                                  (created_time . nil)
                                                  (updated_time . t)
                                                  (title . t)))
              joplin-eob-marker (point-min-marker)
              joplin-search-iter nil
              joplin-search-count 0
              joplin-search-limit joplin-limit-per-search)

  ;; the function preparing the buffer should initialize
  ;; `joplin-search-func', `joplin-search-args' and `joplin-search-type'.
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
        (define-key map [?^] #'joplin-jump-to-parent)
        (define-key map [(control ?c) (control ?j)] #'joplin-jump-to-parent)
        (define-key map [(control ?c) ?j ?j] #'joplin-jump-to-parent)

        (define-key map [?S ?o] #'joplin-search-sort-notes-by-order)
        (define-key map [?S ?i] #'joplin-search-sort-notes-by-id)
        (define-key map [?S ?t] #'joplin-search-sort-notes-by-title)
        (define-key map [?S ?u] #'joplin-search-sort-notes-by-updated_time)
        (define-key map [?S ?c] #'joplin-search-sort-notes-by-created_time)

        (define-key map [?q] #'quit-window)
        ;;(define-key map [?o] #'joplin-search-visit-note-other-window)
        (define-key map [?\r] #'joplin-search-visit-note)

        (define-key map [?i] #'joplin-search-note-info)

        (define-key map [?m] #'joplin-search-mark)
        (define-key map [?u] #'joplin-search-unmark)
        (define-key map [?U] #'joplin-search-unmark-all)
        (define-key map [?d] #'joplin-search-flag-note-for-delete)

        (define-key map [?* ?c] #'joplin-search-change-marks)
        (define-key map [?* ?d] #'joplin-search-mark-notes-regexp)
        (define-key map [?% ?m] #'joplin-search-mark-notes-regexp)
        (define-key map [?% ?d] #'joplin-search-flag-notes-regexp)

        (define-key map [?t] #'joplin-search-toggle-marks)

        (define-key map [?M] #'joplin-search-move-notes)
        (define-key map [?x] #'joplin-search-delete-notes)
        map))


(defun joplin--note-buffer-name (note)
  (format "*JoplinNote:%s*" (JNOTE-id note)))

(defun joplin-search-visit-note (&optional arg)
  (interactive "P")
  ;; (joplin-note-buffer "id")
  (let* ((note (joplin--search-note-at-point))
         (bufname (joplin--note-buffer-name note))
         (parent (current-buffer)))
    (when note
      (let ((buf (get-buffer bufname)))
        (if buf
            (with-current-buffer buf
              (setq-local joplin-parent-buffer parent))
          (setq buf (get-buffer-create bufname))
          (joplin-note-buffer (JNOTE-id note) buf parent))
        (if arg
            (pop-to-buffer buf)
          (switch-to-buffer buf))))))

(defun joplin-search-visit-note-other-window (&optional arg)
  (interactive "P")
  (joplin-search-visit-note arg))

(defun joplin-clear-search (&optional arg)
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (set-marker joplin-eob-marker (point-min))
    (setq joplin-notes ()
          joplin-search-iter nil
          joplin-search-count 0
          joplin-search-limit joplin-limit-per-search
          joplin-search-func nil
          joplin-search-args nil)))

(defun joplin--search-buffer ()
  "Return Joplin Search buffer, create one if none exists."
  (let ((buf (get-buffer-create joplin-search-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'joplin-search-mode)
        (joplin-search-mode)
        (setq joplin-search-type 'search))
      (current-buffer))))

(defun joplin--folder-buffer (fid)
  (let ((folder (alist-get fid joplin-folders nil nil #'equal))
        bufname)
    (setq bufname (format joplin-folder-buffer-format (JFOLDER-title folder)))
    (let ((buf (get-buffer-create bufname)))
      (with-current-buffer buf
        (unless (eq major-mode 'joplin-search-mode)
          (joplin-search-mode)
          (setq joplin-search-type 'folder))
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
                joplin-search-func #'joplin--search-notes-by-text
                joplin-search-args (list text))
          (joplin--search-load)
          (goto-char (point-min))
          (joplin-search-point-to-title))))
    (pop-to-buffer buf)))


(defun joplin-visit-folder (&optional arg)
  (interactive "P")
  (let ((folder (joplin--folder-at-point)))
    (when folder
      (joplin-folder (JFOLDER-id folder)))))

(defun joplin-folder (&optional fid switch)
  (interactive (progn
                 (joplin--init)
                 (list (joplin--completing-folder-name "Notebook: "))))

  (let ((buf (joplin--folder-buffer fid)))
    (with-current-buffer buf
      (unless (and (boundp 'joplin-search-func) joplin-search-func)
        ;; fill the folder buffer if it looks new and empty.
        (let ((iter (joplin--folder-notes fid)))
          (joplin-clear-search)
          (setq joplin-search-iter iter
                joplin-search-func #'joplin--folder-notes
                joplin-search-args (list fid))
          (joplin--search-load)
          (goto-char (point-min))
          (joplin-search-point-to-title))))
    (joplin--switch-or-pop-to-buffer buf switch)))

(defun joplin-search-revert (&optional arg)
  "Replace current search buffer text with JoplinApp search result."
  (interactive "p")

  (let (noteid)
    (let ((note (joplin--search-note-at-point)))
      (when note
        (setq noteid (JNOTE-id note))))

    (let ((iter (apply joplin-search-func joplin-search-args)))
      (setq joplin-notes ()
            joplin-search-iter iter
            joplin-search-count 0)

      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-marker joplin-eob-marker (point-min)))

      (joplin--search-load))

    (let ((pos (joplin--search-note-position noteid)))
      (when pos
        (goto-char pos)))))

(defun joplin--render-note (note &optional update)
  (let (text items)
    (set-marker joplin-eob-marker (point))

    (if update
        (delete-region (line-beginning-position) (line-end-position)))

    (let ((m (JNOTE-_marked note)))
      (setq items (cons (format "%1s" (or m "")) items)))

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
    (insert (format "%s  %s" text
                    (propertize (JNOTE-title note)
                                'face 'joplin-note-title-face
                                'jfield 'title
                                'jnote note
                                )
                    ))
    (if update
        (let ((ol (joplin--search-overlay-at-point)))
          (overlay-put ol 'joplin-note note))
      (insert "\n")
      (let ((ol (make-overlay joplin-eob-marker (point))))
        (overlay-put ol 'joplin-note note)))

    (set-marker joplin-eob-marker (point))))


;; TODO: enable third-party minor modes may add additional overlay to
;; the buffer.  Should handle properly
(defun joplin--search-note-at-point ()
  ;; TODO: reuse `joplin--search-overlay-at-point'
  (let (note)
    (cl-loop for o in (overlays-at (point))
             when (setq note (overlay-get o 'joplin-note)) return note)))

(defun joplin--search-overlay-at-point ()
  (let (ol)
    (cl-loop for o in (overlays-at (point))
             when (setq ol (when (overlay-get o 'joplin-note) o)) return ol)))


(defun joplin--folder-at-point ()
  ;; TODO: reuse `joplin--search-overlay-at-point'
  (let (folder)
    (cl-loop for o in (overlays-at (point))
             when (setq folder (overlay-get o 'joplin-folder)) return folder)))

(defun joplin--folder-overlay-at-point ()
  (let (ol)
    (cl-loop for o in (overlays-at (point))
             when (setq ol (when (overlay-get o 'joplin-folder) o)) return ol)))


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

(defun joplin-search-move-point-to-note (nid)
  (let ((pos (joplin--search-note-position nid)))
    (when pos
      (goto-char pos))))

(defun joplin-search-toggle-id ()
  (interactive)
  (let ((val (alist-get 'id joplin-visible-fields)) noteid)
    (setf (alist-get 'id joplin-visible-fields) (not val))

    (let ((note (joplin--search-note-at-point)))
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

    (let ((note (joplin--search-note-at-point)))
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

    (let ((note (joplin--search-note-at-point)))
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

(defun joplin-search-next-line (arg &optional interactive)
  "Move cursor vertically down ARG lines.

Similar to \\[next-line], tuned to JoplinSearch buffer"
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (or arg (setq arg 1))

  (joplin--error 'debug "(joplin-search-next-line %d)" arg)
  (when interactive
    ;; check if there's more note to load
    ;; TODO: async processing would be nice
    (when (and joplin-search-iter
               ;; below may not correct if narrow-to-region is in effect.
               (pos-visible-in-window-p (point-max)))
      (save-excursion
        (joplin--search-load))))

  (let ((ret (forward-line arg)))
    (joplin-search-point-to-title)
    ret))

(defun joplin-search-previous-line (&optional arg)
  "Move cursor vertically up ARG lines.

Similar to \\[previous-line], tuned to JoplinSearch buffer"
  (interactive "p")
  (or arg (setq arg 1))

  ;; TODO: check return value to align with next-line function
  (let ((ret (forward-line (- arg))))
    (joplin-search-point-to-title)
    ret))

(defun joplin-search-debug (&optional arg)
  (interactive)
  (message "limit(%d) count(%d/%d) func(%S) args(%S) iter(%s)"
           joplin-limit-per-search
           joplin-search-count
           joplin-search-limit
           joplin-search-func
           joplin-search-args
           (if joplin-search-iter "t" "nil")))

;; (defun joplin-search-mark (arg &optional interactive)
;;   (interactive (list current-prefix-arg t))
;;   (cond ((and interactive (use-region-p))
;;          (save-excursion
;;            (let ((beg (region-beginning))
;;                  (end (region-end)))
;;              (joplin-search-mark-notes-in-region
;;               (progn (goto-char beg) (line-beginning-position))
;;               (progn (goto-char end) (line-beginning-position))))))
;;         (t
;;          (let ((inhibit-read-only t))
;;            (joplin-repeat-over-lines
;;             (prefix-numeric-value arg)
;;             (lambda () (delete-char 1) (insert dired-marked-char)))))))



;;(add-hook 'markdown-mode-hook 'joplin-note-mode)

;;(defun joplin-resource-post (file)
;;  (when (file-readable-p file)

;; (defun joplin-search-repeat-over-notes (lines function)

(defun joplin-search-mark (arg &optional interactive)
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (or arg (setq arg 1))

  (dotimes (n arg)
    (joplin--search-mark-line "*")
    (joplin-search-next-line 1)))

(defun joplin-search-unmark (arg &optional interactive)
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (or arg (setq arg 1))

  (dotimes (n arg)
    (joplin--search-mark-line nil)
    (joplin-search-next-line 1)))

(defun joplin-search-toggle-marks (arg &optional interactive)
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (or arg (setq arg 1))

  (let ((cont t))
    (save-excursion
      ;; TODO: do I need `save-match-data'?
      (goto-char (point-min))

      (while cont
        (let ((note (joplin--search-note-at-point)))
          (when note
            (let ((mark (JNOTE-_marked note)))
              (joplin--search-mark-line (cond ((null mark) "*")
                                              ((string-equal mark "*") nil)
                                              (t mark))
                                        note))))
        (setq cont (= (joplin-search-next-line 1) 0))))))


(defun joplin-search-change-marks (&optional old new)
  (interactive
   (let* ((cursor-in-echo-area t)
          (old (progn (message "Change (old mark): ") (read-char)))
          (new (progn (message  "Change %c marks to (new mark): " old)
                      (read-char))))
     (list old new)))

  (setq old (make-string 1 old)
        new (make-string 1 new))

  (let ((cont t))
    (save-excursion
      ;; TODO: do I need `save-match-data'?
      (goto-char (point-min))

      (while cont
        (let ((note (joplin--search-note-at-point)))
          (when note
            (let ((mark (JNOTE-_marked note)))
              (if (and mark (string-equal mark old))
                  (joplin--search-mark-line new note)))))
        (setq cont (= (joplin-search-next-line 1) 0))))))

(defun joplin-search-flag-note-for-delete (arg &optional interactive)
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (or arg (setq arg 1))

  (dotimes (n arg)
    (joplin--search-mark-line "D")
    (joplin-search-next-line 1)))


(defvar joplin-regexp-history nil)

(defun joplin-search-mark-notes-regexp (regexp &optional marker-char)
  (interactive
   (list (read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
                              " notes (regexp): ")
                      nil
                      'joplin-regexp-history)
         (if current-prefix-arg nil "*")))

  (let ((cont t))
    (save-excursion
      ;; TODO: do I need `save-match-data'?
      (goto-char (point-min))

      (while cont
        (let ((note (joplin--search-note-at-point)))
          (when note
            (if (string-match regexp (JNOTE-title note))
                (joplin--search-mark-line marker-char))))
        (setq cont (= (joplin-search-next-line 1) 0))))))

(defun joplin-search-flag-notes-regexp (regexp &optional marker-char)
  (interactive
   (list (read-regexp "Flag for deletion (regexp): " nil
                      'joplin-regexp-history)))
  (joplin-search-mark-notes-regexp regexp "D"))

(defun joplin-search-unmark-all (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))

  (save-excursion
    (goto-char (point-min))
    ;; below loop calls (joplin--search-mark-line) one more time
    ;; around end of the buffer, but no harm done.
    (joplin--search-mark-line nil)
    (while (= (joplin-search-next-line 1) 0)
      (joplin--search-mark-line nil))))

(defun joplin--search-delete-line ()
  ;; `delete-line' is not available in Emacs 27.1
  (delete-region (line-beginning-position) (line-beginning-position 2)))

(defun joplin--search-mark-line (markchar &optional note)
  "Mark the note in the current line with MARKCHAR.

To unset the mark, pass nil instead of \" \".
If you already know JNOTE struct of the current line, pass it
as NOTE to speed up."
  ;; to unset, use nil rather than " ".
  ;; No save-excursion. No inhibit-read-only
  (let ((n (or note (joplin--search-note-at-point))))
    (when n
      (let ((inhibit-read-only t))
        (beginning-of-line)
        (when (re-search-forward "^." (line-end-position) t 1)
          (replace-match (or markchar " ")))
        (setf (JNOTE-_marked n) markchar)))))

(defun joplin--remove-matched-notes (set objs)
  "Remove note struct from SET where it also appears in OBJS

The ordering in SET should be identical to that of OBJS.  For example,
(joplin--remove-matched '(a b c d) '(b c)) return '(a d)."
  (let (newlst srst drst)
    (while set
      (if (not objs)
          (setq newlst (append (reverse set) newlst)
                set nil)
        (if (string-equal (JNOTE-id (car set)) (JNOTE-id (car objs)))
            (setq set (cdr set)
                  objs (cdr objs))
          (setq newlst (cons (car set) newlst)
                set (cdr set)))))
    (nreverse newlst)))

(defun joplin--search-do-marked (func &optional markchar)
  ;; call FUNC on each marked note in the buffer.
  ;;
  ;; FUNC will receive one argument, note struct.
  ;; if it returns non-nil, the line will be removed from the buffer.
  ;; MARKCHAR is a single char string.
  (let ((inhibit-read-only t)
        (cont t)
        (chstr (or markchar "*"))
        rem lst)
    (save-excursion
      ;; TODO: do I need `save-match-data'?
      (goto-char (point-min))

      (while cont
        (let ((note (joplin--search-note-at-point)))
          (when note
            (let ((mark (JNOTE-_marked note)))
              (when (and mark (string-equal mark chstr))
                (joplin--error 'debug "run PROC (note:%s)" (JNOTE-id note))
                (setq lst (cons note lst))
                (if (setq rem (funcall func note))
                    (joplin--search-delete-line))
                ))))
        (if rem
            (setq rem nil)
          (setq cont (= (joplin-search-next-line 1) 0)))))
    (nreverse lst)))


(defun joplin--search-mapcar (func)
  ;; call FUNC on each marked note in the buffer.
  ;;
  ;; FUNC will receive one argument, note struct.
  ;; if it returns non-nil, the line will be removed from the buffer.
  (let ((cont t) ret lst)
    (save-excursion
      ;; TODO: do I need `save-match-data'?
      (goto-char (point-min))

      (while cont
        (let ((note (joplin--search-note-at-point)))
          (when note
            (setq ret (funcall func note)
                  lst (cons ret lst))))
        (setq cont (= (joplin-search-next-line 1) 0))))
    (nreverse lst)))

(defun joplin--move-note (note-id folder-id)
  (let ((resp (joplin--http-put (concat "/notes/" note-id)
                                `((parent_id . ,folder-id)))))
    (build-JNOTE resp)))

(defun joplin--delete-note (note-id)
  (joplin--http-del (concat "/notes/" note-id)))


(defun joplin-search-note-info ()
  (interactive)
  (let ((note (joplin--search-note-at-point)))
    (when note
      (message "%s\n_mark [%s], _readall[%s], _tags[%s], order[%s]
id: %s\nparent_id: %s [%s]
created_time: %s\nupdated_time: %s"
               (JNOTE-title note)
               (JNOTE-_marked note) (JNOTE-_readall note) (JNOTE-_tags note) (JNOTE-order note)
               (JNOTE-id note)
               (JNOTE-parent_id note) (joplin--folder-title (JNOTE-parent_id note))
               (JNOTE-created_time note)
               (JNOTE-updated_time note)))))


(defun joplin--short-id (id)
  (and id (substring id 0 7)))

(defmacro joplin--folder-title (id)
  `(let ((f (cdr (assoc ,id joplin-folders))))
     (if f
         (JFOLDER-title f))))

(defun joplin-search-marked-count ()
  (with-current-buffer (get-buffer "*JoplinSearch*")
    (let ((count 0))
      (joplin--search-do-marked
       (lambda (n)
         (setq count (1+ count))
         nil))
      count)))

(defun joplin-search-flaged-count ()
  (with-current-buffer (get-buffer "*JoplinSearch*")
    (let ((count 0))
      (joplin--search-do-marked
       (lambda (n)
         (setq count (1+ count))
         nil)
       "D")
      count)))

(defun joplin--search-update-joplin-notes (updated)
  "Update `joplin-notes' with updated elements in UPDATED.

UPDATED is a list of JNOTE struct, and the order of elements in
UPDATED should be identical to that of `joplin-notes'.  Missing
elements are okay.

Note that this function will destructively rebuild `joplin-notes'."
  (let ((lst (cons nil joplin-notes)) hdr src n)
    (setq hdr lst)
    (setq n (pop updated))
    (while (setq src (cadr lst))
      (when (string-equal (JNOTE-id src) (JNOTE-id n))
        (setcdr lst (cons n (cddr lst)))
        (setq n (pop updated)))
      (setq lst (cdr lst))
      (if (null n)
          (setq lst nil)))
    (setq joplin-notes (cdr hdr))))


(defmacro joplin--search-sort (arg cmp slot)
  `(let ((n (joplin--search-note-at-point))
         id)
     (if n (setq id (JNOTE-id n)))

     (setq joplin-notes
           (sort joplin-notes (lambda (a b)
                                (funcall ,cmp
                                         (funcall ,slot a)
                                         (funcall ,slot b)))))

     (if ,arg
         (setq joplin-notes (nreverse joplin-notes)))

     (joplin--search-rerender)
     (and id
          (joplin-search-goto-note id))))

(defun joplin-search-sort-notes-by-id (&optional arg)
  (interactive "P")
  (joplin--search-sort arg #'string-lessp #'JNOTE-id))

(defun joplin-search-sort-notes-by-title (&optional arg)
  (interactive "P")
  (joplin--search-sort arg
                       (lambda (a b)
                         (< (compare-strings a nil nil b nil nil t) 0))
                       #'JNOTE-title))

(defun joplin-search-sort-notes-by-updated_time (&optional arg)
  (interactive "P")
  (joplin--search-sort arg #'< #'JNOTE-updated_time))

(defun joplin-search-sort-notes-by-created_time (&optional arg)
  (interactive "P")
  (joplin--search-sort arg #'< #'JNOTE-updated_time))

(defun joplin-search-sort-notes-by-order (&optional arg)
  (interactive "P")
  ;; (joplin--search-sort arg #'> #'JNOTE-order)
  (let ((n (joplin--search-note-at-point))
        id)
    (if n (setq id (JNOTE-id n)))

    (setq joplin-notes
          (sort joplin-notes (lambda (a b)
                               (let ((ret (- (JNOTE-order a) (JNOTE-order b))))
                                 (if (= ret 0)
                                     (> (JNOTE-updated_time a)
                                        (JNOTE-updated_time b))
                                   (> ret 0))))))

    (if arg
        (setq joplin-notes (nreverse joplin-notes)))

    (joplin--search-rerender)
    (and id
         (joplin-search-goto-note id))
    ))

(defun joplin-search-delete-notes ()
  (interactive)
  (let ((count 0)
        (total (joplin-search-flaged-count)))
    (if (= total 0)
        (message "No note flagged for deletion")
      (joplin--search-do-marked (lambda (n)
                                  (message "delete note(s) %d/%d..."
                                           (setq count (1+ count)) total)
                                  (joplin--delete-note (JNOTE-id n))
                                  t)
                                "D")
      (message "%d note(s) deleted" count))))

(defun joplin-search-move-notes (folder-id)
  (interactive (list (joplin--completing-folder-name
                      "Select destination folder: ")))

  (let ((count 0)
        (total (joplin-search-marked-count))
        moved)
    (cl-flet ((mov (n)
                   (let (note)
                     (setq note (joplin--move-note (JNOTE-id n) folder-id))
                     (message "move note(s) %d/%d..."
                              (setq count (1+ count))
                              total)
                     (joplin--render-note note 'update)
                     (push note moved)
                     nil)))
      (if (> total 0)
          (joplin--search-do-marked #'mov)
        (let ((inhibit-read-only t))
          (mov (joplin--search-note-at-point)))))

    (joplin--search-update-joplin-notes (nreverse moved))
    (message "%d note(s) moved" count)))

(defun joplin-search-goto-note (id)
  (let ((cont t) found)
    (widen)
    (push-mark)
    (goto-char (point-min))
    (while cont
      (let ((n (joplin--search-note-at-point))
            nid)
        (if n (setq nid (JNOTE-id n)))
        (if (string-equal id nid)
            (setq cont nil
                  found t)
          (setq cont (= (forward-line 1) 0)))))
    (if found
        (joplin-search-point-to-title)
      (goto-char (mark))
      (pop-mark))))


(defun joplin--buffer-resources (&optional buffer)
  "Return the list of resources (JRES struct) of the note buffer.

If the buffer local variable, `joplin-resources' is defined, this
function simply returns it.  Otherwise, it will retrieve resources
from JoplinApp."
  (or buffer
      (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (if (local-variable-p 'joplin-resources)
        joplin-resources

      (when (local-variable-p 'joplin-note)
        (let ((iter (joplin--note-resources (JNOTE-id joplin-note)))
              resmap)
          ;; Read from JoplinApp for all resources belongs to this note
          (while iter
            (condition-case x
                (let ((res (iter-next iter)))
                  (if res
                      (push (cons (JRES-id res) res) resmap)))
              (iter-end-of-sequence (setq iter nil))))
          ;; Scan the buffer to mark the embedded resources
          (save-excursion
            (save-restriction
              ;; TODO: save-match-data?
              (widen)
              (goto-char (point-min))
              (while (re-search-forward
                      "!\\[\\([^]]*\\)\\](\\(:/\\([^)]*\\)\\))" nil t)
                (let* ((kv (assoc (match-string-no-properties 3) resmap))
                       (r (cdr kv)))
                  (if r
                      (setf (JRES-_embeded r) t))))))

          (setq-local joplin-resources
                      (mapcan (lambda (kv) (list (cdr kv))) resmap)))))))

(defun joplin--register-resources (filename &optional title)
  "Register new resource from FILENAME with optional TITLE.

Returns new JRES struct of the resource."
  (unless (file-readable-p filename)
    (error "joplin: cannot read file %s" filename))
  (or title
      (setq title ""))
  (let ((resp (joplin--http-post-attachment-url (list (cons 'title title))
                                                filename)))
    (build-JRES resp t)))

(defconst joplin--markdown-regex-link-inline
  ;; Stealed from markdown-mode. Joplin may need to work without
  ;; markdown-mode so I had to steal it.
  "\\(?1:!\\)?\\(?2:\\[\\)\\(?3:\\^?\\(?:\\\\\\]\\|[^]]\\)*\\|\\)\\(?4:\\]\\)\\(?5:(\\)\\s-*\\(?6:[^)]*?\\)\\(?:\\s-+\\(?7:\"[^\"]*\"\\)\\)?\\s-*\\(?8:)\\)"
  "Regular expression for a [text](file) or an image link ![text](file).
Group 1 matches the leading exclamation point (optional).
Group 2 matches the opening square bracket.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket.
Group 5 matches the opening parenthesis.
Group 6 matches the URL.
Group 7 matches the title (optional).
Group 8 matches the closing parenthesis.")

(defun joplin--markdown-link-at-point ()
  "Return pos if the point is on a markdown link text, otherwise nil.

You can use match data if this function returns non-nil.  This function
will clobber the match data."
  (save-excursion
    (let ((old (point)) pos)
      (beginning-of-line)
      (cl-loop while (setq pos
                           (re-search-forward joplin--markdown-regex-link-inline (line-end-position) t))
               if (and (<= (match-beginning 0) old)
                       (<= old (match-end 0))) return pos))))


(defun joplin--note-do-resource (file &optional title)
  (save-match-data
    (or title (setq title ""))
    (unless (local-variable-p 'joplin-resources)
      (setq-local joplin-resources nil))
    (unless (local-variable-p 'joplin-resources-files)
      (setq-local joplin-resources-files nil))

    (let ((res-key (concat title (char-to-string ?\u0000)
                           (file-truename file))))
      (setq res (alist-get res-key joplin-resources-files nil nil #'equal))
      (unless res
        (setq res (joplin--register-resources file title))
        (joplin--error 'debug "resource registered: %S" res)
        (when res
          (push res joplin-resources)
          (push (cons res-key res) joplin-resources-files)))
      res)))


(defun joplin--local-resource-p (s &optional verbose)
  "Return t if the link S can be locally accessible.

Precisely, this function returns t if the link is regular file
pathname which it can access or URL with \"file:\" scheme.

Note that this function preserve match-data if any."
  (if (or (null s) (string-equal s ""))
      (and verbose (message "no link at point") nil)
    (save-match-data
      (if (string-match "^:/[0-9a-fA-F]+" s)
          (and verbose (message "skip Joplin link") nil)
        (let ((u (url-generic-parse-url s)))
          (if (or (null (url-type u)) (string-equal (url-type u) "file"))
              (let ((f (url-filename u)))
                (if (file-readable-p f)
                    t
                  (and verbose (message "link access denied") nil)))
            (and verbose (message "non-local link ignored") nil)))))))

(defun joplin-resource-upload-at-point (&optional arg)
  "Replace the markdown link target to JoplinApp target.

This may upload the link target as a resource to JoplinApp.
It returns the JRES struct for the resources."
  (interactive "p")
  (joplin--init)
  (save-excursion
    (let ((pos (joplin--markdown-link-at-point)) file title)
      (if pos (setq file (string-trim (match-string-no-properties 6))))
      (when (joplin--local-resource-p file 'verbose)
        (setq title (or (match-string-no-properties 7) ""))
        (let ((res (joplin--note-do-resource file title)))
          (replace-match (concat "\\1\\2\\3\\4\\5"
                                 (format ":/%s" (JRES-id res))
                                 (if (> (length title) 0)
                                     " " "")
                                 "\\7\\8")))))))

(defun joplin-resource-upload-all (&optional arg)
  (interactive "p")
  (joplin--init)
  (save-excursion
    (save-restriction
      (let (file title)
        (widen)
        (goto-char (point-min))
        (while (re-search-forward joplin--markdown-regex-link-inline nil t)
          (setq file (string-trim (match-string-no-properties 6))
                title (or (match-string-no-properties 7) ""))
          (when (joplin--local-resource-p file)
            (let ((res (joplin--note-do-resource file title)))
              (replace-match (concat "\\1\\2\\3\\4\\5"
                                     (format ":/%s" (JRES-id res))
                                     (if (> (length title) 0)
                                         " " "")
                                     "\\7\\8")))))))))

(defun joplin--get-resource (rid)
  "Return JRES struct retrive from JoplinApp"
  (let (resp)
    (setq resp (joplin--http-get (concat "/resources/" rid)
                                 '((fields . "id,title,mime,filename,created_time,updated_time,user_created_time,user_updated_time,file_extension,encryption_cipher_text,encryption_applied,encryption_blob_encrypted"))))
    (build-JRES resp)))


(define-derived-mode joplin-resources-mode tabulated-list-mode "Note Resources"
  "Major mode for listing resources"
  (setq tabulated-list-format [("id" 7 t)
                               ("mime" 24 t)
                               ("size" 8 joplin--note-list-res-size-p
                                . (:right-align t))
                               ("filename" 30 t)
                               ("title" 30 t)])
  ;;(setq tabulated-list-sort-key (cons "Process" nil))
  ;;(add-hook 'tabulated-list-revert-hook 'list-processes--refresh nil t))
  (setq-local joplin-parent-buffer nil)

  (let ((keymap joplin-resources-mode-map))
    (define-key keymap [(control ?c) (control ?j)] #'joplin--jump-to-note-buffer)
    (define-key keymap [(control ?c) ?j] #'joplin--jump-to-note-buffer))
  )

(defun joplin--note-list-res-size-p (e1 e2)
  (let ((r1 (car e1))
        (r2 (car e2)))
    (< (JRES-size r1) (JRES-size r2))))

(defun joplin-note-list-resources (&optional arg)
  (interactive "P")
  (let ((resbuf (get-buffer-create (format "*JoplinRes:%s*"
                                           (JNOTE-id joplin-note))))
        (notebuf (current-buffer)))
    (with-current-buffer resbuf
      (joplin-resources-mode)
      (setq-local joplin-parent-buffer notebuf)
      (joplin--note-list-resources-refresh notebuf)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (select-window (display-buffer (current-buffer))))))

(defsubst joplin--field-string (arg)
  (if (or (null arg) (string-equal arg ""))
      "--"
    arg))

(defun joplin--note-list-resources-refresh (&optional notebuf)
  (or notebuf
      (setq notebuf joplin-parent-buffer))
  (let ((reslst (joplin--buffer-resources notebuf))
        enties)
    (setq entries (mapcan
                   (lambda (res)
                     (list
                      (list
                       ;; (JRES-id res)
                       res
                       (vector (substring (JRES-id res) 0 7)
                               (joplin--field-string (JRES-mime res))
                               (file-size-human-readable (JRES-size res))
                               (joplin--field-string (JRES-filename res))
                               (joplin--field-string (JRES-title res))))))
                   reslst))
    (setq tabulated-list-entries entries)
    (tabulated-list-init-header)))

(defun joplin--jump-to-note-buffer (&optional arg)
  (interactive "P")
  (let ((notebuf joplin-parent-buffer)
        buf)
    (quit-window (not arg))
    (setq buf (window-buffer (selected-window)))
    (unless (eq buf notebuf)
      (if (buffer-live-p notebuf)
          (switch-to-buffer notebuf)))))



(provide 'joplin-mode)

;;; joplin-mode.el ends here
