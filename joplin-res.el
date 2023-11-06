;;; joplin-res.el --- Joplin resource                -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: data, comm, tools

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
(require 'joplin-gen)


(defvar joplin-resource-directory (expand-file-name "~/.joplin-cache"))
(defvar joplin-image-mimes '(("image/svg+xml" . svg)
                             ("image/png" . png)
                             ("image/gif" . gif)
                             ("image/tiff" . tiff)
                             ("image/jpeg" . jpeg)))

(defun joplin-resource-to-emacs-type (res)
  (alist-get (JRES-mime res) joplin-image-mimes nil nil #'equal))

(defun joplin-resource-supported-p (res)
  (let ((type (alist-get (JRES-mime res) joplin-image-mimes nil nil #'equal)))
    (if (memq type image-types) t)))

(defun joplin--http-get-resource-file (res &optional context)
  (let ((url-proxy-services joplin-url-proxy)
        (newname (concat (file-name-as-directory joplin-resource-directory)
                          (JRES-id res))))
    (if (file-exists-p newname)
        (progn
          (setf (JRES-_cachedfile res) newname)
          (set-file-times newname))
      (url-copy-file (joplin--build-url (concat "/resources/"
                                             (JRES-id res)
                                             "/file")
                                        (append context joplin-context))
                     newname 'ok-if-already-exists))
    newname))

(defun joplin-get-resource (rid)
  (let (res)
    (when (boundp 'joplin-resources)
      (let ((lst joplin-resources)
            r)
        (while (and (not res)
                    (setq r (pop lst)))
          (if (string-equal (JRES-id r) rid)
              (setq res r)))))
    (unless res
      (setq res (build-JRES (joplin--http-get (concat "/resources/" rid)
                                              '((fields . "id,title,mime,updated_time,filename,file_extension,size")))))
      (when (boundp 'joplin-resources)
        (push res joplin-resources)))
    res))


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


(defun joplin--resource-cache-remove-expired ()
  (let ((cont t)
        (count 0)
        files f)
    (when (file-directory-p joplin-resource-directory)
      (setq files (sort (directory-files joplin-resource-directory
                                         t "\\`[0-9a-fA-F]+\\'" t)
                        (lambda (a b) (file-newer-than-file-p b a))))
      (while (and cont (setq f (pop files)))
        (if (joplin-file-older-than-p f 7 'days)
            (progn (delete-file f)
                   (cl-incf count))
          (setq cont nil)))
      (joplin--error 'debug "cache: delete %d expired file(s)" count))))

(provide 'joplin-res)

;;; joplin-res.el ends here
