;;; joplin-http.el --- Joplin request                -*- lexical-binding: t; -*-

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


;; To keep it simple, I prefer to have code that rely on 'plz in this file.

(require 'plz)

(defvar joplin-token-file ".joplin.token"
  "Filename for the Joplin API token.

This should not contain directory component as the filename
will be appended to the variable, `user-emacs-directory'.")

(defvar joplin-endpoint "http://127.0.0.1:41184")
(defvar joplin-curl-args '("--proxy" "http://172.22.144.1:8080")
  "Additional arguments to curl(1).  Useful for accessing Joplin through the proxy")

(defvar joplin-context nil)

(defun joplin--http-get (url &optional context)
  "GET method with CONTEXT (query-string)"
  (let ((plz-curl-default-args (append joplin-curl-args plz-curl-default-args)))
    (plz 'get
      (joplin--build-url url (append context joplin-context))
      :as #'json-read)))

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
      (joplin--error 'debug "joplin-build-url: %s" url)
      url)))


;; plz 0.7 does not support POST form(multipart/form-data) yet.
;; See https://github.com/alphapapa/plz.el/issues/22 for more.
;; Until then, upload attachments can not be implemented.

(provide 'joplin-http)
;;; joplin-http.el ends here
