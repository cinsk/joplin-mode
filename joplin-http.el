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

(when (locate-library "plz")
  (require 'plz))

(require 'joplin-http-post-simple)

(defvar joplin-token-file ".joplin.token"
  "Filename for the Joplin API token.

This should not contain directory component as the filename
will be appended to the variable, `user-emacs-directory'.")

(defvar joplin-endpoint "http://127.0.0.1:41184")

(defvar joplin-curl-args '()
  "Additional arguments to curl(1).  Useful for accessing Joplin through the proxy")
(defvar joplin-url-proxy nil
  "Proxy setting for Joplin.  See `url-proxy-services' for details")

(defvar joplin-context nil)

(defun joplin--http-get-plz (url &optional context)
  "GET method with CONTEXT (query-string)"
  (let ((plz-curl-default-args (append joplin-curl-args plz-curl-default-args)))
    (plz 'get
      (joplin--build-url url (append context joplin-context))
      :as #'json-read)))

(defun joplin--http-put-plz (url body &optional context)
  "PUT method with an alist BODY to URL with CONTEXT (query-string)"
  (let ((plz-curl-default-args (append joplin-curl-args plz-curl-default-args)))
    (plz 'put
      (joplin--build-url url (append context joplin-context))
      :as #'json-read
      :body (json-encode body)
      )))

(defun joplin--http-post-plz (url body &optional context)
  "PUT method with an alist BODY to URL with CONTEXT (query-string)"
  (let ((plz-curl-default-args (append joplin-curl-args plz-curl-default-args)))
    (plz 'post
      (joplin--build-url url (append context joplin-context))
      :as #'json-read
      :body (json-encode body)
      )))

(defun joplin--http-del-plz (url &optional context)
  "PUT method with an alist BODY to URL with CONTEXT (query-string)"
  (let ((plz-curl-default-args (append joplin-curl-args plz-curl-default-args)))
    (plz 'delete
      (joplin--build-url url (append context joplin-context))
      )))

(defun joplin--build-url (&optional path ctx)
  (let (qslst url)
    (dolist (p ctx)
      ;; TODO: for a key/value pair in query string,
      ;;       should I call `joplin--http-post-encode-string'?
      (setq qslst (cons (format "%s=%s" (car p) (cdr p)) qslst)))
    (let ((qs (mapconcat 'identity qslst "&")))
      (setq url (concat joplin-endpoint (or path "")
                        (if (string-equal qs "")
                            ""
                          (concat "?" qs))))
      url)))


;; plz 0.7 does not support POST form(multipart/form-data) yet.
;; See https://github.com/alphapapa/plz.el/issues/22 for more.
;; Until then, upload attachments can not be implemented.

(defun joplin-ping ()
  (let ((url-proxy-services '(("http" . "172.22.144.1:8080")))
        ;;(url-request-method "GET")
        )
    (condition-case x
        (with-temp-buffer
          (url-insert-file-contents "http://127.0.0.1:41184/ping")
          (buffer-string)
          )
      (error
       (signal (car x) (cdr x)))))
  )


(defun joplin--http-get-url (url &optional context)
  (let ((url-proxy-services joplin-url-proxy))
    (with-temp-buffer
      (joplin--error 'debug "HTTP GET %s" url)
      (url-insert-file-contents
       (joplin--build-url url (append context joplin-context)))
      (json-read))))

(defun joplin--http-post-url (url args &optional context)
  (let ((url-proxy-services joplin-url-proxy)
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (json-serialize args)))
    (with-temp-buffer
      (joplin--error 'debug "HTTP POST %s" url)
      (url-insert-file-contents
       (joplin--build-url url (append context joplin-context)))
      (json-read))))

(defun joplin--http-put-url (url args &optional context)
  (let ((url-proxy-services joplin-url-proxy)
        (url-request-method "PUT")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (encode-coding-string (json-serialize args) 'utf-8)))
    (with-temp-buffer
      (joplin--error 'debug "HTTP PUT %s" url)
      (url-insert-file-contents
       (joplin--build-url url (append context joplin-context)))
      (json-read))))

(defun joplin--http-del-url (url &optional context)
  (let ((url-proxy-services joplin-url-proxy)
        (url-request-method "DELETE"))
    (with-temp-buffer
      (joplin--error 'debug "HTTP DELETE %s" url)
      (url-insert-file-contents
       (joplin--build-url url (append context joplin-context))))))

(defun joplin--http-post-attachment-url (args filename &optional context)
  ;; (joplin--http-post-attachment-url '((title . "my image")) "front.png")
  (let ((url-proxy-services joplin-url-proxy)
        ret)
    (joplin--error 'debug "HTTP POST MULTIPART %s" url)
    (setq ret (joplin--http-post-simple-multipart
               (joplin--build-url "/resources"
                                  (append context joplin-context))
               (list (cons 'props
                           (json-serialize args)))
               (list (list 'data
                           filename
                           ""           ; TODO: how to get mime type?
                           (with-temp-buffer
                             (insert-file-contents filename)
                             (buffer-substring-no-properties
                              (point-min) (point-max)))))
               'utf-8))
    (json-parse-string (car ret) :object-type 'alist)))

(when nil
  (defalias 'joplin--http-get #'joplin--http-get-plz)
  (defalias 'joplin--http-put #'joplin--http-put-plz)
  (defalias 'joplin--http-del #'joplin--http-del-plz)
  (defalias 'joplin--http-post #'joplin--http-post-plz))

(defalias 'joplin--http-get #'joplin--http-get-url)
(defalias 'joplin--http-put #'joplin--http-put-url)
(defalias 'joplin--http-del #'joplin--http-del-url)
(defalias 'joplin--http-post #'joplin--http-post-url)
(defalias 'joplin--http-post-res #'joplin--http-post-attachment-url)

(provide 'joplin-http)
;;; joplin-http.el ends here
