;;; http-post-simple.el --- HTTP POST requests using the url library

;; Author: Tom Schutzer-Weissmann
;; Keywords: comm, data, processes, hypermedia

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as1
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Provides ways to use the url library to perform HTTP POST requests.
;; See the documentation to `joplin--http-post-simple' for more information.
;;
;; The `url-http' library does not handle 1xx response codes.

;; However, as RFC 2616 puts it:
;;     a server MAY send a 100 (Continue)
;;     status in response to an HTTP/1.1 PUT or POST request that does
;;     not include an Expect request-header field with the "100-continue"
;;     expectation.
;;
;; -- and some servers do, giving you annoying errors. To avoid these errors,
;; you can either set `url-http-version' to "1.0", in which case any compliant
;; server will not send the 100 (Continue) code, or call
;; `joplin--http-post-finesse-code-100'. Note that the latter advises
;; 'url-http-parse-response'
;;
;;; Change Log:

;; 23/10/2023 Imported to Joplin package (See below)
;; 11/06/2008 Set `url-http-version' to "1.0" when posting.
;; 19/07/2008 Don't set special variables like `url-http-version' and
;;            `url-http-attempt-keepalives'.
;; 03/11/2008 Tell the server what charset we're using & accepting.

;;;
;; The authentic package, `http-post-simple.el' seems not updated
;; for latest Emacs, which causing a few problems such as:
;;
;; - use of cl package (e.g. `values' and `destructuring-bind')
;; - URL package does not allow to send multibyte string which
;;   requires `encode-coding-string'.
;;
;; I didn't have enough time to fix these nicely, so I just
;; monkey-patched the file so that joplin-mode can work.  As I don't
;; want to create a conflict for users who already uses
;; `http-post-simple.el', all function/variable names are prefixed
;; with "joplin--" -- cinsk

;;; Code:
(require 'url)
(require 'url-http)

(defvar joplin--url-http-response-status nil) ; url-http

(defun joplin--http-post-simple (url fields &optional charset)
  "Send FIELDS to URL as an HTTP POST request, returning the response
and response headers.
FIELDS is an alist, eg ((field-name . \"value\")); all values
need to be strings, and they are encoded using CHARSET,
which defaults to 'utf-8"
  (joplin--http-post-simple-internal
   url
   (joplin--http-post-encode-fields fields charset)
   charset
   `(("Content-Type"
      .
      ,(joplin--http-post-content-type
        "application/x-www-form-urlencoded"
        charset)))))


(defun joplin--http-post-simple-multipart (url fields files &optional charset)
  "Send FIELDS and FILES to URL as a multipart HTTP POST, returning the
response and response headers.
FIELDS is an alist, as for `joplin--http-post-simple', FILES is an a list of
\(fieldname \"filename\" \"file MIME type\" \"file data\")*"
  (let ((boundary (joplin--http-post-multipart-boundary)))
    (joplin--http-post-simple-internal
     url
     (encode-coding-string (joplin--http-post-encode-multipart-data fields files charset) 'utf-8)
     charset
     `(("Content-Type"
        .
        ,(joplin--http-post-content-type
          (format "multipart/form-data; boundary=%S" boundary)
          charset))))))


(defun joplin--http-post-content-type (content-type &optional charset)
  (if charset
      (format "%s; charset=%s" content-type (joplin--http-post-charset-name charset))
    content-type))


(defun joplin--http-post-charset-name (charset)
  (symbol-name charset))


;; based on `http-url-encode' from the from http-get package
;; http://savannah.nongnu.org/projects/http-emacs
(defun joplin--http-post-encode-string (str content-type)
  "URL encode STR using CONTENT-TYPE as the coding system."
  (apply 'concat
         (mapcar (lambda (c)
                   (if (or (and (>= c ?a) (<= c ?z))
                           (and (>= c ?A) (<= c ?Z))
                           (and (>= c ?0) (<= c ?9)))
                       (string c)
                     (format "%%%02x" c)))
                 (encode-coding-string str content-type))))


(defun joplin--http-post-encode-fields (fields &optional charset)
  "Encode FIELDS using `joplin--http-post-encode-string', where
FIELDS is an alist of \(
        \(field-name-as-symbol . \"field value as string\"\) |
        \(field-name \"value1\" \"value2\" ...\)
        \)*

CHARSET defaults to 'utf-8"
  (let ((charset (or charset 'utf-8)))
    (mapconcat #'identity
               (mapcar (lambda (field)
                         (concat (symbol-name (car field))
                                 "="
                                 (joplin--http-post-encode-string (cdr field) charset)))
                       (cl-mapcan (lambda (field)
                                    (if (atom (cdr field)) (list field)
                                      ;; unpack the list
                                      (mapcar (lambda (value)
                                                `(,(car field) . ,value))
                                              (cdr field))))
                                  fields))
               "&")))


(defun joplin--http-post-simple-internal (url data charset extra-headers)
  (let ((url-request-method        "POST")
        (url-request-data          data)
        (url-request-extra-headers extra-headers)
        (url-mime-charset-string   (joplin--http-post-charset-name charset)))
    (let (header
          data
          status)
      (with-current-buffer
          (url-retrieve-synchronously url)
        ;; status
        (setq status joplin--url-http-response-status)
        ;; return the header and the data separately
        (goto-char (point-min))
        (if (search-forward-regexp "^$" nil t)
            (setq header (buffer-substring (point-min) (point))
                  data   (buffer-substring (1+ (point)) (point-max)))
          ;; unexpected situation, return the whole buffer
          (setq data (buffer-string))))
      (cl-values data header status))))


(defun joplin--http-post-multipart-boundary ()
  "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")


(defun joplin--http-post-bound-field (&rest parts)
  (let ((boundary (format "--%s" (joplin--http-post-multipart-boundary))))
    (joplin--http-post-join-lines  boundary parts)))


(defun joplin--http-post-encode-multipart-data (fields files charset)
  "Return FIELDS and FILES encoded for use as the data for a multipart HTTP POST request"
  (joplin--http-post-join-lines
   (mapcar (lambda (field)
             (joplin--http-post-bound-field
              (format "Content-Disposition: form-data; name=%S" (symbol-name (car field)))
              ""
              (cdr field)))
           fields)
   (cl-mapcan (lambda (file)
                (cl-destructuring-bind (fieldname filename mime-type data) file
                  (joplin--http-post-bound-field
                   (format "Content-Disposition: form-data; name=%S; filename=%S" fieldname filename)
                   (format "Content-type: %s" (joplin--http-post-content-type mime-type charset))
                   ""
                   data)))
              files)
   (format "--%s--" (joplin--http-post-multipart-boundary))))


(defun joplin--http-post-join-lines (&rest bits)
  (let ((sep "\r\n"))
    (mapconcat (lambda (bit)
                 (if (listp bit)
                     (apply 'joplin--http-post-join-lines bit)
                   bit))
               bits sep)))


(defun joplin--http-post-finesse-code-100 ()
  "Transforms response code 100 into 200, to avoid errors when the
server sends code 100 in response to a POST request."
  (defadvice url-http-parse-response (after url-http-parse-response-100 activate)
    "Turns any HTTP 100 response code to 200, to avoid getting an error."
    (declare (special joplin--url-http-response-status
                      url-request-method))
    (when (and (= 100               joplin--url-http-response-status)
               (string-equal "POST" url-request-method)
               (string-equal "1.1"  url-http-version))
      (setf joplin--url-http-response-status 200))))

(provide 'joplin-http-post-simple)

;;; http-post-simple.el ends here
