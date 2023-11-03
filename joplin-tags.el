;;; joplin-tags.el --- Joplin request                -*- lexical-binding: t; -*-

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

(defvar joplin-tags nil
  ;; TODO: (TAG-ID . TAG) or (TAG-TITLE . TAG)?  Or (TAG-TITLE . TAG-ID)
  "")

(defun joplin--tags-init ()
  (unless joplin-tags
    (setq joplin-tags (make-hash-table :test #'equal :size 200))
    (joplin-revert-tags))
  joplin-tags)

(defun joplin-revert-tags ()
  (clrhash joplin-tags)
  (let ((iter (joplin--gen-tags)))
    (while iter
      (condition-case x
          (let ((tag (iter-next iter)))
            (if tag
                (puthash (JTAG-title tag) tag joplin-tags)))
        (iter-end-of-sequence (setq iter nil))))))

(defun joplin--true-tag (name)
  "Return JTAG struct if NAME is a Joplin Tag title."
  (let* ((resp (joplin--http-get "/search" `((query . ,name)
                                             (type . tag)
                                             (limit . 1))))
         (items (alist-get 'items resp)))
    (if (= (length items) 0)
        nil
      (let ((tag (build-JTAG (aref items 0))))
        (puthash (JTAG-title tag) tag joplin-tags)
        tag))))

(defun joplin--tag-names ()
  (let (lst)
    (maphash (lambda (k v)
               (push (JTAG-title v) lst))
             joplin-tags)
    lst))

(defun joplin--note-add-tag (note tag)
  (if (JTAG-p tag)
      (setq tag (JTAG-id tag)))
  (if (JNOTE-p note)
      (setq note (JNOTE-id note)))
  (joplin--http-post (concat "/tags/" tag "/notes")
                     `((id . ,note)))
;; {
;;     "created_time": 1698429262984,
;;     "id": "f6082c39de9944579fe50c94d7d3c6ce",
;;     "note_id": "28839f0413f444debd7349aa63c8b91b",
;;     "tag_id": "609221e0fbae455e894d3133ec14703b",
;;     "type_": 6,
;;     "updated_time": 1698429262984,
;;     "user_created_time": 1698429262984,
;;     "user_updated_time": 1698429262984
  ;; }
  )

(defun joplin--note-delete-tag (note tag)
  (if (JTAG-p tag)
      (setq tag (JTAG-id tag)))
  (if (JNOTE-p note)
      (setq note (JNOTE-id note)))
  (joplin--http-del (concat "/tags/" tag "/notes/" note)))



(defun joplin--tag-create (name)
  (let ((tag (joplin--true-tag name))
        resp)
    ;; Hmm, a tag metadata from `joplin--true-tag' may lack some
    ;; metadata that were already there in joplin-tags.  However, for
    ;; TAGs, id and title are the only important fields so ...

    ;; See also `joplin--note-tags'.
    (unless tag
      (setq resp (joplin--http-post "/tags" `((title . ,name))))
      (setq tag (build-JTAG resp))
      (puthash (JTAG-title tag) tag joplin-tags))
    tag))


(defun joplin--completing-read-tags (prompt &optional require-match initial-input)
  (let ((resp (completing-read-multiple
               prompt (joplin--tag-names) nil require-match initial-input))
        taglst)
    (dolist (name resp)
      (let ((tag (joplin--tag-create name)))
        (push tag taglst)))
    taglst))


(defun joplin--tag-add-del-list (src dst)
  "Return (DEL-LST . ADD-LST) by comparing TAGS in SRC and DST

SRC is the list of current tags of the note, DST is a list of
tags that user want to set to the note.  ADD-LST is a list of
tags that need to be added, and DEL-LST is a list of tags that should
be deleted.

SRC and DST are a list of JTAG structs.  DEL-LST and ADD-LST are
also a list of JTAG struct.

For example, if SRC is (tag1 tag2 tag3) and DST is (tag1 tag3 tag4),
then this funtion will return ((tag2) . (tag4))."
  (let (add del)
    (dolist (d dst)
      (unless (cl-member d src :test (lambda (a b)
                                       (string-equal (JTAG-id a) (JTAG-id b))))
        (push d add)))
    (dolist (s src)
      (unless (cl-member s dst :test (lambda (a b)
                                       (string-equal (JTAG-id a) (JTAG-id b))))
        (push s del)))
    (cons del add)))


(provide 'joplin-tags)

;;; joplin-tags.el ends here
