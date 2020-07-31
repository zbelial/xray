;; topic-thread.el --- Create topic threads across files.  -*- lexical-binding: t; -*-

;; Filename: topic-thread.el
;; Description: Topic threads across files.
;; Author:  zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer:  zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2020, zbelial, all rights reserved.
;; Created: 2020-07-30 23:08:12
;; Version: 0.1
;; URL: https://github.com/zbelial/topic-thread
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

(defcustom topic-thread-default-dir user-emacs-directory
  "The default directory used to store topic thread data."
  :type  'string
  :group 'org-pdf-thread)

(defcustom topic-thread-save-to-default-dir nil
  "If non-nil, topic thread data will be saved to `topic-thread-default-dir'.
Otherwise, the data will be saved to current directory."
  :type 'boolean
  :group 'org-pdf-thread)

(defcustom topic-thread-default-file-name "topic-thread-data.el"
  "The default file name used to store thread data."
  :type  'string
  :group 'org-pdf-thread)

(defvar topic-thread-file-map (make-hash-table :test 'equal)
  "Keep track of the thread data files of opened files.")

(defvar topic-thread-file-threads (make-hash-table :test 'equal)
  "In memory thread data. One item per file.")

(defvar topic-thread-topic-threads (make-hash-table :test 'equal)
  "In memory thread data. One item per thread.")

(defvar topic-thread-topic-desc (make-hash-table :test 'equal)
  "In memory topic and associated description.")

(defmacro topic-thread-with-message-suppression (&rest body)
  "Suppress any incoming messages within `body' while keeping the
currently displayed message, if any."
  (let ((msg (make-symbol "msg-temp")))
    `(let ((,msg (current-message))
           (message-log-max nil))
       (unwind-protect
           (progn ,@body)
         (if ,msg
             (message ,msg)
           (message nil))))))


(defun topic-thread-new-item-text()
  "Create a new item."
  )

(defun topic-thread-new-item-pdf()
  "Create a new item."
  )

(defun topic-thread-data-file-name (&optional file-name)
  "Find the thread data file of current file."
  (let ((file-name (or file-name (buffer-file-name)))
        data-file-name data-file-dir
        item )
    (when file-name
      (setq data-file-name (if topic-thread-file-map (gethash file-name topic-thread-file-map)))
      (when (not data-file-name)
        (setq data-file-dir (locate-dominating-file file-name topic-thread-default-file-name))
        (if data-file-dir
            (setq data-file-name (concat data-file-dir topic-thread-default-file-name))
          (setq data-file-name (concat topic-thread-default-dir topic-thread-default-file-name)))
        (puthash file-name data-file-name topic-thread-file-map))

      (expand-file-name data-file-name)
      )))

(defun topic-thread-memory-data (file-name)
  (when (not topic-thread-file-threads)
    (setq topic-thread-file-threads (make-hash-table :test 'equal)))
  (gethash file-name topic-thread-file-threads))

(defun topic-thread-add-thread ()
  "Add a new piece of thread data."
  (interactive)
  (if (eq major-mode 'eaf-mode)
      (let* ((app eaf--buffer-app-name)
             thread-name page file-name topic desc data-file memory-data
             )
        (if (string-equal app "pdf-viewer")
            (progn
              (setq file-name eaf--buffer-url)
              (setq page (eaf-call "call_function" eaf--buffer-id "current_page"))
              (setq data-file (topic-thread-data-file-name file-name))
              (setq topic (topic-thread-select-or-add-topic data-file))
              (setq memory-data (topic-thread-memory-data file-name))
              (add-to-list 'memory-data (list :topic topic :page page))
              (puthash file-name memory-data topic-thread-file-threads)
              )
          (message "Not an eaf pdf viewer buffer.")))
    (message "Not an eaf mode buffer")))

(defun topic-thread-add-desc-to-topic (topic)
  "The existing titles associated with the current file."
  (let ((desc (read-string "Description: ")))
    ;; (add-to-list 'topic-thread-topic-desc (cons topic desc))
    desc
    ))

(defun topic-thread-select-or-add-topic (data-file-name)
  "Select from the existing titles or create a new one."
  (interactive)
  (let* ((topic-desc (if topic-thread-topic-desc (gethash data-file-name topic-thread-topic-desc)))
         (topics (mapcar 'car topic-desc))
         (topic (completing-read "Select or Create Topic: " topics))
         desc)
    (when (not (member topic topics))
      (setq desc (topic-thread-add-desc-to-topic topic))
      (add-to-list 'topic-desc (cons topic desc))
      (when (not topic-thread-topic-desc)
        (setq topic-thread-topic-desc (make-hash-table :test 'equal))
        (puthash data-file-name topic-desc topic-thread-topic-desc)
        ))
    topic))

(defun topic-thread-load-data ()
  "Load saved thread data of current file."
  (let ((data-file (topic-thread-data-file)))
    (when (file-exists-p data-file)
      (annot-with-message-suppression
       (load-file filename))
      )))

(defun topic-thread-recover-topics (data)
  "Restore topic data."
  )

(defun topic-thread-recover-threads(data)
  "Restore thread data."
  )

(defun topic-thread-save-data (&optional file-name)
  (let ((file-name (or file-name (buffer-file-name))))
    (when file-name
      (let ((data-file-name (topic-thread-data-file file-name)))
        (topic-thread-save-topic data-file-name (gethash data-file-name topic-thread-topic-desc))
        (topic-thread-save-thread file-name (gethash file-name topic-thread-file-threads)))))
  )

(defun topic-thread-save-all-data ()
  "Save thread data."
  (interactive)
  (topic-thread-save-topics)
  (topic-thread-save-threads)
  )

(defun topic-thread-save-topic (data-file-name topics)
  ""
  (with-temp-file data-file-name
    (erase-buffer)
    (insert (format "\(topic-thread-recover-topics '\(
:file \"%s\"
:topics \(
%s
)))" data-file-name (mapconcat (lambda (item)
                                 (format "\(:topic \"%s\" :desc \"%s\")" (car item) (cdr item)))
                               topics "\n")))
    )
  )

(defun topic-thread-save-topics ()
  "Save thread data."
  (interactive)
  (maphash #'topic-thread-save-topic topic-thread-topic-desc)
  )

;; TODO bug - overwrite data, erase data
(defun topic-thread-save-thread (file-name threads)
  ""
  (let ((data-file-name (topic-thread-data-file-name file-name)))
    (with-temp-file data-file-name
      (erase-buffer)
      (insert (format "\(topic-thread-recover-threads '\(
:file \"%s\"
:data-file-name \"%s\"
:threads \(
%s
)))" file-name data-file-name (mapconcat (lambda (thread)
                                           ;; (format "%s" thread))
                                           (format "\(:topic \"%s\" :page \"%s\")" (plist-get thread :topic) (plist-get thread :page)))
                                         threads "\n")))
      )
    ))

(defun topic-thread-save-threads ()
  "Save thread data."
  (interactive)
  (maphash #'topic-thread-save-thread topic-thread-file-threads)
  )

(add-hook 'kill-emacs-hook #'topic-thread-save-all-data)

(provide 'topic-thread)
