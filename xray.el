;; xray.el --- Create topic threads across files.  -*- lexical-binding: t; -*-

;; Filename: xray.el
;; Description: Topic threads across files.
;; Author:  zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer:  zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2020, zbelial, all rights reserved.
;; Created: 2020-07-30 23:08:12
;; Version: 0.1
;; URL: https://github.com/zbelial/xray.el
;; Package-Requires: ((emacs "26.1") (ht "2.2") (f "0.17.2") (s "1.12.0"))
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

(require 'ht)
(require 's)
(require 'f)
(require 'seq)
(require 'cl-seq)

(declare-function doc-view-current-page "doc-view")
(declare-function pdf-view-bookmark-make-record "ext:pdf-view")
(declare-function pdf-view-current-page "ext:pdf-view")
(declare-function pdf-view-mode "ext:pdf-view")

;;; Custom
(defcustom xr-default-directory user-emacs-directory
  "The default directory used to store topic thread data."
  :type  'string
  :group 'xray)

(defcustom xr-directory-alist nil
  "Alist of directory.
The rays added to files in the first directory will be saved to second directory."
  :group 'xray
  :type '(repeat (cons string (cons (directory :tag "Directory storing files.")
                                    (directory :tag "XRay data file directory.")))))

(defcustom xr-open-pdf-with-eaf nil
  "When non-nil, use eaf-open to open pdf files."
  :group 'xray
  :type 'boolean)

(defcustom xr-recent-topic-count 5
  "The number of topics recently added or visited that should be saved."
  :group 'xray
  :type 'integer)

(defcustom xr-show-visible-area-xray-count nil
  "When non-nil, show how many xray in visible area of current buffer."
  :group 'xray
  :type 'boolean)


(defconst xr-default-tag "__DEFAULT_TAG__")
(defconst xr-default-xray-file-name "xray-data.el")
(defconst xr-default-topic-file-name "xray-topic.el")

;;; Variables
(defvar xr-file-rays (ht-create)
  "Which rays each file has.
key: raw file name, value: rays")

(defvar xr-loaded-xray-files (ht-create)
  "Which rays are stored in each xray file.
key: xray file name, value: rays.")

(defvar xr-topics (ht-create)
  "Which topics are stored in each xray file.
key: xray file name, value: topics")

(defvar xr-file-topics (ht-create)
  "Which topics are stored in each file.
key: file name, value: topics")

(defvar xr-latest-modified-time (ht-create)
  "Keep track of the latest time when new xray data is added.")

(defvar xr-recent-topics (ht-create)
  "Keep track of recently visited topics for each xray file.
key: xray-file-name, value: topics")

(defvar xr-cleared nil
  "When `xr-cleared' is not nil, mode-line info shows.")

(defvar xray-note-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'xray-note-edit-buffer-cancel)
    (define-key map (kbd "C-c C-c") #'xray-note-edit-buffer-confirm)
    map))

(define-minor-mode xray-note-edit-mode
  "The major mode to edit xray note file."
  :keymap xray-note-edit-mode-map)

(defun xray-note-edit-buffer-cancel ()
  "Cancel editing note file."
  (interactive)
  (kill-buffer)
  (delete-window)
  )

(defun xray-note-edit-buffer-confirm()
  "Saves the note-editing buffer and kills the window."
  (interactive)
  (save-buffer)
  (let* ((file (plist-get ray :file))
         (xray-file (xr-xray-file-name file))
         (xray-dir (xr-xray-file-directory file))
         (id (plist-get ray :id))
         (file-rays (ht-get xr-file-rays file))
         )
    ;; (message "before adding :note-file")
    ;; (message "has-note %S" has-note)
    ;; (message "note-file: %s" note-file)
    ;; (message "id: %d" id)
    (when (not has-note)
      ;; (message "add :note-file")
      (setq ray (plist-put ray :note-file (s-chop-prefix (f-full xray-dir) (f-full note-file))))
      (setq file-rays (cl-remove-if #'(lambda (ray)
                                     (equal id (plist-get ray :id)))
                                 file-rays))
      (push ray file-rays)

      (ht-set! xr-file-rays file file-rays)
      (xr-save-rays xray-file)))
  
  (kill-buffer-and-window)
  )

;;; Macros
(defmacro xr-with-message-suppression (&rest body)
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

;;; Add ray
(defsubst xr-assoc-matched-filename (filename)
  "Return xr-directory-alist item which matches filename most."
  (let ((max 0)
        result)
    (dolist (assoc xr-directory-alist)
      (when (string-prefix-p (cadr assoc) filename)
        (when (> (length (cadr assoc)) max)
          (setq result assoc)
          (setq max (length (cadr assoc))))))
    (identity result)))

(defsubst xr-assoc-matched-tag (tag)
  "Return xr-directory-alist item matching tag."
  (cl-find-if
   (lambda (x) (string= (car x) tag))
   xr-directory-alist))

(defsubst xr-xray-file-directory (filename)
  "Return xray file directory of filename"
  (let ((xr-dir-assoc (xr-assoc-matched-filename filename)))
    (if xr-dir-assoc
        (f-slash (cddr xr-dir-assoc))
      (f-slash xr-default-directory))))

(defsubst xr-file-tag (filename)
  "Return base directory of filename"
  (let ((xr-dir-assoc (xr-assoc-matched-filename filename)))
    (if xr-dir-assoc
        (car xr-dir-assoc)
      xr-default-tag)))

(defsubst xr-xray-file-directory-tag (tag)
  "Return contents directory using tag"
  (let ((xr-dir-assoc (xr-assoc-matched-tag tag)))
    (if xr-dir-assoc
        (f-slash (cddr xr-dir-assoc))
      (f-slash xr-default-directory))))

(defsubst xr-file-base-directory (filename)
  "Return base directory of filename"
  (let ((xr-dir-assoc (xr-assoc-matched-filename filename)))
    (if xr-dir-assoc
        (f-slash (cadr xr-dir-assoc))
      "/"
      )))

(defsubst xr-file-base-directory-tag (tag)
  "Return base directory of tag"
  (let ((xr-dir-assoc (xr-assoc-matched-tag tag)))
    (if xr-dir-assoc
        (f-slash (cadr xr-dir-assoc))
      "/"
      )))

(defsubst xr-file-relative-path (filename)
  "Return relative path of filename to xr-file-base-directory"
  (let ((xr-dir-assoc (xr-assoc-matched-filename filename)))
    (if xr-dir-assoc
        (s-chop-prefix (f-full (cadr xr-dir-assoc)) filename)
      (s-chop-prefix "/" filename))))

(defsubst xr-xray-file-relative-path (filename xray-file-name)
  "Return relative path of filename to xr-file-base-directory"
  (let ((xr-dir-assoc (xr-assoc-matched-filename filename)))
    (if xr-dir-assoc
        (s-chop-prefix (f-full (cddr xr-dir-assoc)) xray-file-name)
      (s-chop-prefix (f-full xr-default-directory) xray-file-name)
      )))


(defun xr-current-line-number ()
  ""
  (line-number-at-pos)
  )

(defun xr-current-time()
  ""
  (time-convert nil 'integer))

(defun xr-current-time-readable()
  ""
  (format-time-string "%Y%m%d%H%M%S"))

(defun xr-new-ray-text-or-prog(file-name &optional xray-file-name id topic desc)
  "Create a new ray."
  (let* ((topic (or topic (xr-select-or-add-topic file-name xray-file-name)))
         (desc (or desc (xr-add-desc topic)))
         (id (or id (xr-id)))
         (linum (xr-current-line-number))
         (context ""))
    (list :id id :type "text" :file file-name :topic topic :desc desc :linum linum :context context)
    ))

(defun xr-pdf-view-page-percent ()
  ""
  (let ((bookmark (pdf-view-bookmark-make-record))
        (percent 0.0))
    (dolist (ele bookmark)
      ;; (message "ele %S" ele)
      (when (and (listp ele) (eq (car ele) 'origin))
        ;; (message "target %S" ele)
        (setq percent (cddr ele))
        ))
    percent))

(defun xr-pdf-view-total-percent ()
  (let ((page-percent (xr-pdf-view-page-percent))
        (current-page (pdf-view-current-page))
        (total-page (pdf-cache-number-of-pages)))
    (/ (+ (1- current-page) page-percent) total-page)))

(defun xr-eaf-pdf-page-percent()
  (let ((current-page (string-to-number (eaf-call "call_function" eaf--buffer-id "current_page")))
        (total-page (string-to-number (eaf-call "call_function" eaf--buffer-id "page_total_number")))
        (total-percent (string-to-number (eaf-call "call_function" eaf--buffer-id "current_percent"))))
    (* (- 
        (/ total-percent 100.0)
        (/ (* 1.0 (1- current-page)) total-page)
        )
       total-page)))

(defun xr-pdf-page-and-percent (&optional file-name)
  (let ((page-no 0)
        (percent-eaf -1)
        (percent-other -1))
    (cond
     ((eq major-mode 'pdf-view-mode)
      (setq page-no (pdf-view-current-page))
      (setq percent-eaf (* 100 (xr-pdf-view-total-percent)))
      (setq percent-other (* 100 (xr-pdf-view-page-percent))))
     ((eq major-mode 'doc-view-mode)
      (setq page-no (doc-view-current-page)))
     ((eq major-mode 'eaf-mode)
      (setq page-no (string-to-number (eaf-call "call_function" eaf--buffer-id "current_page")))
      (setq percent-eaf (string-to-number (eaf-call "call_function" eaf--buffer-id "current_percent")))
      (setq percent-other (* 100 (xr-eaf-pdf-page-percent))))
     (t
      (user-error (format "%s" "Unsupported mode."))))
    (list page-no percent-eaf percent-other)))

(defun xr-new-ray-pdf(file-name &optional xray-file-name id topic desc)
  "Create a new ray."
  (let* ((xray-file-name (or xray-file-name (xr-xray-file-name file-name)))
         (page-percent (xr-pdf-page-and-percent file-name))
         (page (nth 0 page-percent))
         (percent-eaf (nth 1 page-percent))
         (percent-other (nth 2 page-percent))
         (id (or id (xr-id)))
         (topic (or topic (xr-select-or-add-topic file-name xray-file-name)))
         (desc (or desc (xr-add-desc topic)))
         )
    (list :id id :type "pdf" :file file-name :topic topic :desc desc :page page :context "" :percent (cons percent-eaf percent-other))))

(defun xr-new-ray-html(file-name &optional xray-file-name id topic desc)
  "Create a new ray."
  (let* ((topic (or topic (xr-select-or-add-topic file-name xray-file-name)))
         (desc (or desc (xr-add-desc topic)))
         (id (or id (xr-id)))
         (linum (xr-current-line-number))
         (context ""))
    (list :id id :type "html" :file file-name :topic topic :desc desc :linum linum :context context))
  )

(defun xr-xray-file-name (&optional file-name)
  "Find the xray data file of current file."
  (let ((file-name (or file-name (xr-buffer-file-name)))
        xray-file-name xray-file-dir)
    (when file-name
      (setq xray-file-dir (xr-xray-file-directory file-name))
      (if (not (f-exists-p xray-file-dir))
          (make-directory xray-file-dir t)
        )
      (setq xray-file-name (concat (f-slash xray-file-dir) xr-default-xray-file-name))

      (expand-file-name xray-file-name)
      )))

(defun xr-xray-topic-file-name (&optional file-name)
  "Find the xray data file of current file."
  (let ((file-name (or file-name (xr-buffer-file-name)))
        xray-file-name xray-file-dir)
    (when file-name
      (setq xray-file-dir (xr-xray-file-directory file-name))
      (if (not (f-exists-p xray-file-dir))
          (make-directory xray-file-dir t)
        )
      (setq xray-file-name (concat (f-slash xray-file-dir) xr-default-topic-file-name))

      (expand-file-name xray-file-name)
      )))

(defsubst xr-remove-html-anchor (file-name)
  ""
  (let* ((len (length file-name))
         (pos-of-anchor (s-index-of "#" file-name)))
    (if (not pos-of-anchor) ;; no anchor part
        file-name
      (substring file-name 0 pos-of-anchor))))


(defun xr-buffer-file-name()
  ""
  (cond
   ((eq major-mode 'eaf-mode)
    (let ((app eaf--buffer-app-name))
      (when (string-equal app "pdf-viewer")
        eaf--buffer-url))
    )
   ((eq major-mode 'eww-mode)
    (let (buffer-url)
      (setq buffer-url (eww-current-url))
      (when (string-prefix-p "file://" buffer-url)
        (xr-remove-html-anchor (substring buffer-url (length "file://")))))
    )
   ((eq major-mode 'w3m-mode)
    (let (buffer-url)
      (setq buffer-url w3m-current-url)
      (when (string-prefix-p "file://" buffer-url)
        (xr-remove-html-anchor (substring buffer-url (length "file://")))))
    )
   (t
    (buffer-file-name))))

(defun xr-update-recent-topics (file-name topic)
  ""
  (let* ((xray-file (xr-xray-file-name file-name))
         (topics (xr-recent-topics file-name)))
    (setq topics (delete-dups (cons topic topics)))
    (when (> (length topics) xr-recent-topic-count)
      (setq topics (seq-take topics xr-recent-topic-count)))
    (ht-set xr-recent-topics xray-file topics)
    (xr-save-recent-topics file-name)
    ))

(defun xr-recent-topic (file-name)
  ""
  (let ((xray-file-name (xr-xray-file-name file-name))
        topics)
    (setq topics (ht-get xr-recent-topics xray-file-name nil))
    topics))

(defun xr-recent-topics (file-name)
  ""
  (let ((xray-file-name (xr-xray-file-name file-name))
        topics)
    (setq topics (ht-get xr-recent-topics xray-file-name nil))
    topics))

(defun xr-add-ray()
  "Create a new ray and add it to xray file."
  (interactive)
  (let* ((file-name (xr-buffer-file-name))
         xray-file-name rays topic topics
         ray)
    (if (and
         file-name
         (file-exists-p file-name))
        (progn
          (setq xray-file-name (xr-xray-file-name file-name))

          (xr-load-data-ensure file-name)

          (setq ray (xr-new-ray file-name xray-file-name))
          (when (not (ht-contains-p xr-file-rays file-name))
            (ht-set! xr-file-rays file-name '()))

          (setq rays (ht-get xr-file-rays file-name))
          (push ray rays)
          (ht-set! xr-file-rays file-name rays)

          (setq topic (plist-get ray :topic))
          (setq topics (ht-get xr-topics xray-file-name))
          (setq topics (delete topic topics))
          (push topic topics)
          (ht-set! xr-topics xray-file-name topics)

          (xr-update-recent-topics file-name topic)

          (xr-save-rays xray-file-name)
          )
      (user-error "%s" "Cannot add ray to this file."))
    )
  )

(defun xr-id ()
  "Random number as ray id."
  (random 9999999999999))

(defun xr-new-ray (file-name xray-file-name)
  "Create a new piece of xray data."
  (let* ((suffix (f-ext file-name))
         ray)
    ;; (message "xr-new-ray suffix: %s" suffix)
    (cond
     ((s-equals? suffix "pdf")
      (setq ray (xr-new-ray-pdf file-name xray-file-name))
      )
     ((or
       (eq major-mode 'eww-mode)
       (eq major-mode 'w3m-mode))
      (setq ray (xr-new-ray-html file-name xray-file-name))
      )
     ((derived-mode-p 'text-mode)
      (setq ray (xr-new-ray-text-or-prog file-name xray-file-name))
      )
     ((derived-mode-p 'prog-mode)
      (setq ray (xr-new-ray-text-or-prog file-name xray-file-name))
      )
     (t
      (user-error "%s" "Unsupported file type."))
     )
    )
  )

(defun xr-add-desc (topic)
  "Add a desc to ray."
  (read-string (format "Add Description (for %s): " topic) "")
  )

(defun xr-select-or-add-topic (file-name xray-file-name)
  "Select from the existing titles or create a new one."
  (let* ((topics (ht-get xr-topics xray-file-name))
         (topic (completing-read "Select or create a topic: " topics)))
    topic))

;;; Load xray data
(defun xr-clear (&optional file-name)
  ""
  (interactive)
  (setq xr-cleared t)
  (let* ((file-name (or file-name (xr-buffer-file-name)))
         (xray-file-name (xr-xray-file-name file-name))
         (files (xr-files-in-xray xray-file-name)))
    (ht-remove! xr-loaded-xray-files xray-file-name)
    (ht-remove! xr-topics xray-file-name)
    (ht-remove! xr-latest-modified-time xray-file-name)
    (dolist (file files)
      (ht-remove! xr-file-rays file)
      (ht-remove! xr-file-topics file))))

(defun xr-clear-all ()
  ""
  (interactive)
  (setq xr-cleared t)
  (ht-clear! xr-loaded-xray-files)
  (ht-clear! xr-topics)
  (ht-clear! xr-file-rays)
  (ht-clear! xr-file-topics)
  (ht-clear! xr-latest-modified-time))

(defun xr-xray-file-changed-time (xray-file-name)
  ""
  (time-convert (file-attribute-modification-time (file-attributes xray-file-name)) 'integer)
  )

(defun xr-xray-file-outdated (xray-file-name)
  ""
  (let ((latest-added-time (ht-get xr-latest-modified-time xray-file-name))
        (xray-file-changed-time (xr-xray-file-changed-time xray-file-name))
        (current-time (time-convert nil 'integer))
        )
    (< latest-added-time xray-file-changed-time)))

(defun xr-load-data-ensure (file-name)
  ""
  ;; (message "file-name %s, xray-file-name %s" file-name (xr-xray-file-name file-name))
  (setq xr-cleared nil)
  (let ((xray-file-name (xr-xray-file-name file-name))
        (xray-topic-file-name (xr-xray-topic-file-name file-name)))
    (when (or (not (ht-contains-p xr-loaded-xray-files xray-file-name))
              (xr-xray-file-outdated xray-file-name))
      (xr-load-data xray-file-name xray-topic-file-name))))

(defun xr-load-data (xray-file-name xray-topic-file-name)
  "Load xray file."
  (xr-with-message-suppression
   (when (f-exists-p xray-file-name)
     (ht-set! xr-loaded-xray-files xray-file-name t)
     (ht-set! xr-topics xray-file-name '())
     (ht-set! xr-latest-modified-time xray-file-name (time-convert nil 'integer))
     (load-file xray-file-name))
   (when (f-exists-p xray-topic-file-name)
     (ht-set! xr-recent-topics xray-file-name '())
     (load-file xray-topic-file-name))))

(defun xr-reload (&optional file)
  ""
  (interactive)
  (let ((file (or file (xr-buffer-file-name)))
        xray-file xray-topic-file)
    (when file
      (setq xray-file (xr-xray-file-name file))
      (setq xray-topic-file (xr-xray-topic-file-name file))
      (xr-clear file)
      (xr-load-data xray-file xray-topic-file))))

(defun xr-recover-recent-topics(data)
  (let ((tag (plist-get data :tag))
        (recent-topics (plist-get data :recent-topics))
        xray-file
        )
    (setq xray-file (concat (expand-file-name (xr-xray-file-directory-tag tag)) xr-default-xray-file-name))

    (ht-set! xr-recent-topics xray-file '())
    (when recent-topics
      (ht-set! xr-recent-topics xray-file recent-topics))))

(defun xr-recover-file-rays(data)
  "Restore xray data."
  (let ((file (plist-get data :file))
        (tag (plist-get data :tag))
        (rays (plist-get data :rays))
        xray-file
        )

    (setq file (concat (expand-file-name (xr-file-base-directory-tag tag)) file))
    (setq xray-file (concat (expand-file-name (xr-xray-file-directory-tag tag)) xr-default-xray-file-name))

    (ht-set! xr-file-rays file '())
    (ht-set! xr-file-topics file '())

    ;; (message "rays: %S" rays)

    (when rays
      (let ((xray-topics (ht-get xr-topics xray-file))
            file-topics topic-rays file-rays)
        (dolist (ray rays)
          (setq ray (plist-put ray :file file))

          (setq topic (plist-get ray :topic))
          (push topic file-topics)

          (push topic xray-topics)

          (push ray file-rays)
          )

        (ht-set! xr-file-topics file file-topics)
        (ht-set! xr-file-rays file file-rays)
        (ht-set! xr-topics xray-file xray-topics)))))

;;; Save xray to file.
(defun xr-format-ray (ray)
  ""
  (let ((type (plist-get ray :type)))
    (cond
     ((s-equals? type "text")
      (format "\(:id %d :type \"%s\" :topic \"%s\" :desc \"%s\" :linum %d :context \"%s\" :note-file %S)"
              (plist-get ray :id)
              (plist-get ray :type)
              (plist-get ray :topic)
              (plist-get ray :desc)
              (plist-get ray :linum)
              (plist-get ray :context)
              (plist-get ray :note-file))
      )
     ((s-equals? type "pdf")
      (format "\(:id %d :type \"%s\" :topic \"%s\" :desc \"%s\" :page %d :context \"%s\" :percent %S :note-file %S)"
              (plist-get ray :id)
              (plist-get ray :type)
              (plist-get ray :topic)
              (plist-get ray :desc)
              (plist-get ray :page)
              (plist-get ray :context)
              (plist-get ray :percent)
              (plist-get ray :note-file))
      )
     ((s-equals? type "html")
      (format "\(:id %d :type \"%s\" :topic \"%s\" :desc \"%s\" :linum %d :context \"%s\" :note-file %S)"
              (plist-get ray :id)
              (plist-get ray :type)
              (plist-get ray :topic)
              (plist-get ray :desc)
              (plist-get ray :linum)
              (plist-get ray :context)
              (plist-get ray :note-file))
      )
     (t
      (user-error "%s - %s" "Invalid ray type" type))
     )))

(defun xr-save--recent-topics (xray-topic-file-name file-name)
  (let* ((topics (xr-recent-topics file-name)))
    (when topics
      (f-append-text
       (format "\(xr-recover-recent-topics '\(
:tag \"%s\"
:recent-topics
%S
))\n\n" (xr-file-tag file-name) topics)
       'utf-8-unix
       xray-topic-file-name
       ))))

(defun xr-save-file-rays (xray-file-name file-name)
  ""
  (let* ((rays (ht-get xr-file-rays file-name)))
    (when rays
      (f-append-text
       (format "\(xr-recover-file-rays '\(
:tag \"%s\"
:file \"%s\"
:rays \(
%s
)))\n\n" (xr-file-tag file-name) (xr-file-relative-path file-name) (mapconcat #'xr-format-ray rays "\n"))
       'utf-8-unix
       xray-file-name
       ))))

(defun xr-files-in-xray (xray-file-name)
  ""
  (let ((all-files (ht-keys xr-file-rays))
        files)
    (dolist (file all-files)
      (when (s-equals? xray-file-name (xr-xray-file-name file))
        (push file files)))
    files))


(defun xr-update-modified-time (xray-file-name)
  ""
  (ht-set xr-latest-modified-time xray-file-name (xr-current-time)))

(defun xr-xray-file-tag (xray-file-name)
  ""
  (let ((files (xr-files-in-xray xray-file-name)))
    (when files
      (xr-file-tag (nth 0 files)))))

(defun xr-save-rays (xray-file-name &optional files)
  ""
  (with-temp-file xray-file-name
    (erase-buffer)
    (insert ";;; -*- mode: emacs-lisp -*-\n")
    )

  (xr-update-modified-time xray-file-name)

  (dolist (file (or files (xr-files-in-xray xray-file-name)))
    (xr-save-file-rays xray-file-name file)))

(defun xr-save-recent-topics (file-name)
  ""
  (let ((xray-topic-file-name (xr-xray-topic-file-name file-name)))
    (with-temp-file xray-topic-file-name
      (erase-buffer)
      (insert ";;; -*- mode: emacs-lisp -*-\n")
      )

    (xr-save--recent-topics xray-topic-file-name file-name)))

;;; Read xray and display them

(defun xr-topics-in-file (&optional file-name)
  ""
  (interactive)
  (let* ((file-name (or file-name (xr-buffer-file-name))))
    (when file-name
      (xr-load-data-ensure file-name)
      (ht-get xr-file-topics file-name))))

(defun xr-topics (&optional file-name)
  ""
  (interactive)
  (let ((file-name (or file-name (xr-buffer-file-name)))
        xray-file-name)
    (when file-name
      (setq xray-file-name (xr-xray-file-name file-name))
      (xr-load-data-ensure file-name)

      (ht-get xr-topics xray-file-name))))

(defun xr-rays-in-visible-area (&optional file-name)
  (let ((file-name (xr-buffer-file-name))
        begin-line
        end-line
        page-no xray-line xray-page-no
        rays visible-rays)
    (when file-name
      (setq rays (xr-rays-in-file file-name))
      (cond
       ((or (derived-mode-p 'text-mode)
            (derived-mode-p 'prog-mode))
        (setq begin-line (line-number-at-pos (window-start)))
        (setq end-line (1- (line-number-at-pos (window-end))))
        (dolist (ray rays)
          (setq xray-line (plist-get ray :linum))
          (when (and (>= xray-line begin-line)
                     (<= xray-line end-line))
            (push ray visible-rays))))
       ((or (derived-mode-p 'eww-mode)
            (derived-mode-p 'w3m-mode))
        (setq begin-line (line-number-at-pos (window-start)))
        (setq end-line (1- (line-number-at-pos (window-end))))
        (dolist (ray rays)
          (setq xray-line (plist-get ray :linum))
          (when (and (>= xray-line begin-line)
                     (<= xray-line end-line))
            (push ray visible-rays))))
       ((s-suffix? ".pdf" file-name t)
        (setq page-no (nth 0 (xr-pdf-page-and-percent file-name)))
        
        (dolist (ray rays)
          (setq xray-page-no (plist-get ray :page))
          (when (= page-no xray-page-no)
            (push ray visible-rays))))
       (t
        (user-error (format "%s" "Unsupported file type."))))
      )
    visible-rays))

(defun xr-rays-in-file (&optional file-name)
  "Rays in current file."
  (interactive)
  (let ((file-name (or file-name (xr-buffer-file-name))))
    (when file-name
      (xr-load-data-ensure file-name)
      (ht-get xr-file-rays file-name)
      )))

(defun xr-rays-of-topic (topic)
  "Rays in current file."
  (interactive)
  (let ((file-name (xr-buffer-file-name))
        xray-file-name
        files
        rays all-rays)
    (when file-name
      (setq xray-file-name (xr-xray-file-name file-name))
      (xr-load-data-ensure file-name)

      (setq files (xr-files-in-xray xray-file-name))
      (dolist (file files)
        (setq rays (ht-get xr-file-rays file))
        (dolist (ray rays)
          (when (s-equals-p (plist-get ray :topic) topic)
            (push ray all-rays))))
      all-rays
      )))

(defun xr-rays (&optional file-name)
  "Rays in file-name's xray-file."
  (interactive)
  (let ((file-name (or file-name (xr-buffer-file-name)))
        xray-file-name all-rays files)
    (when file-name
      (setq xray-file-name (xr-xray-file-name file-name))
      (xr-load-data-ensure file-name)

      (setq files (xr-files-in-xray xray-file-name))
      (dolist (file files)
        (dolist (ray (ht-get xr-file-rays file))
          (push ray all-rays))))
    all-rays))

;;; move

(defun xr-new-ray-for-move (file-name xray-file-name id topic desc)
  "Create a new piece of xray data using old `id', `topic' and `desc'."
  (let* ((suffix (f-ext file-name))
         ray)
    (cond
     ((s-equals? suffix "pdf")
      (setq ray (xr-new-ray-pdf file-name xray-file-name id topic desc))
      )
     ((or
       (eq major-mode 'eww-mode)
       (eq major-mode 'w3m-mode))
      (setq ray (xr-new-ray-html file-name xray-file-name id topic desc))
      )
     ((derived-mode-p 'text-mode)
      (setq ray (xr-new-ray-text-or-prog file-name xray-file-name id topic desc))
      )
     ((derived-mode-p 'prog-mode)
      (setq ray (xr-new-ray-text-or-prog file-name xray-file-name id topic desc))
      )
     (t
      (user-error "%s" "Unsupported file type."))
     )
    )
  )


(defun xr-move-ray (ray)
  ""
  (let* ((file (plist-get ray :file))
         (xray-file (xr-xray-file-name file))
         (id (plist-get ray :id))
         (topic (plist-get ray :topic))
         (desc (plist-get ray :desc))
         (note-file (plist-get ray :note-file))
         (file-rays (ht-get xr-file-rays file))
         (current-file (xr-buffer-file-name))
         new-ray)
    (if (string-equal file current-file)
        (progn
          (when (yes-or-no-p "Please move to the target position. Ready?")
            (setq new-ray (xr-new-ray-for-move file xray-file id topic desc))
            (setq file-rays (cl-remove-if #'(lambda (ray)
                                           (equal id (plist-get ray :id)))
                                       file-rays))
            (when note-file
              (setq new-ray (plist-put :note-file note-file))
              )
            (message "ray %S" ray)
            (message "new-ray %S" new-ray)

            (push new-ray file-rays)
            (ht-set! xr-file-rays file file-rays)
            (xr-save-rays xray-file)))
      (user-error (format "You can only move xray in current file.")))))


;;; Delete

(defun xr-delete-ray (ray)
  ""
  (let* ((file (plist-get ray :file))
         (xray-file (xr-xray-file-name file))
         (id (plist-get ray :id))
         (file-rays (ht-get xr-file-rays file)))
    (setq file-rays (cl-remove-if #'(lambda (ray)
                                   (equal id (plist-get ray :id)))
                               file-rays))
    (ht-set! xr-file-rays file file-rays)
    (when (not file-rays)
      (ht-remove! xr-file-rays file))
    (xr-save-rays xray-file)))

;;; Edit Ray
(defun xr-note-file-name (xray-dir topic id)
  (let* ((now (xr-current-time-readable))
         (topic (nth 0 (s-split "/" topic)))
         (dir (concat (f-slash xray-dir) "notes/"))
         (filename (concat (f-slash xray-dir) "notes/" topic "-" (number-to-string id) ".org")))
    (f-full filename)))

(defvar xr--edit-buffer-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'xray-note-edit-buffer-cancel)
    (define-key map (kbd "C-c C-c") #'xray-note-edit-buffer-confirm)
    map))

(defun xr-edit-or-add-note (ray)
  (let* ((file (plist-get ray :file))
         (xray-file (xr-xray-file-name file))
         (xray-dir (xr-xray-file-directory file))
         (topic (plist-get ray :topic))
         (id (plist-get ray :id))
         (note-file (plist-get ray :note-file))
         has-note dir)
    (if note-file
        (progn
          (setq note-file (expand-file-name note-file xray-dir))
          (setq has-note t))
      (setq note-file (xr-note-file-name xray-dir topic id)))

    (setq dir (f-dirname note-file))
    (when (not (f-exists-p dir))
      (make-directory dir))

    (find-file-other-window note-file)
    (with-current-buffer (current-buffer)
      (org-mode)
      (outline-show-all)
      (end-of-buffer)
      (xray-note-edit-mode t)
      (xr-edit-set-header-line)
      (set (make-local-variable 'has-note) has-note)
      (set (make-local-variable 'note-file) note-file)
      (set (make-local-variable 'ray) ray))))

(defun xr-edit-set-header-line ()
  "Set header line."
  (setq header-line-format
        (substitute-command-keys
         (concat
          "\\<xray-note-edit-mode-map>"
          "Confirm with `\\[xray-note-edit-buffer-confirm]', "
          "Cancel with `\\[xray-note-edit-buffer-cancel]'. "
          ))))

(defun xr-edit-ray (ray)
  ""
  (let* ((file (plist-get ray :file))
         (xray-file (xr-xray-file-name file))
         (id (plist-get ray :id))
         (desc (plist-get ray :desc))
         (topic (plist-get ray :topic))
         (file-rays (ht-get xr-file-rays file))
         new-desc new-topic topics xray-topics)
    (setq new-topic (read-string "New topic: " topic nil topic))
    (setq new-desc (read-string "New desc: " desc nil desc))

    (xr-update-recent-topics file new-topic)

    (when (or (not (s-equals-p topic new-topic)) (not (s-equals-p desc new-desc)))
      (setq file-rays (cl-remove-if #'(lambda (ray)
                                     (equal id (plist-get ray :id)))
                                 file-rays))
      (setq ray (plist-put ray :topic new-topic))
      (setq ray (plist-put ray :desc new-desc))

      (setq topics (ht-get xr-file-topics file))
      (push new-topic topics)
      (ht-set! xr-file-topics file topics)
      (setq xray-topics (ht-get xr-topics xray-file))
      (push new-topic xray-topics)
      (ht-set! xr-topics xray-file xray-topics)
      
      (push ray file-rays)

      (ht-set! xr-file-rays file file-rays)
      (xr-save-rays xray-file))))


;;; Other 

(defun xr-visible-area-xray-count (&optional file-name)
  (let ((file-name (xr-buffer-file-name))
        (count 0)
        begin-line
        end-line
        page-no xray-line xray-page-no
        xray-file-name xrays)
    (when file-name
      (setq xray-file-name (xr-xray-file-name file-name))
      (setq xrays (xr-rays-in-file file-name))
      (cond
       ((or (derived-mode-p 'text-mode)
            (derived-mode-p 'prog-mode))
        (setq begin-line (line-number-at-pos (window-start)))
        (setq end-line (1- (line-number-at-pos (window-end))))
        (dolist (ray xrays)
          (setq xray-line (plist-get ray :linum))
          (when (and (>= xray-line begin-line)
                     (<= xray-line end-line))
            (setq count (1+ count))
            )))
       ((or
         (eq major-mode 'eww-mode)
         (eq major-mode 'w3m-mode))
        (setq begin-line (line-number-at-pos (window-start)))
        (setq end-line (1- (line-number-at-pos (window-end))))
        (dolist (ray xrays)
          (setq xray-line (plist-get ray :linum))
          (when (and (>= xray-line begin-line)
                     (<= xray-line end-line))
            (setq count (1+ count))
            ))
        )
       ((s-suffix? ".pdf" file-name t)
        (setq page-no (nth 0 (xr-pdf-page-and-percent file-name)))
        
        (dolist (ray xrays)
          (setq xray-page-no (plist-get ray :page))
          (when (= page-no xray-page-no)
            (setq count  (1+ count)))))
       (t
        ;; (user-error (format "%s" "Unsupported file type."))
        (setq count 0)
        ))
      )
    count))

(defun xr-mode-line ()
  (let ((file-name (xr-buffer-file-name))
        (visible-rays-count 0)
        (file-rays-count 0)
        (rays-count 0)
        xray-file-name)
    (if xr-cleared
        (setq visible-rays-count -1
              file-rays-count -1
              rays-count -1)
      (when file-name
        (setq xray-file-name (xr-xray-file-name file-name))
        (setq file-rays-count (length (ht-get xr-file-rays file-name)))
        (mapcar #'(lambda (file) (setq rays-count (+ rays-count (length (ht-get xr-file-rays file)))))
                (xr-files-in-xray xray-file-name))
        (when xr-show-visible-area-xray-count
          (setq visible-rays-count (xr-visible-area-xray-count file-name)))))
    (if xr-show-visible-area-xray-count
        (format "%d:%d:%d" visible-rays-count file-rays-count rays-count)
      (format "%d:%d" file-rays-count rays-count))))

(defvar mode-line-xray-info '(:eval (format "  [R:%s]" (xr-mode-line))))
;; (setq mode-line-xray-info '(:eval (format "  [R:%s]" (xr-mode-line))))
(put 'mode-line-xray-info 'risky-local-variable t)

(provide 'xray)
