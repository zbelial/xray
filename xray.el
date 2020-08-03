;; xray.el --- Create topic threads across files.  -*- lexical-binding: t; -*-

;; Filename: xray.el
;; Description: Topic threads across files.
;; Author:  zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer:  zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2020, zbelial, all rights reserved.
;; Created: 2020-07-30 23:08:12
;; Version: 0.1
;; URL: https://github.com/zbelial/xray.el
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

;; TODO
;; 1. counsel列表格式 TEMP
;; 2. ray的desc DONE
;; 3. 记录文件相对路径
;; 4. 当前文件的ray数量显示在modeline DONE
;; 5. html相关 DONE
;; 6. context

(require 'ht)
(require 's)
(require 'f)

;;; Custom
(defcustom xr-default-dir user-emacs-directory
  "The default directory used to store topic thread data."
  :type  'string
  :group 'xray)

(defcustom xr-save-to-default-dir nil
  "If non-nil, topic thread data will be saved to `xr-default-dir'.
Otherwise, the data will be saved to current directory."
  :type 'boolean
  :group 'xray)

(defcustom xr-default-file-name "xray-data.el"
  "The default file name used to store thread data."
  :type  'string
  :group 'xray)

;;; Variables
(defvar xr-file-map (ht-create)
  "Keep track of the xray files of opened files.
key: raw file name, value: xray file name.")

(defvar xr-file-rays (ht-create)
  "Which rays each file has.
key: raw file name, value: rays")

(defvar xr-rays (ht-create)
  "Which rays are stored in each xray file.
key: xray file name, value: rays.")

(defvar xr-topic-rays (ht-create)
  "Which rays have a particular topic.
key: topic, value: rays")

(defvar xr-topics (ht-create)
  "Which topics are stored in each xray file.
key: xray file name, value: topics")

(defvar xr-file-topics (ht-create)
  "Which topics are stored in each file.
key: raw file name, value: topics")

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
(defun xr-current-line-number ()
  ""
  (line-number-at-pos)
  )

(defun xr-new-ray-text-or-prog(file-name xray-file-name)
  "Create a new ray."
  (let ((topic (xr-select-or-add-topic file-name xray-file-name))
        (desc (xr-add-desc))
        (linum (xr-current-line-number))
        (context ""))
    (list :id (xr-id) :type "text" :file file-name :topic topic :desc desc :linum linum :context context)
    ))

(defun xr-new-ray-pdf(file-name xray-file-name)
  "Create a new ray."
  (cond
   ((eq major-mode 'eaf-mode)
    (let* ((app eaf--buffer-app-name)
           page topic desc
           )
      (if (string-equal app "pdf-viewer")
          (progn
            (setq page (string-to-number (eaf-call "call_function" eaf--buffer-id "current_page")))
            (setq topic (xr-select-or-add-topic file-name xray-file-name))
            (setq desc (xr-add-desc))

            (list :id (xr-id) :type "pdf" :file file-name :topic topic :desc desc :page page :viewer "eaf-open" :context "")
            )
        (message "Not an eaf pdf viewer buffer.")))
    )
   ((eq major-mode 'pdf-view-mode)
    (setq page (pdf-view-current-page))
    (setq topic (xr-select-or-add-topic file-name xray-file-name))
    (setq desc (xr-add-desc))

    (list :id (xr-id) :type "pdf" :file file-name :topic topic :desc desc :page page :context "")
    )
   (t
    (user-error "%s" "Unsupported mode."))
   )
  )

(defun xr-new-ray-html(file-name xray-file-name)
  "Create a new ray."
  (let ((topic (xr-select-or-add-topic file-name xray-file-name))
          (desc (xr-add-desc))
          (linum (xr-current-line-number))
          (context ""))
      (list :id (xr-id) :type "html" :file file-name :topic topic :desc desc :linum linum :context context))
  )

(defun xr-xray-file-name (&optional file-name)
  "Find the xray data file of current file."
  (let ((file-name (or file-name (xr-buffer-file-name)))
        xray-file-name xray-file-dir)
    (when file-name
      (setq xray-file-name (ht-get xr-file-map file-name))
      (when (not xray-file-name)
        (setq xray-file-dir (locate-dominating-file file-name xr-default-file-name))
        (if xray-file-dir
            (setq xray-file-name (concat xray-file-dir xr-default-file-name))
          (setq xray-file-name (concat xr-default-dir xr-default-file-name)))
        (ht-set! xr-file-map file-name xray-file-name))

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

(defun xr-add-ray()
  "Create a new ray and add it to xray file."
  (interactive)
  (let* ((file-name (xr-buffer-file-name))
         xray-file-name rays
         ray)
    (if (and
         file-name
         (file-exists-p file-name))
        (progn
          ;; (message "xr-add-ray file-name: %s" file-name)
          (setq xray-file-name (xr-xray-file-name file-name))
          ;; (message "xray-file-name: %s" xray-file-name)

          (xr-load-data-ensure xray-file-name)

          (setq ray (xr-new-ray file-name xray-file-name))
          (when ray
            (when (not (ht-contains-p xr-file-rays file-name))
              (ht-set! xr-file-rays file-name '())
              (ht-set! (ht-get xr-rays xray-file-name) file-name '())
              )

            (setq rays (ht-get xr-file-rays file-name))
            (add-to-list 'rays ray)
            (ht-set! xr-file-rays file-name rays)
            (ht-set! (ht-get xr-rays xray-file-name) file-name rays)

            (xr-save-rays xray-file-name)
            )
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

(defun xr-add-desc ()
  "Add a desc to ray."
  (read-string "Add an Description: " "")
  )

(defun xr-select-or-add-topic (file-name xray-file-name)
  "Select from the existing titles or create a new one."
  (let* ((topics (append (ht-values (ht-get xr-topics xray-file-name))))
         (topic (completing-read "Select or create a topic: " topics))
         desc)
    (when (not (member topic topics))
      (add-to-list 'topics topic)
      (ht-set! (ht-get xr-topics xray-file-name) file-name topics))
    topic))

;;; Load xray data
(defun xr-clear (&optional file-name)
  ""
  (interactive)
  (let* ((file-name (or file-name (xr-buffer-file-name)))
        (xray-file-name (xr-xray-file-name file-name)))
    (ht-clear! xr-rays)
    (ht-clear! xr-topics)
    (ht-clear! xr-file-rays)
    (ht-clear! xr-file-topics)
    (ht-clear! xr-file-map)
    )
  )

(defun xr-load-data-ensure (xray-file-name)
  ""
  (when (not (ht-contains-p xr-rays xray-file-name))
    (xr-load-data xray-file-name)))

(defun xr-load-data (xray-file-name)
  "Load xray file."
  (xr-with-message-suppression
   (when (f-exists-p xray-file-name)
     (load-file xray-file-name)))

  (if (not (ht-contains-p xr-rays xray-file-name))
      (ht-set! xr-rays xray-file-name (ht-create)))
  (if (not (ht-contains-p xr-topics xray-file-name))
      (ht-set! xr-topics xray-file-name (ht-create)))
  )

(defun xr-recover-file-rays(data)
  "Restore xray data."
  (let ((file (plist-get data :file))
        (xray-file (plist-get data :xray-file-name))
        (rays (plist-get data :rays))
        )
    (when (not (ht-contains-p xr-rays xray-file))
      (ht-set! xr-rays xray-file (ht-create)))
    (when (not (ht-contains-p xr-topics xray-file))
      (ht-set! xr-topics xray-file (ht-create)))
    
    (ht-set! (ht-get* xr-rays xray-file) file '())
    (ht-set! (ht-get* xr-topics xray-file) file '())
    (ht-set! xr-file-rays file '())
    (ht-set! xr-file-topics file '())

    (ht-set! xr-file-map file xray-file)

    (let (file-topics topic-rays file-rays)
      (dolist (ray rays)
        (setq ray (plist-put ray :file file))
        (setq topic (plist-get ray :topic))
        (setq topic-rays (ht-get xr-topic-rays topic))
        (when (not (member topic file-topics))
          (add-to-list 'file-topics topic))
        (add-to-list 'topic-rays ray)
        (add-to-list 'file-rays ray)
        (ht-set! xr-topic-rays topic topic-rays)
        )

      (ht-set! xr-file-topics file file-topics)
      (ht-set! xr-file-rays file file-rays)
      (ht-set! (ht-get xr-topics xray-file) file file-topics)
      (ht-set! (ht-get xr-rays xray-file) file file-rays)
      )
    )
  )

;;; Save xray to file.
(defun xr-save-data (&optional file-name)
  (let ((file-name (or file-name (xr-buffer-file-name))))
    (when file-name
      (let ((xray-file-name (xr-xray-file-name file-name)))
        (xr-save-thread file-name (ht-get xr-file-rays xray-file-name)))))
  )

(defun xr-format-ray (ray)
  ""
  (let ((type (plist-get ray :type)))
    (cond
     ((s-equals? type "text")
      (format "\(:id %d :type \"%s\" :topic \"%s\" :linum %d :context \"%s\")"
              (plist-get ray :id)
              (plist-get ray :type)
              (plist-get ray :topic)
              (plist-get ray :linum)
              (plist-get ray :context))
      )
     ((s-equals? type "pdf")
      (format "\(:id %d :type \"%s\" :topic \"%s\" :page %d :viewer \"%s\" :context \"%s\")"
              (plist-get ray :id)
              (plist-get ray :type)
              (plist-get ray :topic)
              (plist-get ray :page)
              (plist-get ray :viewer)
              (plist-get ray :context))
      )
     ((s-equals? type "html")
      (format "\(:id %d :type \"%s\" :topic \"%s\" :linum %d :context \"%s\")"
              (plist-get ray :id)
              (plist-get ray :type)
              (plist-get ray :topic)
              (plist-get ray :linum)
              (plist-get ray :context))
      )
     (t
      (user-error "%s - %s" "Invalid ray type" type))
     ))
  )

(defun xr-save-file-rays (file-name &optional rays)
  ""
  (let ((xray-file-name (xr-xray-file-name file-name)))
    (f-append-text
     (format "\(xr-recover-file-rays '\(
:file \"%s\"
:xray-file-name \"%s\"
:rays \(
%s
)))\n\n" file-name xray-file-name (mapconcat #'xr-format-ray rays "\n"))
     'utf-8-unix
     xray-file-name
     )
    )
  )

(defun xr-save-rays (xray-file-name &optional file-rays)
  ""
  (let ((file-rays (or file-rays (ht-get xr-rays xray-file-name))))
    (with-temp-file xray-file-name
      (erase-buffer)
      (insert ";;; -*- mode: emacs-lisp -*-\n")
      )
    (ht-map #'xr-save-file-rays file-rays)
    )
  )

(defun xr-save-all-rays ()
  "Save thread data."
  (interactive)
  (ht-map #'xr-save-rays xr-rays)
  )


;;; Read xray and display them

(defun xr-topics-in-file (&optional file-name)
  ""
  (interactive)
  (let* ((file-name (or file-name (xr-buffer-file-name)))
         xray-file-name)
    (when file-name
      (xr-load-data-ensure (xr-xray-file-name file-name))
      (ht-get xr-file-topics file-name))
    )
  )

(defun xr-topics (&optional file-name)
  ""
  (interactive)
  (let ((file-name (or file-name (xr-buffer-file-name)))
        xray-file-name)
    (when file-name
      (xr-load-data-ensure (xr-xray-file-name file-name))
      (ht-get xr-topics xray-file-name))
    )
  )

(defun xr-rays-in-file (&optional file-name)
  ""
  (interactive)
  (let ((file-name (or file-name (xr-buffer-file-name))))
    (when file-name
      (xr-load-data-ensure (xr-xray-file-name file-name))
      (ht-get xr-file-rays file-name)
      )))

(defun xr-rays (&optional file-name)
  ""
  (interactive)
  (let ((file-name (or file-name (xr-buffer-file-name)))
        xray-file-name file-rays all-rays)
    (when file-name
      (setq xray-file-name (xr-xray-file-name file-name))
      ;; (message "xr-rays %s" xray-file-name)

      (xr-load-data-ensure xray-file-name)
      (setq all-rays '())
      (setq file-rays (ht-get xr-rays xray-file-name))
      (ht-map #'(lambda (k rays)
                  (dolist (ray rays)
                    ;; (message "ray %S" ray)
                    (add-to-list 'all-rays ray))
                  )
              file-rays)
      all-rays)))

;;; Other 
(defun xr-mode-line ()
  (let ((file-name (xr-buffer-file-name))
        (file-rays-count 0)
        (rays-count 0)
        xray-file-name)
    (when file-name
      (setq xray-file-name (xr-xray-file-name file-name))
      (setq file-rays-count (length (ht-get xr-file-rays file-name)))
      
      (ht-map #'(lambda (k v)
                  (setq rays-count (+ rays-count (ht-size v))))
              xr-rays))
    (format "%d:%d" file-rays-count rays-count)))

(defvar mode-line-xray-info '(:eval (format "  [R:%s]" (xr-mode-line))))
;; (setq mode-line-xray-info '(:eval (format "  [R:%s]" (xr-mode-line))))
(put 'mode-line-xray-info 'risky-local-variable t)

;; (add-to-list 'mode-line-format 'mode-line-xray-info t)

;;; Hook
(add-hook 'kill-emacs-hook #'xr-save-all-rays)

(provide 'xray)
