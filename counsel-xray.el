;; counsel-xray.el --- Create topic threads across files.  -*- lexical-binding: t; -*-

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

(require 'f)
(require 'xray)
(require 'ivy)
(require 'compile) ;; compilation-info-face, compilation-line-face

(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function doc-view-goto-page "doc-view")

;;; Read xray and display them

(defvar counsel-xr-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k") 'counsel-xr-delete-ray)
    map))


(defun counsel-xr-delete-ray (cand)
  ""
  (let* ((ray (cdr cand)))
    (xr-delete-ray ray)))

(defun counsel-xr-edit-ray (cand)
  ""
  (let* ((ray (cdr cand)))
    (xr-edit-ray ray)))

(defun counsel-xr-format-ray (ray &optional with-file)
  ""
  (if with-file
      (format "%s - %s  %s"
              (propertize (plist-get ray :topic) 'face compilation-info-face)
              ;; (plist-get ray :topic)
              (plist-get ray :desc)
              (propertize
               (concat "["
                       (f-filename (plist-get ray :file))
                       ":"
                       (number-to-string (or (plist-get ray :linum) (plist-get ray :page)))
                       "]")
               'face compilation-line-face)
              )
    (format "%s - %s  %s"
            (propertize (plist-get ray :topic) 'face compilation-info-face)
            ;; (plist-get ray :topic)
            (plist-get ray :desc)
            (propertize
             (concat ":"
                     (number-to-string (or (plist-get ray :linum) (plist-get ray :page)))
                     )
             'face compilation-line-face)
            )))

(defun counsel-xr-visible-area-rays-collector ()
  ""
  (let ((rays (xr-rays-in-visible-area (xr-buffer-file-name))))
    (mapcar #'(lambda (ray)
                (cons (counsel-xr-format-ray ray) ray))
            rays)))

(defun counsel-xr-file-rays-collector ()
  ""
  (let ((rays (xr-rays-in-file (xr-buffer-file-name))))
    (mapcar #'(lambda (ray)
                (cons (counsel-xr-format-ray ray) ray))
            rays)))

(defun counsel-xr-rays-collector ()
  ""
  (let ((rays (xr-rays (xr-buffer-file-name))))
    (mapcar #'(lambda (ray)
                (cons (counsel-xr-format-ray ray t) ray))
            rays)))

(defun counsel-xr-topic-rays-collector (topic)
  ""
  (let ((rays (xr-rays-of-topic topic)))
    (mapcar #'(lambda (ray)
                (cons (counsel-xr-format-ray ray t) ray))
            rays)))

(defun counsel-xr-doc-get-page-slice ()
  "Return (slice-top . slice-height)."
  (let* ((slice (or (image-mode-window-get 'slice) '(0 0 1 1)))
         (slice-top (float (nth 1 slice)))
         (slice-height (float (nth 3 slice))))
    (when (or (> slice-top 1)
              (> slice-height 1))
      (let ((height (cdr (image-size (image-mode-window-get 'image) t))))
        (setq slice-top (/ slice-top height)
              slice-height (/ slice-height height))))
    (cons slice-top slice-height)))

(defun counsel-xr-conv-page-percentage-scroll (percentage)
  (let* ((slice (counsel-xr-doc-get-page-slice))
         (display-height (cdr (image-display-size (image-get-display-property))))
         (display-percentage (min 1 (max 0 (/ (- percentage (car slice)) (cdr slice)))))
         (scroll (max 0 (floor (* display-percentage display-height)))))
    scroll))

(defun counsel-xr-pdf-view-goto-percent (percent)
  ""
  (image-scroll-up (- (counsel-xr-conv-page-percentage-scroll percent)
                      (window-vscroll))))

(defun counsel-xr-file-rays-jump (cand)
  (let* ((ray (cdr cand))
         (file (plist-get ray :file))
         (type (plist-get ray :type))
         (topic (plist-get ray :topic))
         page linum percent)
    (setq xr-recent-topic topic)
    (cond
     ((s-equals? type "text")
      (setq linum (plist-get ray :linum))
      (goto-line linum)
      (recenter)
      )
     ((s-equals? type "pdf")
      (setq page (plist-get ray :page))
      (setq percent (plist-get ray :percent))

      (cond
       ((eq major-mode 'eaf-mode)
        (if (and percent (not (equal -1 (car percent))))
            (eaf-call "call_function_with_args" eaf--buffer-id
                      "jump_to_percent_with_num" (format "%s" (car percent)))
          (eaf-call "call_function_with_args" eaf--buffer-id
                    "jump_to_page_with_num" page)
          )
        )
       ((eq major-mode 'pdf-view-mode)
        (pdf-view-goto-page page)
        (when (and percent (not (equal -1 (cdr percent))))
          (counsel-xr-pdf-view-goto-percent (cdr percent)))
        )
       ((eq major-mode 'doc-view-mode)
        (doc-view-goto-page page))
       (t
        (message "Unsupported pdf view mode.")))
      )
     ((s-equals? type "html")
      (setq linum (plist-get ray :linum))
      (goto-line linum)
      (recenter)
      ))))

(defun counsel-xr-visible-area-rays ()
  (interactive)
  (let ((rays (counsel-xr-visible-area-rays-collector)))
    (ivy-read "Rays: " rays
              :action '(1
                        ("o" counsel-xr-file-rays-jump "jump to ray")
                        ("d" counsel-xr-delete-ray "delete a ray")
                        ("e" counsel-xr-edit-ray "edit a ray's desc")
                        )
              :caller 'counsel-xr-visible-area-rays
              )))


(defun counsel-xr-file-rays ()
  (interactive)
  (let ((rays (counsel-xr-file-rays-collector)))
    (ivy-read "Rays: " rays
              :action '(1
                        ("o" counsel-xr-file-rays-jump "jump to ray")
                        ("d" counsel-xr-delete-ray "delete a ray")
                        ("e" counsel-xr-edit-ray "edit a ray's desc")
                        )
              :caller 'counsel-xr-file-rays
              )))

(defun counsel-xr-rays-jump (cand)
  (let* ((ray (cdr cand))
         (file (plist-get ray :file))
         (type (plist-get ray :type))
         (topic (plist-get ray :topic))
         page linum percent)
    (setq xr-recent-topic topic)
    (cond
     ((s-equals? type "text")
      (find-file file)
      (setq linum (plist-get ray :linum))
      (goto-line linum)
      (recenter)
      )
     ((s-equals? type "pdf")
      (setq page (plist-get ray :page))
      (setq percent (plist-get ray :percent))

      (cond
       ((eq xr-open-pdf-with-eaf t)
        (eaf-open file "pdf-viewer")
        (if (and percent (not (equal -1 (car percent))))
            (eaf-call "call_function_with_args" eaf--buffer-id
                      "jump_to_percent_with_num" (format "%s" (car percent)))
          (eaf-call "call_function_with_args" eaf--buffer-id
                    "jump_to_page_with_num" page))
        )
       (t
        (find-file file)
        (cond
         ((eq major-mode 'pdf-view-mode)
          (pdf-view-goto-page page)
          (when (and percent (not (equal -1 (cdr percent))))
            (counsel-xr-pdf-view-goto-percent (cdr percent)))
          )
         ((eq major-mode 'doc-view-mode)
          (doc-view-goto-page page)
          )
         (t
          (user-error "Unsupported pdf view mode.")
          )))))
     ((s-equals? type "html")
      (eww (s-concat "file://" file))
      (setq linum (plist-get ray :linum))
      (goto-line linum)
      (recenter)
      ))))

(defun counsel-xr-rays ()
  (interactive)
  (let ((rays (counsel-xr-rays-collector)))
    (ivy-read "Rays: " rays
              :action '(1
                        ("o" counsel-xr-rays-jump "jump to ray")
                        ("d" counsel-xr-delete-ray "delete a ray")
                        ("e" counsel-xr-edit-ray "edit a ray's desc")
                        )
              :caller 'counsel-xr-rays
              ))
  )

(defun counsel-xr-topic-rays ()
  (interactive)
  (let (topic
        rays)
    (setq topic (completing-read "Select a topic: " (xr-topics)))
    (message "topic %s" topic)
    (setq rays (counsel-xr-topic-rays-collector topic))
    (ivy-read "Rays: " rays
              :action '(1
                        ("o" counsel-xr-rays-jump "jump to ray")
                        ("d" counsel-xr-delete-ray "delete a ray")
                        ("e" counsel-xr-edit-ray "edit a ray's desc")
                        )
              :caller 'counsel-xr-topic-rays
              )))

(defun counsel-xr-topic-xray-sorter (&optional l r)
  (let* ((lray (cdr l))
         (rray (cdr r))
         (ltopic (plist-get lray :topic))
         (rtopic (plist-get rray :topic))
         (lf (f-filename (plist-get lray :file)))
         (rf (f-filename (plist-get rray :file)))
         (lpos (or (plist-get lray :linum) (plist-get lray :page)))
         (rpos (or (plist-get rray :linum) (plist-get rray :page))))
    (cond
     ((and (equal ltopic xr-recent-topic) (equal rtopic xr-recent-topic))
      (cond
       ((equal lf rf)
        (< lpos rpos))
       ((string< lf rf)
        t)
       (t
        nil)))
     ((equal ltopic xr-recent-topic)
      t)
     ((equal rtopic xr-recent-topic)
      nil)
     ((equal ltopic rtopic)
      (cond
       ((equal lf rf)
        (< lpos rpos))
       ((string< lf rf)
        t)
       (t
        nil)))
     ((string< ltopic rtopic)
      t)
     (t
      nil))))

(ivy-configure 'counsel-xr-topic-rays
  :sort-fn #'counsel-xr-topic-xray-sorter)

(defun counsel-xr-xray-sorter (&optional l r)
  (let* ((lray (cdr l))
         (rray (cdr r))
         (ltopic (plist-get lray :topic))
         (rtopic (plist-get rray :topic))
         (lpos (or (plist-get lray :linum) (plist-get lray :page)))
         (rpos (or (plist-get rray :linum) (plist-get rray :page))))
    (cond
     ((and (equal ltopic xr-recent-topic) (equal rtopic xr-recent-topic))
      (< lpos rpos))
     ((equal ltopic xr-recent-topic)
      t)
     ((equal rtopic xr-recent-topic)
      nil)
     ((equal ltopic rtopic)
      (< lpos rpos))
     ((string< ltopic rtopic)
      t)
     (t
      nil))))

(ivy-configure 'counsel-xr-visible-area-rays
  :sort-fn #'counsel-xr-xray-sorter)
(ivy-configure 'counsel-xr-file-rays
  :sort-fn #'counsel-xr-xray-sorter)
(ivy-configure 'counsel-xr-rays
  :sort-fn #'counsel-xr-xray-sorter)



(provide 'counsel-xray)
