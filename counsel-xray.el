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
(require 'seq)

(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function pdf-view-image-size "ext:pdf-view")
(declare-function doc-view-goto-page "doc-view")

;;; Read xray and display them

(defun counsel-xr-delete-ray (cand)
  ""
  (let* ((ray (cdr cand)))
    (xr-delete-ray ray)))

(defun counsel-xr-edit-ray (cand)
  ""
  (let* ((ray (cdr cand)))
    (xr-edit-ray ray)))

(defun counsel-xr-move-ray (cand)
  ""
  (let* ((ray (cdr cand)))
    (xr-move-ray ray)))

(defun counsel-xr-edit-or-add-note (cand)
  ""
  (let* ((ray (cdr cand)))
    (xr-edit-or-add-note ray)))

(defun counsel-xr-format-ray (ray &optional with-file)
  ""
  (let ((note-file (plist-get ray :note-file))
        (face compilation-info-face))
    (when note-file
      (setq face compilation-error-face))
    (if with-file
        (format "%s - %s  %s"
                (propertize (plist-get ray :topic) 'face face)
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
              (propertize (plist-get ray :topic) 'face face)
              ;; (plist-get ray :topic)
              (plist-get ray :desc)
              (propertize
               (concat ":"
                       (number-to-string (or (plist-get ray :linum) (plist-get ray :page)))
                       )
               'face compilation-line-face)
              ))))

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
            (counsel-xr-sort-rays rays (xr-recent-topics (xr-buffer-file-name))))))

(defun counsel-xr-rays-collector ()
  ""
  (let ((rays (xr-rays (xr-buffer-file-name))))
    (mapcar #'(lambda (ray)
                (cons (counsel-xr-format-ray ray t) ray))
            (counsel-xr-sort-rays rays (xr-recent-topics (xr-buffer-file-name))))))

(defun counsel-xr-topic-rays-collector (topic)
  ""
  (let ((rays (xr-rays-of-topic topic)))
    (mapcar #'(lambda (ray)
                (cons (counsel-xr-format-ray ray t) ray))
            (counsel-xr-sort-rays rays (xr-recent-topics (xr-buffer-file-name))))))

(defun counsel-xr-pdf-view-goto-percent (percent)
  (let ((size (pdf-view-image-size t)))
    (image-set-window-vscroll
     (round (/ (* percent (cdr size))
               (if pdf-view-have-image-mode-pixel-vscroll
                   1
                 (frame-char-height)))))))

(defun counsel-xr-file-rays-jump (cand)
  (let* ((ray (cdr cand))
         (file (plist-get ray :file))
         (type (plist-get ray :type))
         (topic (plist-get ray :topic))
         page linum percent)
    (xr-update-recent-topics file topic)
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
          (counsel-xr-pdf-view-goto-percent (/ (cdr percent) 100.0)))
        )
       ((eq major-mode 'doc-view-mode)
        (doc-view-goto-page page))
       (t
        (user-error "Unsupported pdf view mode.")))
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
                        ("n" counsel-xr-edit-or-add-note "edit ray's note or add a note to it")
                        ("v" counsel-xr-move-ray "move ray to a new position")
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
                        ("n" counsel-xr-edit-or-add-note "edit ray's note or add a note to it")
                        ("v" counsel-xr-move-ray "move ray to a new position")
                        )
              :caller 'counsel-xr-file-rays
              )))

(defun counsel-xr-existed-eww-buffer (file)
  (let ((buffers (buffer-list))
        target)
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when (and
               (eq major-mode 'eww-mode)
               (equal file (xr-buffer-file-name)))
          (setq target buffer))))
    target))

(defun counsel-xr-rays-jump (cand)
  (let* ((ray (cdr cand))
         (file (plist-get ray :file))
         (type (plist-get ray :type))
         (topic (plist-get ray :topic))
         page linum percent)
    (xr-update-recent-topics file topic)
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
            (counsel-xr-pdf-view-goto-percent (/ (cdr percent) 100.0)))
          )
         ((eq major-mode 'doc-view-mode)
          (doc-view-goto-page page)
          )
         (t
          (user-error "Unsupported pdf view mode.")
          )))))
     ((s-equals? type "html")
      (setq linum (plist-get ray :linum))
      (let ((buffer (counsel-xr-existed-eww-buffer file))
            )
        (if buffer
            (progn
              (switch-to-buffer buffer)
              (goto-line linum)
              (recenter))
          (eww (s-concat "file://" file) 4)
          (goto-line linum)
          (recenter)))))))

(defun counsel-xr-rays ()
  (interactive)
  (let ((rays (counsel-xr-rays-collector)))
    (ivy-read "Rays: " rays
              :action '(1
                        ("o" counsel-xr-rays-jump "jump to ray")
                        ("d" counsel-xr-delete-ray "delete a ray")
                        ("e" counsel-xr-edit-ray "edit a ray's desc")
                        ("n" counsel-xr-edit-or-add-note "edit ray's note or add a note to it")
                        )
              :caller 'counsel-xr-rays
              ))
  )

(defun counsel-xr-topic-rays ()
  (interactive)
  (let (topic
        rays)
    (setq topic (completing-read "Select a topic: " (xr-topics)))
    ;; (message "topic %s" topic)
    (setq rays (counsel-xr-topic-rays-collector topic))
    (ivy-read "Rays: " rays
              :action '(1
                        ("o" counsel-xr-rays-jump "jump to ray")
                        ("d" counsel-xr-delete-ray "delete a ray")
                        ("e" counsel-xr-edit-ray "edit a ray's desc")
                        ("n" counsel-xr-edit-or-add-note "edit ray's note or add a note to it")
                        )
              :caller 'counsel-xr-topic-rays
              )))

(defun counsel-xr-sort-filter (rays topic)
  (let (filtered
        rt)
    (dolist (ray rays)
      (setq rt (plist-get ray :topic))
      (when (s-equals-p rt topic)
        (add-to-list 'filtered ray)))
    filtered))

;; FIXME not efficient
(defun counsel-xr-sort-rays (rays recent-topics &optional cmp-file)
  (if recent-topics
      (let ((rrays rays)
            sorted
            fr)
        (dolist (topic recent-topics)
          (setq fr (counsel-xr-sort-filter rrays topic))
          (when fr
            (setq rrays (seq-filter #'(lambda (ray) (not (s-equals-p (plist-get ray :topic) topic))) rrays))
            (setq sorted (append sorted
                                 (if cmp-file
                                     (sort fr #'counsel-xr-topic-xray-sorter)
                                   (sort fr #'counsel-xr-xray-sorter))))))
        (append sorted (if cmp-file
                           (sort rrays #'counsel-xr-topic-xray-sorter)
                         (sort rrays #'counsel-xr-xray-sorter))))
    (sort rays #'counsel-xr-topic-xray-sorter))
  )

(defun counsel-xr-topic-xray-sorter (&optional lray rray)
  (let* ((ltopic (plist-get lray :topic))
         (rtopic (plist-get rray :topic))
         (lf (f-filename (plist-get lray :file)))
         (rf (f-filename (plist-get rray :file)))
         (lpos (or (plist-get lray :linum) (plist-get lray :page)))
         (rpos (or (plist-get rray :linum) (plist-get rray :page)))
         )
    (cond
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

(defun counsel-xr-xray-sorter (&optional lray rray)
  (let* ((ltopic (plist-get lray :topic))
         (rtopic (plist-get rray :topic))
         (lpos (or (plist-get lray :linum) (plist-get lray :page)))
         (rpos (or (plist-get rray :linum) (plist-get rray :page)))
         )
    (cond
     ((equal ltopic rtopic)
      (< lpos rpos))
     ((string< ltopic rtopic)
      t)
     (t
      nil))))

(provide 'counsel-xray)
