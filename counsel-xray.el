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

;;; Read xray and display them

(defun counsel-xr-format-ray (ray)
  ""
  (format "%s - %s  %s"
          (plist-get ray :topic)
          (plist-get ray :desc)
          (f-filename (plist-get ray :file))
          )
  )

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
                  (cons (counsel-xr-format-ray ray) ray))
            rays)))

(defun counsel-xr-file-rays ()
  (interactive)
  (let ((cb (current-buffer))
        (rays (counsel-xr-file-rays-collector)))
    (ivy-read "Rays: " rays
              :action #'(lambda (cand)
                          (let* ((ray (cdr cand))
                                 (file (plist-get ray :file))
                                 (type (plist-get ray :type))
                                 viewer page linum)
                            (cond
                             ((s-equals? type "text")
                              (setq linum (plist-get ray :linum))
                              (goto-line linum)
                              (recenter)
                              )
                             ((s-equals? type "pdf")
                              (setq page (plist-get ray :page))
                              (cond
                               ((eq major-mode 'eaf-mode)
                                (eaf-call "call_function_with_args" eaf--buffer-id
                                          "jump_to_page_with_num" page)
                                )
                               ((eq major-mode 'pdf-view-mode)
                                (pdf-view-goto-page page))
                               (t
                                (message "Unsupported major mode.")))
                              )
                             ((s-equals? type "html")
                              (setq linum (plist-get ray :linum))
                              (goto-line linum)
                              (recenter)
                              ))))
              :caller 'counsel-xr-file-rays
              ))
  )

(defun counsel-xr-rays ()
  (interactive)
  (let ((cb (current-buffer))
        (rays (counsel-xr-rays-collector)))
    (ivy-read "Rays: " rays
              :action #'(lambda (cand)
                          (let* ((ray (cdr cand))
                                 (file (plist-get ray :file))
                                 (type (plist-get ray :type))
                                 viewer page linum)
                            (cond
                             ((s-equals? type "text")
                              (find-file file)
                              (setq linum (plist-get ray :linum))
                              (goto-line linum)
                              (recenter)
                              )
                             ((s-equals? type "pdf")
                              (setq page (plist-get ray :page))
                              (setq viewer (plist-get ray :viewer))
                              (cond
                               ((equal viewer "eaf-open")
                                (eaf-open file "pdf-viewer")
                                (eaf-call "call_function_with_args" eaf--buffer-id
                                          "jump_to_page_with_num" page))
                               (t
                                (find-file file)
                                (pdf-view-goto-page page))))
                             ((s-equals? type "html")
                              (eww (s-concat "file://" file))
                              (setq linum (plist-get ray :linum))
                              (goto-line linum)
                              (recenter)
                              ))))
              :caller 'counsel-xr-rays
              ))
  )

(provide 'counsel-xray)