;;; horn.el --- Call your favorite buffer in no time -*- lexical-binding: t -*-

;; Author: Jackson Benete Ferreira <jacksonbenete@gmail.com>
;; URL: https://github.com/jacksonbenete/horn.el
;; Package-Version: xx
;; Package-Commit: xx
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; The MIT License (MIT)
;;
;; Copyright (c) 2016 Al Scott
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Select your favorite mode or buffer from a pre-defined list.
;; This way you can avoid defining too much keybindings and you can have a list
;; of useful modes always in hand/

;;; Code:

;; -------------------------------------------------------------------

(defgroup horn nil
  "Call a mode or buffer easily."
  :group 'emacs)

(defcustom horn-default-list 
  '(("Shell" . shell)
    ("Eshell" . eshell)
    ("Magit" . magit))
  "Commonly used modes or functions to be called."
  ;; :type '(alist :key-type string :value-type symbol)
  :type '(alist :key-type string :value-type string)
  :group 'horn)

;; TODO: recover buffer-list and organize it like ibuffer
(defun horn-call-buffer ()
  "List the current oppened buffers to be called."
  (interactive)
  ;; customization
  (speedbar-frame-mode)
  (speedbar-change-initial-expansion-list "buffers")
  (speedbar-refresh)
  (message "horn-call-buffer"))
(global-set-key (kbd "C-x C-b") 'horn-call-buffer)

(defun horn-call-mode ()
  "Select a mode from defined list to be called."
  (interactive)
  (let* ((key (completing-read "Call: " horn-default-list nil nil "^"))
	 (val (alist-get key horn-default-list nil nil #'string=)))
    (message "Horn called mode or function: %s" key)
    (funcall val)))

(provide 'horn)
;;; end of file horn.el
