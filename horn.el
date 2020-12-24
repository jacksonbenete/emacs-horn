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

;;; Parts of code of lusty-explorer where used.
;; -------------------------------------------------------------------
;; Copyright (C) 2008-2020 Stephen Bach
;;
;; Version: 3.2
;; Keywords: convenience, files, matching, tools
;; URL: https://github.com/sjbach/lusty-emacs
;; Package-Requires: ((emacs "25.1"))
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.
;; -------------------------------------------------------------------

;;; Commentary:
;; Select your favorite mode from a pre-defined list or select
;; a buffer from a concise list.
;; This way you can avoid defining too much keybindings and you can have a list
;; of useful modes always in hand.
;;
;; LustyExplorer provides useful functions to recover buffers.
;; The function names lusty-<function> are unmodified.

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

(defvar horn-literate-p nil
  "If horn-literate-p is non-nill it will append ^ at the beginning
of the caller functions.")

;; LustyExplorer Code
;; -------------------------------------------------------------------
(defun lusty-buffer-list ()
  "Return a list of buffers ordered with those currently visible at the end."
  (let ((visible-buffers '()))
    (walk-windows
     (lambda (window)
       ;; Add visible buffers
       (let ((b (window-buffer window)))
         (unless (memq b visible-buffers)
           (push b visible-buffers))))
     nil 'visible)
    (let ((non-visible-buffers
           (cl-loop for b in (buffer-list (selected-frame))
                    unless (memq b visible-buffers)
                    collect b)))
      (nconc non-visible-buffers visible-buffers))))

(defun lusty-filter-buffers (buffers)
  "Return BUFFERS converted to strings with hidden buffers removed."
  (cl-macrolet ((ephemeral-p (name)
                  `(eq (string-to-char ,name) ?\ )))
    (cl-loop for buffer in buffers
             for name = (buffer-name buffer)
             unless (ephemeral-p name)
             collect (copy-sequence name))))

;; (lusty-filter-buffers (lusty-buffer-list))
;; -------------------------------------------------------------------

(defun make-plist (x)
  "Make a plist of mirrored args.
The key will be a substring without *'s if the buffer name is *<name>*.
The first buffer on list is always the last visited buffer."
  (let ((element (car x)))
    (if (null x)
	x
      (cons (cons (if (equal (substring element -1) "*")
		      (substring element 1 -1)
		    element) element)
	    (funcall #'make-plist (cdr x))))))

;; TODO: recover buffer-list and organize it like ibuffer
;;;###autoload
(defun horn-call-buffer ()
  "List the current opened buffers to be called."
  (interactive)
  (let* ((visible-buffers (lusty-filter-buffers (lusty-buffer-list)))
	 (visible-buffers-plist (make-plist visible-buffers))
	 (key (completing-read "Buffer: "
			       visible-buffers-plist nil nil
			       (when horn-literate-p "^")))
	 (val (alist-get key visible-buffers-plist nil nil #'string=)))
    (message "Horn called buffer: %s" val)
    (switch-to-buffer val)))
(global-set-key (kbd "C-x C-b") 'horn-call-buffer)

;;;###autoload
(defun horn-call-mode ()
  "Select a mode from defined list to be called."
  (interactive)
  (let* ((key (completing-read "Call: " horn-default-list nil nil
			       (when horn-literate-p "^")))
	 (val (alist-get key horn-default-list nil nil #'string=)))
    (message "Horn called mode or function: %s" key)
    (funcall val)))

(provide 'horn)
;;; end of file horn.el
