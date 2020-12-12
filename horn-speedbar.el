(require 'speedbar)

(defgroup horn-speedbar nil
  "Call a speedbar to show ordened buffers."
  :group 'speedbar)


;; Speedbar
;; -------------------------------------------------------------------
;; [https://www.gnu.org/software/emacs/manual/html_node/speedbar/Extending.html]
;; Three functions are needed to create a speedbar implementation
;; a keymap, a install function, and the speedbar buttons
;; then you need a list compatible with easymenu

(defvar horn-speedbar-key-map nil "Keymap for `horn-buffer-mode'")

(defun horn-install-speedbar-variables ()
  (progn
    (setq horn-speedbar-key-map (speedbar-make-specialized-keymap))
    (define-key horn-speedbar-key-map (kbd "C-c C-b")
      '(lambda () (message "pressed C-c C-b")))

    ;; Extending speedbar
    (speedbar-add-expansion-list '("Horn Buffer List"
				   horn-speedbar-menu-items
				   horn-speedbar-key-map
				   horn-speedbar-buttons))

    
    ;; (speedbar-add-mode-functions-list
    ;;  '("Horn"
    ;;    (speedbar-item-info . horn-speedbar-item-info)
    ;;    (speedbar-line-directory . horn-speedbar-line-directory)))
    ))

(if (featurep 'speedbar)
    (horn-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'horn-install-speedbar-variables))

(easy-menu-define horn-speedbar-menu-items horn-speedbar-key-map
  "Creates a menu list organized just like ibuffer"
  '("My Stuff"
    ["One entry" my-function t]
    ("Sub Menu"
     ["My subentry" my-obscure-function t])))

(defun horn-speedbar-buttons (dir depth))


;; -------------------------------------------------------------------

