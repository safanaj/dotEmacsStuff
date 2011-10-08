;; Global Key Bindings 
(unless (keymapp 'my-prefix-map) (define-prefix-command 'my-prefix-map))
;; Forward declaration
(unless (keymapp 'my-cedet-map) (define-prefix-command 'my-cedet-map))
(unless (keymapp 'my-org-map) (define-prefix-command 'my-org-map))
(unless (keymapp 'my-modes-prefix) (define-prefix-command 'my-modes-prefix))
(message "%s" "Set Global Bindings ...")

(global-set-key "\C-cx" 'my-prefix-map)
(global-set-key "\C-cc" 'my-cedet-map)

(global-set-key [s-f1]
		'(lambda ()(interactive)
		   (or (and
			(bound-and-true-p sr-speedbar-toggle)
			(sr-speedbar-toggle))
		       (and (require 'sr-speedbar "elisp/sr-speedbar.el" t)
			    (sr-speedbar-toggle))
		       (speedbar))))

;; & keymaps

(define-key my-prefix-map "f" 'find-file-at-point)
(define-key my-prefix-map "v" 'view-file-at-point)
(define-key my-prefix-map "b" 'browse-url-at-point)
(define-key my-prefix-map "a" 'find-alternate-file)
;(define-key my-prefix-map "hi" 'infobook-goto-node)
(define-key my-prefix-map "-" 'normal-mode)
(define-key my-prefix-map "k" 'kill-buffer-and-window)
(define-key my-prefix-map "K" 'bury-window)
(define-key my-prefix-map "d" 'find-dired)
(define-key my-prefix-map "c" 'comment-region)
(define-key my-prefix-map "C" 'calendar)
(define-key my-prefix-map "\C-q" 'view-mode)
(define-key my-prefix-map "Z" 'make-frame)
(define-key my-prefix-map "z" 'delete-frame)
(define-key my-prefix-map "t" 'shell)
(define-key my-prefix-map "T" 'toggle-truncate-lines)
(define-key my-prefix-map "P" 'proced)
(define-key my-prefix-map "E" 'eval-buffer)
(define-key my-prefix-map "W" 'woman)
(define-key my-prefix-map "R" 'rmail-other-frame)
(define-key my-prefix-map "G" 'gnus-other-frame)

;; For font in my-frames.el and my-faces.el
(when (featurep 'my-frames)
  (define-key my-prefix-map "^" 'frame-resize-and-position-mode)
  (define-key my-prefix-map "|" 'font-resizing-mode))

;; De/Activation minor modes
(define-key my-prefix-map "M" 'my-modes-prefix)
(define-key my-modes-prefix "d" 'develock-mode)
;(define-key my-modes-prefix "@" 'icy-mode)
(define-key my-modes-prefix "l" 'linkd-mode)
(define-key my-modes-prefix "h" 'folding-mode)
;; (define-key my-modes-prefix "ot" 'orgtbl-mode)
;; (define-key my-modes-prefix "os" 'orgstruct++-mode)
(if (fboundp 'srecode-minor-mode)
    (define-key my-modes-prefix "R" 'srecode-minor-mode))
(if (fboundp 'senator-minor-mode)
    (define-key my-modes-prefix "N" 'senator-minor-mode))
(if (fboundp 'global-ede-mode)
    (define-key my-modes-prefix "P" 'global-ede-mode))
(if (fboundp 'semantic-mode)
    (define-key my-modes-prefix "D" 'semantic-mode))

;; override default
;(global-set-key (kbd "M-f") 'forward-sentence)
;(global-set-key (kbd "M-b") 'backward-sexp)

(global-set-key "\C-cm" 'compile)
(global-set-key "\C-xK" 'bury-buffer)
(global-set-key "\M-_" 'hippie-expand)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-xj" 'dired-jump)

(global-set-key [M-f1] 'help-command)
(defun toggle-ctrl-H-for-backspace () (interactive)
  (if (eq 'help-command (key-binding "\C-h"))
      (global-set-key "\C-h" 'backward-delete-char)
    (global-set-key "\C-h" 'help-command)))

(global-set-key [f1] 'delete-other-windows)
(global-set-key [f11] 'delete-window)
; fix devhelp key
(and (featurep 'devhelp)
     (global-set-key [f8] 'devhelp-word-at-point)
;     (global-set-key [f9] 'devhelp-assistant-word-at-point)
     )
(and (featurep 'imenu) (global-set-key [f9] 'imenu))

;(global-set-key [f2] 'show/switch-other-window)
(global-set-key [f2] 'show/bury-other-window)

(global-set-key [f6] 'speedbar)

(defun get-scratch ()""(interactive)(switch-to-buffer "*scratch*"))
(if window-system
    (progn
      (global-set-key [?\C-<] 'other-window)
      (global-set-key [?\C->] 'other-frame)
      (global-set-key [?\C-'] 'other-window-maximizing)
      (global-set-key [S-f2] 'show/hide-messages)
      (global-set-key [S-f3] 'get-scratch)
      (global-set-key [S-f4] 'show/cycle-compilation/term/shell)
      (global-set-key [S-f1] 'menu-bar-mode)
      (global-set-key [C-S-f1] 'tool-bar-mode)
      (global-set-key [?\C-x C-up] 'cycleframe-forward)
      (global-set-key [?\C-x C-down] 'cycleframe-backward)      )
  (progn
    (global-set-key "\M-n" 'forward-paragraph)
    (global-set-key "\M-p" 'backward-paragraph)
    (global-set-key [?\C-x down] 'cycleframe-backward)
    (global-set-key [?\C-x up] 'cycleframe-forward)
    (global-set-key [f16] 'show/cycle-compilation/term/shell)
    (global-set-key [f15] 'get-scratch)
    (global-set-key [f14] 'show/hide-messages)))

(and window-system
     (progn
       (global-set-key [S-f6] 'toggle-ecb)
       (global-set-key [C-f6] 'ecb-deactivate)
       (global-set-key [f7] 'ecb-toggle-layout)
       (global-set-key (kbd "C-<f7>") 'font-resizing-mode)
       (global-set-key (kbd "C-<f10>") 'un/maximize-frame)
       (global-set-key (kbd "M-S-<f10>") 'set-unmaximized-frame-sizes)
       ))

(global-set-key "\C-x\C-v" 'view-file)
(global-set-key "\C-x4v" 'view-file-other-window)
(global-set-key "\C-x5v" 'view-file-other-frame)
;(global-set-key "\C-c@h" 'folding-mode)
(global-set-key "\C-ct" 'term)
(global-set-key "\C-ce" 'eshell)


;;;;;(global-set-key "\C-h\C-j" 'gtk-lookup-symbol)
(message "done.")


(defsubst reload-my-bindkeys ()
  (interactive) (load-file
		 (expand-file-name
		  "my-bindkeys.el"
		  user-emacs-directory)))

(provide 'my-bindkeys)
