
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TO USE EMACS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Maintain a good .emacs.d
(unless (bound-and-true-p user-emacs-directory)
  (setq user-emacs-directory (expand-file-name "~/.emacs.d/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reload-dot-emacs() "Reolad `user-init-file'."
  (interactive)(eval-buffer (find-file-noselect user-init-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Compile ~/.emacs.d/*.el ...
(defun byte-compile-in-user-emacs-directory (&optional not-all)
  "Compile some elisp files in `user-emacs-directory'."
  (interactive "p")
  (let ((files)
	(_cmd (if (and (numberp not-all) (> not-all 0))
		  "ls -1 *.el |grep -v -e '^my\\|^mail-and-gnus\\|^making'"
		"ls -1 *.el")))
    (save-excursion
      (cd user-emacs-directory)
      (setq files (split-string (shell-command-to-string _cmd)))
      (while (car files)
	(byte-compile-file (car files))
	(setq files (cdr files))))))

(defun clean-user-emacs-directory ()
  "Remove byte-compiled files from `user-emacs-directory'."
  (interactive)
  (shell-command (concat "rm " user-emacs-directory "/*.elc")))

(defun archive-user-emacs-directory ()
  "Make a tarball of `user-emacs-directory'."
  (interactive)
  (save-excursion
    (cd user-emacs-directory)
    (shell-command "tar czf ../dotEmacs.tar.gz .")))

 ;; Various only emacs-dependent (elisp libs by GNU Emacs)
;; cool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Show/Hide the other window (window-list)
(defun show/hide-other-window ()
  "Show/Hide the other window" (interactive)
  (if (< 1 (length (window-list)))
      (delete-other-windows)
    (display-buffer (get-next-valid-buffer (buffer-list) (current-buffer)) t)))

(defun show/switch-other-window ()
  "Show/Switch buffer in other window."
  (interactive)
  (if (< 1 (length (window-list))) ; more windows
      (let ((bufs (buffer-list)))
	(while
	    (and
	     ;; (cadr bufs)
	     (cdr bufs)
	     (not
	      (string-match (buffer-name
			     (window-buffer
			      (cadr (window-list))))
			    (buffer-name
			     (car bufs))))
	     (setq bufs (cdr bufs))))
	(display-buffer
	 (get-next-valid-buffer (buffer-list) (car bufs)) t))
    ; show second windows
    (display-buffer
     (get-next-valid-buffer (buffer-list) (current-buffer)) t)))

(defun show/bury-other-window ()
  "Show/Bury buffer in other window. To cycle all buffers"
  (interactive)
  (let ((bufs (buffer-list)))
    (if (< 1 (length (window-list))) ; more windows
	(while
	    (and
	     ;; (cadr bufs)
	     (cdr bufs)
	     (not
	      (string-match (buffer-name
			     (window-buffer
			      (cadr (window-list))))
			    (buffer-name
			     (car bufs))))
	     (setq bufs (cdr bufs))))
	(display-buffer
	 (get-next-valid-buffer (buffer-list) (car bufs)) t)
	(bury-buffer (car bufs)))
    ; show second windows
    (display-buffer
     (get-next-valid-buffer (buffer-list) (current-buffer)) t) (bury-buffer (car bufs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Show/Hide messages in other window
(defun messages-is-visible-p ()""(get-buffer-window-list "*Messages*"))
(defun show/hide-messages ()"Show/Hide messages in other window"(interactive)
  (if (messages-is-visible-p)
      (progn
	(select-window (car (messages-is-visible-p)))
	(bury-buffer)(delete-window))
    (progn
      (switch-to-buffer-other-window "*Messages*" t)
      (goto-char (point-max)) (other-window 1))))

(defcustom minimal-height-for-window 4 "...")
(defcustom minimal-width-for-window  10 "...")
(defun minimize-window () ""
  (cond ((eq (window-width) (frame-width))
	 (enlarge-window (- minimal-height-for-window (window-height))))
	((eq (1+ (window-height)) (frame-height))
	 (enlarge-window (- minimal-width-for-window (window-width)) t))
	(t
	 (progn
	   (unless (and (cdr (window-list))
			(eq (window-height (next-window))
			    (window-height)))
	     (enlarge-window (- minimal-width-for-window (window-width)) t))
	   (unless (and (cadr (window-list))
			(eq (window-width ())
			    (window-width)))
	     (enlarge-window (- minimal-height-for-window (window-height))))))
	))

(defun maximize-window ()
  "" (interactive)
  (enlarge-window (- (frame-height) (1+ minimal-height-for-window) (window-height)))
  (enlarge-window (- (frame-width) (1+ minimal-width-for-window) (window-width)) t))
  
(defun other-window-minimizing ()
  "Switch to other window minimizing the current."
  (interactive)(minimize-window)(other-window 1))
(defun other-window-maximizing ()
  "Switch to other window maximizing it."
  (interactive)(other-window 1)(maximize-window))

;;;; Cycle frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cycleframe-forward () "" (interactive)
  (other-frame 1))
(defun cycleframe-backward () "" (interactive)
  (other-frame -1))

;;;; Utils to write
(defun title-page-at (title &optional pos)
  (interactive "sTitle: \nd") (unless pos (setq pos (point)))
  (goto-char pos)
  (insert (concat " " title)))

(defun title-page (title)(interactive)
  (beginning-of-buffer)(title-page-at title))

;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xsetroot ...
(defun xsetroot (img-file) "" (interactive "fSelect the Image: ")
  (shell-command (concat "xli -onroot -fullscreen "
			 (shell-quote-argument (expand-file-name img-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chomp `perl-like' function
(defun chomp (s)
  "chomp `perl-like' function. Take a string or symbol
and return a string without final or initial whitespace class chars."
  (and (symbolp s) (setq s (symbol-name str)))
  (when (stringp s)
    (while (and (string-match-p "^\\s-+\\|\\s-+$" s)
		(> (length s) (string-match "^\\s-+\\|\\s-+$" s))
      (setq s (replace-match "" t nil s))))
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun basename (name) "?"
  (file-name-nondirectory (directory-file-name name)))

(defun dirname (name) "?"
  (directory-file-name
   (file-name-directory
    (directory-file-name name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun message-list-of-strings (strings) "To Log sequence."
  (if (not (sequencep strings))
      (error "The arg is not a sequence, it is a %s" (type-of strings))
    (let ((l strings) ret elt)
      (while (car l)
	(cond
	 ( (numberp (car l)) (setq elt (number-to-string (car l))) )
	 ( (symbolp (car l)) (setq elt (symbol-name (car l))) )
	 ( (stringp (car l)) (setq elt (car l)) ))
	(setq l (cdr l))
	(if (not ret) (setq ret elt)
	  (setq ret (concat ret "\n" elt))))
      (message "%s" ret))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun save-and-compile ()
  "Save current buffer and make."
  (interactive "")
  (save-some-buffers 0)
  (compile "make -k"))

;===============================================================
; tab completion
(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    (indent-for-tab-command)
    ))

;================================================================
; buffer cycling
(defun backward-buffer () (interactive)
  "Switch to previously selected buffer."
  (let* ((list (cdr (buffer-list)))
	 (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
	(setq list (cdr list))
	(setq buffer (car list))))
    (bury-buffer)
    (switch-to-buffer buffer)))

(defun forward-buffer () (interactive)
  "Opposite of backward-buffer."
  (let* ((list (reverse (buffer-list)))
	 (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
	(setq list (cdr list))
	(setq buffer (car list))))
    (switch-to-buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I hate have to do 'C-x k RET C-x 0' ... 'C-c x k' is most short
;; (defun kill-window () "" (interactive)
;;   (progn (kill-buffer (current-buffer)) (delete-window)))
;; ... exist kill-buffer-and-window
(defun bury-window () "" (interactive)
  (progn (bury-buffer (current-buffer)) (delete-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun googling () "" (interactive)
  (browse-url "http://google.com/ig"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep 'ffap)
  (defun view-file-at-point () "View File At Point." (interactive)
    (view-file (ffap-prompter (ffap-file-at-point)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Macro for a starter of service or job.
;; (defmacro define-async-job-or-service-w/o-args-macro (name)
;;   "Define a command to start `name' program in a async process."
;;   `(defun ,name () (format "Start %s ." (symbol-name (quote ,name)))(interactive)
;; ;;      (unless (not (get-process (symbol-name (quote ,name))))
;; ;;        (error (format "%s yet started\n" (symbol-name (quote ,name)))))
;;      ;;
;;      (let ((pname (symbol-name (quote ,name)))
;; 	   (cmd (symbol-name (quote ,name)))
;; 	   (i 0) outbuf)
;;        (while (get-process pname)
;; 	 (setq i (1+ i)) (setq pname (format "%s<%d>" cmd i)))
;;        (setq outbuf (get-buffer-create (format "*%s*" pname)))
;;        (set-process-sentinel
;; 	(start-process pname outbuf cmd)
;; 	(lambda (p e)(princ (format "The proc: %s said ... %s\n" p e)))))))

;; Function for a starter of service or job.
(defun define-async-job-or-service-w/o-args (name)
  "Define a command to start NAME program in a async process.
NAME could be a symbol or a string for command to run."
  (unless (or (symbolp name)(stringp name))
    (error "I expect a symbol or a string, not a %s." (type-of name)))
  (and (symbolp name)(setq name (symbol-name name)))
  (eval
   (list 'defun (intern name) nil
    (format "Start %s ." name) ;; doc string
    (list 'interactive)
    (list 'let 
     (list
      (list 'pname name)
      (list 'cmd name)
      (list 'i 0) 'outbuf)
     (list 'while (list 'get-process 'pname)
      (list 'setq 'i (list '1+ 'i))
      (list 'setq 'pname (list 'format "%s<%d>" 'cmd 'i)))
     (list 'setq 'outbuf
      (list 'get-buffer-create
       (list 'format "*%s*" 'pname)))
     (list 'set-process-sentinel
      (list 'start-process 'pname 'outbuf 'cmd)
      '(lambda (p e)
	 (princ (format "The proc: %s said ... %s\n" p e))))))))
(put 'define-async-job-or-service-w/o-args 'lisp-indent-offset 1)  
;; (define-async-job-or-service-w/o-args 'xterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cycle scratch and some buffers in other window
(defcustom compilation/term/shell-buffer-name-list '()
  ""
  :group 'my)
(defconst default-compilation/term/shell-buffer-name-list
  '("*compilation*" "*terminal*" "*eshell*"))

;; (defun show/cycle-compilation/term/shell ()
;;   ""
;;   (interactive)
;;   ;; initialize at first
;;   (unless compilation/term/shell-buffer-name-list
;;     (setq compilation/term/shell-buffer-name-list
;; 	  default-compilation/term/shell-buffer-name-list))
;;   (let ((obufs compilation/term/shell-buffer-name-list)
;; 	(found nil) avails bufs)
;;     (dolist (B obufs)
;;       (and (get-buffer B)
;; 	   (setq avails (cons B avails))))
;;     (setq avails (nreverse avails))
;;     (when avails
;;       (setq bufs avails)
;;       (while (and (not found) bufs)
;; 	(if (get-buffer-window-list (car bufs)) (setq found (car bufs)))
;; 	(setq bufs (cdr bufs)))
;;       (if (not found) ;; set at first
;; 	  (when (get-buffer (car avails))
;; 	    (display-buffer (car avails)))
;; 	;; else find 'found' in my-buffers and cycle
;; 	(when (get-buffer found)
;; 	  (bury
;; 	    (get-buffer (or (car (cdr (member found avails))) ""))
	  
;; 	  (display-buffer (car (cdr (member found avails)))))))))
(defun show/cycle-compilation/term/shell ()
  ""
  (interactive)
  ;; initialize at first
  (unless compilation/term/shell-buffer-name-list
    (setq compilation/term/shell-buffer-name-list
	  default-compilation/term/shell-buffer-name-list))
  (let ((bufs compilation/term/shell-buffer-name-list)
	(done nil))
    (while (and (not done) bufs)
      (and (get-buffer (car bufs))
	   (not (get-buffer-window-list (car bufs)))
	   (if (member (buffer-name (current-buffer))
		       compilation/term/shell-buffer-name-list)
	       (or (switch-to-buffer (car bufs)) t)
	     (or (display-buffer (car bufs) t) t))
	   (setq done t))
      (setq bufs (cdr bufs)))
    (and done
	 (not (member (buffer-name (current-buffer))
		      compilation/term/shell-buffer-name-list))
	 (other-window 1))))

;; (global-set-key [S-f4] 'show/cycle-compilation/term/shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-elib)
