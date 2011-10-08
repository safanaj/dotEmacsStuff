;;;;;;;;;;;;;;;;;;; -*- folded-file: t; -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;{{{ Loading-utils Functions for elisp  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (bound-and-true-p user-elisp-directory)
  (setq user-elisp-directory
	(expand-file-name "elisp" user-emacs-directory)))

(defun check-provided (&optional feature in-file in-dir)
  "Check for provide calls. grep FEATURE IN-FILE IN-DIR.
If IN-DIR is nil or not exist, grep in `user-elisp-directory'."
  (let ((_feature (if (not feature) ".*"
		    feature))
	(_infile (if (not in-file) "*.el"
		   (file-name-nondirectory in-file)))
	(_indir (if (not in-dir) user-elisp-directory
		  (expand-file-name in-dir)))
	(retval '()))
    (unless (car (file-attributes _indir))
      (setq _indir user-elisp-directory))
    (when (file-directory-p user-elisp-directory)
      (save-excursion
	(cd _indir)
	(dolist (LINE
		 (split-string
		  (shell-command-to-string
		   (concat "grep -nH -e"
			   " \"^(provide '" _feature ")$\" " _infile))
		  "\n" t))
	  (setq LINE (split-string LINE ":" t))
	  (add-to-list 'retval (cons (substring (nth 2 LINE) 10 -1)
				     (basename (nth 0 LINE)))))))
    retval))

 ;; Require/Load and Initialize
(defun require-or-load-elisp-library (afeature &optional wrap initform)
  "Require AFEATURE in elisp user dir by name.
If second optional arg WRAP is true, temporary add `user-elisp-director'
during loading library.
The third INITFORM is a form to initialize just loaded library."
  (unless (symbolp afeature)
    (error "The feature is not symbol, but a %s" (type-of afeature)))
  (let* ((afn (symbol-name afeature))
	 (fn (expand-file-name
	      (concat afn ".el") user-elisp-directory))
	 (fnc (concat fn "c"))
	 provide)
    (unless (and fn (file-exists-p fn))
      (setq fn (cdr (assoc afn (check-provided)))))
    (unless (or (not fn)
		(assoc afn (check-provided nil fn))
		(not (caar (check-provided nil fn))))
      (setq afeature (intern (caar (check-provided nil fn)))))
    (when (and fn (file-exists-p fn))
      (unless (file-exists-p fnc) (setq fnc nil))
      ;; if afeature requires other stuff, add user-elisp-directory to load-path
      (and wrap	(add-to-list 'load-path user-elisp-directory))
      ;; require or load feature
      (or (setq provide (require afeature (or fnc fn) t))
	  (load (or fnc fn) t))
      ;; clean load-path if wrapped
      (and wrap (setq load-path (remove user-elisp-directory load-path)))
      (condition-case nil
	  (progn
	    (eval initform))
	(error nil)))
    ;; provide stuff
    (or (and (featurep afeature) afeature)
	(and (featurep provide) provide))))

 ;; Auto-Install mantain elisp/ dir
;; was
;(require-or-load-elisp-library install-elisp)
;(require-or-load-elisp-library require-or-install)
;; but now is better
(require 'auto-install
	 (expand-file-name "auto-install.el" user-elisp-directory) t)
(condition-case nil
    (auto-install-update-emacswiki-package-name nil)
  (error nil))

(setq auto-install-directory
      (file-name-as-directory user-elisp-directory))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; my-require-pkg ... To use Project.ede
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;(require-all-for 'start) ;; by Drew Adams
;; ;; ;; Prevent dired+ font-lock-decoration ...
;; ;; (setq font-lock-maximum-decoration nil)

;; ;;;; pkg: doremi-pkg
;; (defun my-require-doremi-pkg () "" (interactive)
;;   (progn
;; ;;     (require-all-for 'imenu+)
;;     (require-or-load-elisp-library imenu+)
;; ;;     (require-all-for 'autofit-frame)
;;     (require-or-load-elisp-library autofit-frame)
;; ;;     (require-all-for 'thumb-frm)
;;     (require-or-load-elisp-library thumb-frm)
;; ;;     (require-all-for 'zoom-frm)
;;     (require-or-load-elisp-library zoom-frm)
;; ;;     (require-all-for 'doremi) ;; Dynamic adjustment of frame properties.
;;     (require-or-load-elisp-library doremi)
;; ;;     ;; Loads `doremi.el'.
;; ;;     (require-all-for 'doremi-cmd) ;; Other Do Re Mi commands.
;; ;;    (require-or-load-elisp-library doremi-cmd)
;; ;;     (require-all-for 'doremi-frm) ;; Dynamic adjustment of frame properties.
;; ;;     (require-or-load-elisp-library doremi-frm)
;; ;; ;;     (require-all-for 'doremi-mac) ;; Do Re Mi Macro.
;; ;;     (require-or-load-elisp-library doremi-mac)
;;     'my-doremi-pkg))

;;;; pkg: base
(require-or-load-elisp-library 'strings t)
(require-or-load-elisp-library 'misc-fns t)
(require-or-load-elisp-library 'grep-o-matic)
(require-or-load-elisp-library 'grep-buffers t)
(require-or-load-elisp-library 'dired-details t
 '(progn
    (dired-details-install)
    (setq dired-details-hidden-string "[] ")
    (define-key dired-mode-map ")" 'dired-details-toggle)))


(require-or-load-elisp-library 'dired-details+ t)
(require-or-load-elisp-library 'ebs t)
(require-or-load-elisp-library 'perl-find-library t)
(require-or-load-elisp-library 'modeline-posn)
(require-or-load-elisp-library 'compile- t)
(require-or-load-elisp-library 'compile+ t)
(require-or-load-elisp-library 'compile-dwim t)
(require-or-load-elisp-library 'grep+ t)
(require-or-load-elisp-library 'menu-bar+ t)
(require-or-load-elisp-library 'bookmark+ t)
;(require-or-load-elisp-library dired-x)
(require-or-load-elisp-library 'ffap- t)
;(my-require-dired-details-pkg)
(require-or-load-elisp-library 'babel t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; for manage pages from emacswiki.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Customize elisp code from EmacsWiki
;; to edit the emacswiki.org
(require-or-load-elisp-library 'oddmuse-curl t
 '(progn
    (oddmuse-mode-initialize)
    (setq oddmuse-directory "~/.emacs.d/oddmuse"
	  oddmuse-use-always-minor t
	  oddmuse-username "SaFanaj")
    (add-hook
     'oddmuse-mode-hook
     (lambda ()
       (unless (string-match "question" oddmuse-post)
	 (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Stuff
;;;;;;;;;;;;;;;;
;;;; for Google Account
(require-or-load-elisp-library 'googleaccount t)
;;;; for Google search
(require-or-load-elisp-library 'google-define t)
;;;; for GObject coding
(require-or-load-elisp-library 'gobject-class t)
;;;; for elisp code from emacswiki
(require-or-load-elisp-library 'wikiarea-fixed t)
;;;; for Texi
;(require-or-load-elisp-library 'eval-to-texi t)
;;;; for Color Browser
(if window-system (require-or-load-elisp-library 'color-browser))
;;;; for Color Theme Maker
(if window-system (require-or-load-elisp-library 'color-theme-maker))
;;;; for control registers
(require-or-load-elisp-library 'register-list t)
;;;; for finding pkgs (finder C-h p)
(require-or-load-elisp-library 'finder+ t)

(require-or-load-elisp-library 'doc-view-extension t)
;;;; for Emacs Image Manipulation Program
(require-or-load-elisp-library 'eimp t)
;;;; for Easy HyperText Navigation. (it's fantastic ;)
(require-or-load-elisp-library 'linkd t)
;(require-or-load-elisp-library-fn 'icicles t)
;(require-or-load-elisp-library icicles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requires some libs in my .emacs.d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For Proced ...
(require 'proced nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; For Common Lisp ...
(and (require 'slime nil t)
     (setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
     (slime-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; For Dir-Nav dir navigation horizontally
(require 'dirnav (expand-file-name (concat user-emacs-directory "dir-nav/dirnav.el")) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For Git ...
(require 'vc-git nil t)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git nil t)
(autoload 'git-blame-mode "git-blame" "blah blah" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; for HTTP stuff
(require 'http-cookies nil t)
(require 'http-get nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'my-elisp)



;;;;;;;;;;;;;;;;;;;;;; OLD SUFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;; Macro to require libs in elisp/ easely
;; (defmacro require-or-load-elisp-library-macro (feature)
;;   "Require a feature in elisp user dir by name."
;;   `(let ((ext
;; 	  (if (file-exists-p
;; 	       (expand-file-name
;; 		(concat (symbol-name (quote ,feature)) ".elc") user-elisp-directory)) 
;; 	      ".elc" ".el")))
;;      (condition-case nil
;; 	 (if (assq (quote ,feature)
;; 		   (check-provided
;; 		    (symbol-name (quote ,feature))
;; 		    (concat (symbol-name (quote ,feature)) ".el")))
;; 	     (require (quote ,feature)
;; 		      (expand-file-name
;; 		       (concat user-elisp-directory (symbol-name (quote ,feature)) ext)) t)
;; 	   (load
;; 	    (expand-file-name
;; 	     (concat user-elisp-directory (symbol-name (quote ,feature)) ext)) t))
;;        (error nil))
;;      (featurep (quote ,feature))))

;; (defmacro require-or-load-elisp-library-macro-wrapped (feature)
;;   "Require a feature in elisp user dir by name."
;;   `(let ((ext
;; 	  (if (file-exists-p
;; 	       (expand-file-name
;; 		(concat user-elisp-directory
;; 			(symbol-name (quote ,feature)) ".elc")))
;; 	      ".elc" ".el")))
;;      (add-to-list 'load-path user-elisp-directory)
;;      (if (assq (quote ,feature)
;; 	       (check-provided
;; 		(symbol-name (quote ,feature))
;; 		(concat (symbol-name (quote ,feature)) ".el")))
;; 	 (require (quote ,feature)
;; 		  (expand-file-name
;; 		   (concat user-elisp-directory (symbol-name (quote ,feature)) ext)) t)
;;        (load
;; 	(expand-file-name
;; 	 (concat user-elisp-directory (symbol-name (quote ,feature)) ext)) t))
;;      (setq load-path (remove user-elisp-directory load-path))
;;      (featurep (quote ,feature))))
  

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; (defun use-dotElisp (&optional feature) "" (interactive)
;; ;;   (let ((_file (if (not feature)
;; ;; 		      (read-file-name "what to load?" user-elisp-directory)
;; ;; 		 (expand-file-name
;; ;; 		  (concat user-elisp-directory feature ".el")))))    
;; ;;     (if (file-exists-p _file)
;; ;; 	(load-file _file)
;; ;;       (message "What ???"))))

;; ;; (defvar lines-begin-regexp
;; ;;   "^;; *Features .*required by this library:$"
;; ;;   "regexp for meaning lines")

;; ;; (defvar lines-end-regexp
;; ;;   "^;;;;+$"
;; ;;   "regexp for meaning lines")

;; ;; (defvar features-regexp "`\\([-a-z+0-9]*\\)'[,.]" "")

;; ;; (defun requiredBy-ed (lib &optional grep-features-function)
;; ;;   "return a list"
;; ;;   (save-excursion
;; ;;     (unless (file-exists-p lib) (return nil))
;; ;;     (let ((buf (find-file-noselect lib))
;; ;; 	  (rbuf (generate-new-buffer "reqBy"))
;; ;; 	  pos (ret '()))
;; ;;       (progn
;; ;; 	(set-buffer buf) (beginning-of-buffer)
;; ;; 	(if (functionp grep-features-function)
;; ;; 	    (return grep-features-function (buffer-string)))
;; ;; 	(setq pos (string-match lines-begin-regexp (buffer-string)))
;; ;; 	(if (not pos) (return))
;; ;; 	(goto-char pos)(set-mark (point))
;; ;; 	(setq pos (string-match lines-end-regexp (buffer-string)))
;; ;; 	(if (not pos) (return))
;; ;; 	(goto-char pos)
;; ;; 	(copy-to-buffer rbuf (mark) (point))
;; ;; 	(kill-buffer buf)
;; ;; 	(set-buffer rbuf)
;; ;; 	(setq pos (string-match features-regexp (buffer-string) 0))
;; ;; ;	(print (match-string 0))
;; ;; 	(while (numberp pos)
;; ;; 	  (add-to-list 'ret (match-string-no-properties 1 (buffer-string)) t)
;; ;; 	  (setq pos (match-end 1))
;; ;; 	  (setq pos (string-match features-regexp (buffer-string) pos)))
;; ;; 	(kill-buffer (current-buffer))
;; ;; 	ret))))

;; ;; (defun requiredBy (sym) "return a list"
;; ;;   (let ((file
;; ;; 	 (expand-file-name
;; ;; 	  (concat user-elisp-directory (symbol-name sym) ".el"))))
;; ;;     (if (not (file-exists-p file))
;; ;; 	(list (symbol-name sym))
;; ;;       (remove (symbol-name sym) (requiredBy-ed file)))))


;; ;; (defun require-all-inlist (list) ""
;; ;;   (dolist (lib list nil)
;; ;;     (let ((file (expand-file-name
;; ;; 		 (concat user-elisp-directory lib ".el")))
;; ;; 	  (sym (intern lib)))
;; ;;       (if (file-exists-p file) (require-all-inlist (requiredBy sym)))
;; ;;       (require sym
;; ;; 	       (if (file-exists-p file) file nil) t))))

;; ;; (defun require-all-for (pkg) ""
;; ;;   (let ((file
;; ;; 	 (expand-file-name
;; ;; 	  (concat user-elisp-directory (symbol-name pkg) ".el"))))
;; ;;     (require-all-inlist (requiredBy pkg))
;; ;;     (require pkg
;; ;; 	     (if (file-exists-p file) file nil) t)))

;; ;;}}}
