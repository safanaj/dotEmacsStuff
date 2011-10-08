
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load a clean cedet
(or nil (bound-and-true-p cedet-version)
    (dolist (LP load-path)
      (and (string-match "cedet$" LP)
	   (setq load-path
		 (remove LP load-path))))
    (load-file (expand-file-name "~/Progetti/cedet/trunk/common/cedet.el")))


;;; Activate all, TODO put in one menu
(global-ede-mode 1)
(global-senator-minor-mode 1)
(global-srecode-minor-mode 1)
(semanticdb-enable-gnu-global-databases 'c-mode)
(add-hook 'c-mode-common-hook 'imenu-add-menubar-index)

