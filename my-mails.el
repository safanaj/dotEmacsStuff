
(unless (require 'mail-and-gnus nil t)
  ;;; Mail setting
  (setq user-mail-address "bardelli.marco@gmail.com")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq smtpmail-default-smtp-server "smtp.gmail.com")
  (require 'smtpmail)
  (require 'message)
  (require 'gnus)
  (require 'supercite)
  (setq message-cite-function 'sc-cite-original)

  ;; Gnus settings
  (setq
   mail-sources nil
   mail-source-directory nil
   mail-source-delete-incoming t
   mail-sources '((group)
		  (directory :path "~/Mail/.incoming" :suffix ""))
   gnus-select-method '(nnnil "")
   gnus-secondary-select-methods
   '(
     (nntp "news.gmane.org")
     (nnslashdot "")
     (nndoc "")
     (nnimap "Gmail"
	     (nnimap-address "imap.gmail.com")
	     (nnimap-stream ssl)
	     (nnimap-authinfo-file "~/.netrc.secret")
	     (nnimap-server-port 993)	 )
     )

   gnus-summary-line-format "%U%R%z%d %I%( %s %)\n"
   gnus-save-killed-list nil
   gnus-asynchronous t)

  (custom-set-variables
   '(nnimap-nov-is-evil nil))

  ;; gnus-group-mode-hook
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  ;; end gnus settings
  ;; SMTP settings
  (setq
   smtpmail-debug-info t
   smtpmail-debug-verb t
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-default-smtp-server "smtp.gmail.com"
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   smtpmail-smtp-service 587
   smtpmail-auth-credentials "~/.netrc.secret"
   )

  ;; BBDB stuff
  (when (require 'bbdb nil t)
    ;(setq bbdb-file expand-file-name "bbdb.file" user-emacs-directory)
    (bbdb-initialize 'gnus 'message)
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
    (add-hook 'mail-mode-hook 'message-mode)
    (add-hook 'mail-mode-hook 'bbdb-insinuate-message)
    )
  ;;; end settings
  (provide 'mail-and-gnus)
  )

(provide 'my-mail)
