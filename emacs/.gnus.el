;; Personal information
(setq user-full-name "Jacob Jonsson")

;; Send email through SMTP
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; auto-complete emacs address using bbdb command, optional
(add-hook 'message-mode-hook
	  '(lambda ()
	     (flyspell-mode t)
	     (local-set-key (kbd "TAB") 'bbdb-complete-name)))

;; Please note mail folder in `gnus-select-method` have NO prefix like
;; "nnimap+hotmail:" or "nnimap+gmail:"
(setq gnus-select-method '(nntp "news.gwene.org")) ;; Read feeds/atom through gwene

;; ask encryption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; @see http://gnus.org/manual/gnus_397.html
(add-to-list 'gnus-secondary-select-methods '(nnmaildir "personal" (directory "~/.mail/personal")))
(add-to-list 'gnus-secondary-select-methods '(nnmaildir "work" (directory "~/.mail/work")))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

;; NO 'passive
(setq gnus-use-cache t)

;; press "o" to view all groups
(defun local/gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without unread messages."
  (interactive)
  (gnus-group-list-all-groups))

;; list all the subscribed groups even if they contain zero unread messages
(define-key gnus-group-mode-map (kbd "o") 'local/gnus-group-list-subscribed-groups)

;; Fetch only part of the article if we can.
(setq gnus-read-active-file 'some)

;; open attachment
(eval-after-load 'mailcap '(mailcap-parse-mailcaps))

;; Tree view for groups
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Mail threads
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; Only show top level message
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;; Read HTML mail:
(setq gnus-use-correct-string-widths nil)

;; Organize mail folders
(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-message-archive-group '((format-time-string "sent.%Y")))))

(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
     (setq gnus-topic-topology '(("Gnus" visible)
                                 (("misc" visible))
                                 (("Work" visible nil nil))
				 (("News" visible nil nil)
				  (("Golang" visible nil nil))
				  (("Emacs" visible nil nil))
				  (("Hobbies" visible nil nil)))
                                 (("Personal" visible nil nil))))

     ;; key of topic is specified in my sample ".gnus.el"
     (setq gnus-topic-alist '(("Work" ; the key of topic
                               "nnmaildir+work:Inbox"
			       "nnmaildir+work:starred"
			       "nnmaildir+work:sent")

			      ("Personal" ; the key of topic
                               "nnmaildir+personal:Inbox"
                               "nnmaildir+personal:starred"
                               "nnmaildir+personal:sent")

			      ("Golang" ; the key of the topic
			       "gmane.comp.lang.go.general"
			       "gmane.comp.lang.go.devel"
			       "gmane.comp.lang.go.announce")

			      ("Emacs" ; the key of the topic
			       "gmane.emacs.help"
			       "gmane.emacs.orgmode"
			       "gmane.emacs.devel"
			       "gmane.emacs.conference")

			      ("Hobbies" ; the key of the topic
			       "gmane.comp.lang.haskell.cafe"
			       "gmane.linux.distributions.nixos"
			       "gmane.linux.distributions.nixos.scm")

			      ("misc" ; the key of topic
                               "nndraft:drafts"
                               "nnfolder+archive:sent.2023"			       )
                              ("Gnus")))
     ;; see latest 200 articles in "Golang" when press Enter
     (gnus-topic-set-parameters "News" '((display . 200)))))
