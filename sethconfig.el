
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)             

(add-to-list 'exec-path "/usr/local/bin/")
(add-to-list 'exec-path "/usr/bin/")
(add-to-list 'load-path "/usr/share/emacs/24.3/lisp")

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(require 'use-package)
(server-start)

(load-file "~/.emacs.d/personal.el")

(require 'recentf)
(which-key-mode)
(require 'dired-x)

(setq-default fill-column 60)
(setq column-number-mode t)
(electric-indent-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)   

(setq read-buffer-completion-ignore-case t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq recentf-max-saved-items 150)

(setq default-directory "~/")
(setq dired-omit-files 
  (concat dired-omit-files
    "\\|^\\..+$\\|^.
    +?\\.aux$\\|^.
    +?\\.log$\\|^.
    +?sync\\|^.
    +?out\\|^.
    +?run.xml")
)
                         
(add-hook 'dired-mode-hook
  (lambda ()
    (dired-omit-mode 1)
))

(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o") 'dired-omit-mode)

(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l") 'dired-up-directory)

(require 'helm)
(require 'helm-config)
(helm-mode 1)

(setq helm-split-window-in-side-p       t 
  helm-move-to-line-cycle-in-source     t 
  helm-ff-search-library-in-sexp        t 
  helm-scroll-amount                    8 
  helm-ff-file-name-history-use-recentf t
)
(helm-autoresize-mode t)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) 
(define-key helm-map (kbd "C-z")  'helm-select-action) 

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defhydra nomodifier-movement ()
  "Emacs Movement"
  ("f" forward-char)
  ("F" forward-word)
  ("b" backward-char)
  ("B" backward-word)
  ("n" next-line)
  ("p" previous-line)
  ("e" end-of-line)
  ("E" forward-sentence)
  ("a" beginning-of-line)
  ("A" backward-sentence)
  ("u" undo-tree-visualize)
  ("dl" delete-char)
  ("dw" kill-word)
  ("dd" kill-whole-line)
  ("v" set-mark-command)
  ("V" select-current-line)
  (">" end-of-buffer) 
  ("<" beginning-of-buffer)
  ("q" nil)
)

(defhydra window-movement ()
  "Window Movement"
  ("<left>" windmove-left)
  ("<right>" windmove-right)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("o" other-window)
  ("v" split-window-right)
  ("d" delete-window)
  ("D" delete-other-windows)
  ("f" find-file "file")
  ("F" find-file-other-window "other file")
  ("b" helm-buffers-list "buffers list")
  ("B" (progn (other-window 1) (helm-buffers-list)))
  ("q" nil)
)

(defhydra spacehydra ()
  "Space-Hydra"
  ("a" helm-ag "helm-ag")
  ("b" helm-mini)
  ("d" dired) 
  ("f" helm-find-files)
  ("hf" describe-function)
  ("hi" info)
  ("hk" describe-key)
  ("hm" describe-mode)
  ("hv" describe-variable)
  ("s" save-buffer "save")
  ("m" magit-status "magit status")
  ("x" helm-M-x)
  ("q" nil)
)


(key-chord-mode 1)
(key-chord-define-global "np" 'nomodifier-movement/body)

(global-unset-key (kbd "C-o")) 
(global-set-key (kbd "C-o") 'window-movement/body)

(key-chord-define-global "  " 'spacehydra/body)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook
  (lambda () (local-set-key (kbd "<M-S-mouse-1>") #'TeX-view))
)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(getenv "PATH")
(setenv "PATH" (concat "/usr/texbin" ":"
    (getenv "PATH")
  )
)
(getenv "PATH")

(setenv "PATH" (concat "/usr/local/bin" ":"
    (getenv "PATH")
  )
)
(setenv "PATH" (concat "/usr/bin" ":"
    (getenv "PATH")
  )
)

(setq TeX-source-correlate-method 'synctex)
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))

(setq TeX-view-program-list '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))
(add-hook 'LaTeX-mode-hook 
  (lambda()
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
    (setq TeX-save-query nil)
    (setq TeX-show-compilation nil)
  )
)

(add-hook 'LaTeX-mode-hook #'outline-minor-mode)

(autoload 'gap-mode "gap-mode" "Gap editing mode" t)
(setq auto-mode-alist (append (list '("\\.g$" . gap-mode)
  '("\\.gap$" . gap-mode))
  auto-mode-alist))
(autoload 'gap "gap-process" "Run GAP in emacs buffer" t)
(setq gap-executable "/Users/seth/Downloads/gap4r8/bin/gap-default64.sh")
(setq gap-start-options '("-n" "-f" "-b" "-m" "2g"))

(add-to-list 'load-path "/usr/local/bin/pari")
(autoload 'gp-mode "pari" nil t)
(autoload 'gp-script-mode "pari" nil t)
(autoload 'gp "pari" nil t)
(autoload 'gpman "pari" nil t)
(setq auto-mode-alist (cons '("\\.gp$" . gp-script-mode)
  auto-mode-alist))

(require 'jabber)
(setq 
  jabber-roster-line-format " %c %-25n %u %-8s"
  jabber-chat-buffer-show-avatar nil
  jabber-history-enabled t
  jabber-use-global-history t
  jabber-backlog-number 40
  jabber-backlog-days 30
)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(require 'twittering-mode)
(defun twitter-open-link ()
  (twittering-goto-next-thing t)
  (twittering-enter)
)
(if twittering-mode-map
  (let ((km twittering-mode-map))
    (define-key km (kbd "n") 'twittering-goto-next-status)
    (define-key km (kbd "p") 'twittering-goto-previous-status)
    (define-key km (kbd "N") 'twittering-goto-next-status-of-user)
    (define-key km (kbd "P") 'twittering-goto-previous-status-of-user)
    (define-key km (kbd "o") 'twitter-open-link)
    nil
  )
)

(defun file-string (file)
    "Read the contents of a file and return as a string."
    (with-current-buffer (find-file-noselect file)
      (buffer-string)))

(defun boxify ()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (insert "+------------------------------")
  (next-line)
  (beginning-of-line)
  (insert "|  ")
  (end-of-line)
  (insert "  |")
  (newline)
  (insert "+------------------------------")
  (previous-line 1)
  (previous-line 1)  
  (backward-char)
  (kill-line)
  (insert "+")
  (next-line 2)
  (backward-char)
  (kill-line)
  (insert "+")
)

(defun mu4e-kill-update-process ()
  (interactive)
  (kill-process " *mu4e-update*")
)

(setq mu4e-maildir "~/Maildir")

(setq mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
(setq mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/Gmail/[Gmail].Trash")

(setq mu4e-sent-messages-behavior 'sent)

(setq mu4e-maildir-shortcuts
    '( ("/Gmail/INBOX"     . ?i)
       ("/Outlook/INBOX"   . ?e)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "/usr/local/bin/offlineimap")
(setq mu4e-update-interval 180)
(setq mu4e-split-view 'horizontal)
(setq mu4e-headers-visible-lines 14)

(setq mu4e-headers-fields
    '( (:human-date    .  12)
       (:flags         .   6)
       (:from          .  22)
       (:to            .  22)
       (:subject       .  nil)))

(add-to-list 'mu4e-bookmarks
'("\"maildir:/Gmail/[Gmail].Sent Mail\" date:8w..now OR \"maildir:/Outlook/Sent\" date:8w..now" "All sent" ?s))
(add-to-list 'mu4e-bookmarks
           '("\"maildir:/Gmail/INBOX\" date:4w..now OR \"maildir:/Outlook/INBOX\" date:4w..now" "All mail" ?a))


 
;; something about ourselves

(setq message-signature nil)
(setq message-signature-file "~/.emacs.d/.signature")
(setq mu4e-compose-signature-auto-include nil)
(setq mu4e-compose-signature (file-string "~/.emacs.d/.signature"))
(setq mu4e-compose-dont-reply-to-self t)

(setq starttls-gnutls-program "/usr/local/bin/gnutls-cli")

(require 'smtpmail)

(setq message-kill-buffer-on-exit t)

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

(require 'gnus-dired)

(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (and (derived-mode-p 'message-mode)
        (null message-sent-message-via))
        (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(require 'org-mu4e)

(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)
(add-to-list 'helm-find-files-actions
  '("Attach files for mu4e" .
    helm-mu4e-attach) t
)

(defun helm-mu4e-attach (_file)
  (gnus-dired-attach (helm-marked-candidates)))
(require 'helm-mu)
(setq mu4e-hide-index-messages 1)

(mu4e-alert-set-default-style 'notifier)
(setq alert-notifier-command "/usr/local/bin/terminal-notifier")
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)


(require 'mu4e-contrib) 
(setq mu4e-html2text-command 'mu4e-shr2text)

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package helm-swoop
  :bind
  ("C-s" . helm-swoop)
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
)

(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )
