
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

;; I use the server so that I can open applications with emacsclient without
;; running the entire init file again. In automator, I exported
;; for f in "$@"
;;  do
;;      /usr/local/bin/emacsclient -a "" -n "$f"
;;  done
;; as EmacsClient.app, and set that as the default application to open things

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

;; Found on http://jblevins.org/log/dired-open
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

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

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) 
(define-key helm-map (kbd "C-z")  'helm-select-action) 

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h") 'helm-command-prefix)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(use-package helm-swoop
  :bind
  ("C-s" . helm-swoop)
)
(setq helm-swoop-pre-input-function (lambda () ""))

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position))
)



(defhydra nomodifier-movement (
    :hint nil
    :pre (set-cursor-color "#990000")
    :post (progn (set-cursor-color "#000000")
                 (evil-mode 0))
  )
  "Movement Hydra"
  ("a" beginning-of-line)
  ("A" backward-sentence)
  ("b" backward-char)
  ("B" backward-word)
  ("d" evil-delete)
  ("e" end-of-line)
  ("E" forward-sentence)
  ("f" evil-find-char)
  ("F" evil-find-char-backward)
  ("j" evil-forward-paragraph)
  ("k" evil-backward-paragraph)
  ("m" evil-jump-item)
  ("n" next-line)
  ("N" (next-line 5))
  ("p" previous-line)
  ("P" (previous-line 5))
  ("r" evil-replace)
  ("s" evil-search-forward)
  ("u" undo-tree-undo)
  ("U" undo-tree-redo)
  ("v" set-mark-command)
  ("V" select-current-line)
  ("x" delete-char)
  ("X" delete-backward-char)
  ("y" yank) 
  
  ("il" (progn (newline)
    (insert-string "<s")
    (org-try-structure-completion)
    (insert-string "emacs-lisp :tangle yes")
    (next-line)
  ))
  ("ip" (progn (newline)
    (insert-string "<s")
    (org-try-structure-completion)
    (insert-string "python :results output :session *Python* :tangle yes")
    (next-line)
  ))

  ("t" org-todo)
  ("." org-time-stamp)
  ("<left>" org-metaleft)
  ("<right>" org-metaright)
  ("<up>" org-metaup)
  ("<down>" org-metadown)
  
  ("o" window-movement/body "Window movement" :exit t)
  ("SPC" spacehydra-movement/body "Spacehydra" :exit t)
  ("q" nil "Quit" :exit t)
)

(defhydra window-movement (
    :hint nil
    :columns 6
    :pre
    (set-cursor-color "#009900")
    :post
    (set-cursor-color "#000000")
  )
  "Window Movement"
  ("<left>" windmove-left "Window left")
  ("<right>" windmove-right "Window right")
  ("<down>" windmove-down "Window down")
  ("<up>" windmove-up "Window up")
  ("b" helm-mini "Buffer")
  ("B" (progn (other-window 1) (helm-mini)) "Buffer other")
  ("d" delete-window "Delete")
  ("D" delete-other-windows "Delete other")
  ("f" find-file)
  ("F" find-file-other-window)
  ("h" split-window-below)
  ("k" kill-buffer "Kill buffer")
  ("o" other-window)
  ("v" split-window-right)

  ("SPC" spacehydra-movement/body "Spacehydra" :exit t)
  ("n" nomodifier-movement/body :exit t)
  ("p" nomodifier-movement/body :exit t)
  ("q" nil "Quit" :exit t)
)


(defhydra spacehydra-movement (
    :hint nil
    :columns 6
    :exit t 
    :pre
    (set-cursor-color "#000099")
    :post
    (set-cursor-color "#000000")
  )
  "Space-Hydra"
  ("a" org-agenda "Agenda")
  ("b" helm-mini)
  ("c" org-capture "Capture")
  ("d" dired) 
  ("fa" helm-ag "Find with ag")
  ("fr" helm-ag-project-root "Find from root")
  ("ff" helm-find-file "Find file")
  ("hf" describe-function)
  ("hi" info)
  ("hk" describe-key)
  ("hm" describe-mode)
  ("hv" describe-variable)
  ("ls" org-store-link "Store link")
  ("li" org-insert-link "Insert link")
  ("s" save-buffer "Save")
  ("t" (find-file (TODO-file-today)) "Today's todo")
  ("m" magit-status "Magit status")
  ("x" helm-M-x)

  ("n" nomodifier-movement/body :exit t)
  ("p" nomodifier-movement/body :exit t)
  ("o" window-movement/body "Window movement" :exit t)

  ("q" nil "Quit" :exit t)
)

(key-chord-mode 1)
(key-chord-define-global "np" 'nomodifier-movement/body)

(global-unset-key (kbd "C-x o")) 
(global-set-key (kbd "C-x o") 'window-movement/body)

(global-set-key (kbd "<escape>") 'spacehydra-movement/body)

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

(elpy-enable)
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(setq python-shell-completion-native-enable nil)
(setenv "WORKON_HOME" "~/../")
(pyvenv-mode 1)
(pyvenv-activate "homeenv")
(elpy-use-ipython "ipython")

(defun my-python-noindent-docstring (&optional _previous)
  (if (eq (car (python-indent-context)) :inside-docstring)
      'noindent))

(advice-add 'python-indent-line :before-until #'my-python-noindent-docstring)

(require 'jabber)
(setq 
  jabber-roster-line-format " %c %-25n %u %-8s"
  jabber-chat-buffer-show-avatar nil
  jabber-history-enabled t
  jabber-use-global-history t
  jabber-backlog-number 40
  jabber-backlog-days 30
)

(setq magit-repository-directories '("~/Desktop/Repositories"))

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

(cond 
  (
    (eq system-type 'windows-nt)
    (setq elpy-rpc-python-command "C:\\python27\\python")   
    (setq python-shell-interpreter "C:\\python27\\python")
  )
  (
    (eq system-type 'darwin)
    (load-file "~/.emacs.d/personal.el")

    (setq mu4e-maildir "~/Maildir")
    
    (setq mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
    (setq mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail")
    (setq mu4e-trash-folder  "/Gmail/[Gmail].Trash")
    
    (setq mu4e-sent-messages-behavior 'sent)
    
    (setq mu4e-maildir-shortcuts
      '(("/Gmail/INBOX"     . ?i)
        ("/Outlook/INBOX"   . ?e)
      )
    )
    
    (setq mu4e-get-mail-command "mbsync gmail")
    (setq mu4e-update-interval 180)
    (setq mu4e-split-view 'horizontal)
    (setq mu4e-headers-visible-lines 14)
    
    (setq mu4e-headers-fields
      '((:human-date    .  12)
        (:flags         .   6)
        (:from          .  22)
        (:to            .  22)
        (:subject       .  nil)
      )
    )

    
    (setq message-signature nil)
    (setq message-signature-file "~/.emacs.d/.signature")
    (setq mu4e-compose-signature-auto-include nil)
    (setq mu4e-compose-signature (file-string "~/.emacs.d/.signature"))
    (setq mu4e-compose-dont-reply-to-self t)
    
    (setq starttls-gnutls-program "/usr/local/bin/gnutls-cli")
    
    (require 'smtpmail)
    
    (setq message-kill-buffer-on-exit t)
    

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  
  (require 'gnus-dired)

  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
              (null message-sent-message-via)
            )
            (push (buffer-name buffer) buffers)
          )
        )
      )
      (nreverse buffers)
    )
  )
  
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
  
  (require 'org-mu4e)
  
  (add-to-list 'mu4e-view-actions
    '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'helm-find-files-actions
    '("Attach files for mu4e" .
      helm-mu4e-attach
    ) t
  )
  
  (defun helm-mu4e-attach (_file)
    (gnus-dired-attach (helm-marked-candidates))
  )
  (require 'helm-mu)
  (setq mu4e-hide-index-messages 1)
  
  (mu4e-alert-set-default-style 'notifier)
  (setq alert-notifier-command "/usr/local/bin/terminal-notifier")
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  
  
  (require 'mu4e-contrib) 
  (setq mu4e-html2text-command 'mu4e-shr2text) 
  )

)

(setq org-capture-templates '(
    ("t" "TODO capture"
         entry (file (TODO-file-today))
         "* TODO %?")
    ("l" "TODO capture with link"
         entry (file (TODO-file-today))
         "* TODO %?\n  From: %a")
))

(setq org-return-follows-link t)
(setq org-cycle-emulate-tab nil)
(setq org-directory "~/.emacs.d/org-files")
(setq org-agenda-files (file-expand-wildcards "~/.emacs.d/org-files/*.org"))

(setq org-todo-keywords
  '((sequence "TODO" "|" "DONE" "WAIT")))
(setq org-todo-keyword-faces
  '(("TODO" . org-warning) ("WAIT" . "blue")))

(defun get-TODO-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y-%m-%d")))
    (expand-file-name (concat "~/.emacs.d/org-files/" daily-name ".org"))))

(defun TODO-file-today ()
  "Create and load a journal file based on today's date."
  (if (equal (file-exists-p (get-TODO-file-today)) t)
    (get-TODO-file-today)
    (progn
      (find-file (get-TODO-file-today))
      (insert-string (concat "#+TITLE: TODO List for " (format-time-string "%A, %B %d")))
      (newline)
      (insert-string "#+DATE: ")
      (calendar)
      (find-file (get-TODO-file-today))
      (org-date-from-calendar)
      (save-buffer t)
      (setq org-agenda-files (file-expand-wildcards "~/.emacs.d/org-files/*.org"))
      (get-TODO-file-today)
      (delete-other-windows)
    )  
  )
)

(setq org-src-fontify-natively t)

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )


