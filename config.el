;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; other configuration examples
;;

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tony Day"
      user-mail-address "tonyday567@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka ss02" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka etoile" :size 20))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-oceanic-next)
(setq doom-theme 'doom-Iosvkem)
;; (doom-themes-org-config)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq evil-split-window-below t
      evil-vsplit-window-right t
      confirm-kill-emacs nil
      shift-select-mode t
      window-combination-resize t
      delete-selection-mode t
      case-fold-search t
      auto-save-default t)

(setq search-whitespace-regexp ".*"
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil)

;; setq-default sets variables that are usually local to buffers
(setq-default truncate-lines nil
              indent-tabs-mode nil)

(map!
 (:map 'override
   :v "v" #'er/expand-region
   :v "V" #'er/contract-region))
(map!
 (:map 'override
   :m "j" #'evil-next-visual-line
   :m "k" #'evil-previous-visual-line))

(setq evil-want-fine-undo t
      evil-kill-on-visual-paste nil
      evil-want-C-u-scroll nil
      evil-want-C-u-delete nil
      evil-want-integration t
      evil-want-keybinding nil
      evil-move-cursor-back nil
      evil-move-beyond-eol t
      evil-highlight-closing-paren-at-point-states nil)

(defun evil-forward-after-end (thing &optional count)
  "Move forward to end of THING.
The motion is repeated COUNT times."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (forward-thing thing count))
   (t
    (unless (bobp) (forward-char -1))
    (let ((bnd (bounds-of-thing-at-point thing))
          rest)
      (when bnd
        (cond
         ((< (point) (cdr bnd)) (goto-char (car bnd)))
         ((= (point) (cdr bnd)) (cl-incf count))))
      (condition-case nil
          (when (zerop
                 (setq rest
                       (forward-thing thing count)))
            (end-of-thing thing))
        (error))
      rest))))

(evil-define-motion evil-forward-after-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word))
        (count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (evil-forward-after-end thing count)))

(evil-define-motion evil-forward-after-WORD-end (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (evil-forward-after-word-end count t))

(evil-define-key 'motion 'global "e"  #'evil-forward-after-word-end)
(evil-define-key 'motion 'global "E"  #'evil-forward-after-WORD-end)

(setq vertico-sort-function #'vertico-sort-history-alpha)
(setq avy-all-windows t)

(map! :leader "s f" #'consult-find)
(map! :leader "s y" #'consult-yank-from-kill-ring)

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(define-key isearch-mode-map (kbd "M-j") 'avy-isearch)

(defun isearch-forward-other-window (prefix)
    "Function to isearch-forward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))))))

(defun isearch-backward-other-window (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix 1 -1)))
        (other-window next)
        (isearch-backward)
        (other-window (- next))))))

(define-key global-map (kbd "C-M-s") 'isearch-forward-other-window)
(define-key global-map (kbd "C-M-r") 'isearch-backward-other-window)

(defun style/left-frame ()
  (interactive)
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    (progn
      (set-frame-parameter (selected-frame) 'fullscreen nil)
      (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'top 10)
      (set-frame-parameter (selected-frame) 'left 6)
      (set-frame-parameter (selected-frame) 'height 40)
      (set-frame-parameter (selected-frame) 'width 120)))
   ((string-equal system-type "darwin") ; Mac OS X
    (progn
      (set-frame-parameter (selected-frame) 'fullscreen nil)
      (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'top 23)
      (set-frame-parameter (selected-frame) 'left 0)
      (set-frame-parameter (selected-frame) 'height 44)
      (set-frame-parameter (selected-frame) 'width 100)
      (message "default-frame set")))
   ((string-equal system-type "gnu/linux") ; linux
    (progn
      (message "Linux")))))

(add-to-list 'initial-frame-alist '(top . 23))
(add-to-list 'initial-frame-alist '(left . 0))
(add-to-list 'initial-frame-alist '(height . 44))
(add-to-list 'initial-frame-alist '(width . 100))

(defun style/max-frame ()
  (interactive)
  (if t
      (progn
        (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
        (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
        (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))
    (set-frame-parameter (selected-frame) 'top 26)
    (set-frame-parameter (selected-frame) 'left 2)
    (set-frame-parameter (selected-frame) 'width
                         (floor (/ (float (x-display-pixel-width)) 9.15)))
    (if (= 1050 (x-display-pixel-height))
        (set-frame-parameter (selected-frame) 'height
                             (if (>= emacs-major-version 24)
                                 66
                               55))
      (set-frame-parameter (selected-frame) 'height
                           (if (>= emacs-major-version 24)
                               75
                             64)))))

(after! doom-dashboard
  (message "post doom-dashboard")
  (style/left-frame)  ;; Focus new window after splitting
)

;; replaces just-one-space
(map! "M-SPC" #'cycle-spacing)
(map! :map global-map "M-j" #'avy-goto-char-timer)

(map! (:after evil-org
       :map evil-org-mode-map
       :inv "M-j" nil))

(use-package! discover-my-major)

(after! org
  (setq
   org-capture-templates
   (quote
    (("r" "refile" entry
      (file "~/org/refile.org")
      "* ToDo %?
")
     ("s" "stack" checkitem
      (file+headline "~/org/stuff.org" "stack")
      "- %?
  %a" :prepend t)
     ("z" "bugz" entry
      (file+headline "~/org/bugz.org" "bugz!")
      "* ToDo %?
%a")))))

(after! org
  :config
  (setq
   org-superstar-headline-bullets-list '("⁖")
   org-startup-folded 'overview
   org-support-shift-select t
   org-insert-heading-respect-content nil
   org-ellipsis " [...] "
   org-startup-with-inline-images t
   org-cycle-include-plain-lists 'integrate
   ;; https://github.com/syl20bnr/spacemacs/issues/13465
   org-src-tab-acts-natively nil)
   ;; flyspell off for org mode
   (remove-hook 'org-mode-hook 'flyspell-mode)
)

(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmd! (if (org-on-heading-p)
                         (org-backward-element)
                       (evil-previous-visual-line)))
       :n "gj" (cmd! (if (org-on-heading-p)
                         (org-forward-element)
                       (evil-next-visual-line)))))

(after! org
  :config
   (setq-default org-todo-keywords '((sequence "ToDo(t)" "Next(n)" "Blocked(b)" "|" "Done(d!)")))
   (setq org-todo-keyword-faces (quote (("ToDo" :foreground "#2E2E8B8B5757" :weight bold)
                                        ("Done" :foreground "black" :weight bold)
                                        ("Blocked" :foreground "yellow4" :weight bold)
                                        ("Next" :foreground "orange red" :weight bold))))
   (setq org-agenda-category-icon-alist
        `(("life" ,(list (all-the-icons-material "home" :height 1)) nil nil :ascent center)
          ("garden" ,(list (all-the-icons-material "home" :height 1)) nil nil :ascent center)
          ("sys" ,(list (all-the-icons-material "settings" :height 1)) nil nil :ascent center)
          ("bugz" ,(list (all-the-icons-material "flag" :height 1)) nil nil :ascent center)
          ("emacs" ,(list (all-the-icons-material "edit" :height 1)) nil nil :ascent center)
          ("repo" ,(list (all-the-icons-material "ac_unit" :height 1)) nil nil :ascent center)
          ("ib" ,(list (all-the-icons-material "account_balance" :height 1)) nil nil :ascent center)
          ("fe" ,(list (all-the-icons-material "local_atm" :height 1)) nil nil :ascent center)
          ("auspol" ,(list (all-the-icons-material "format_align_left" :height 1)) nil nil :ascent center)
          ("drafts" ,(list (all-the-icons-material "format_align_left" :height 1)) nil nil :ascent center)
          ("iqfeed" ,(list (all-the-icons-material "account_balance" :height 1)) nil nil :ascent center)
          ("haskell" ,(list (all-the-icons-material "event_available" :height 1)) nil nil :ascent center)
          ("act" ,(list (all-the-icons-material "event_available" :height 1)) nil nil :ascent center)
          ("refile" ,(list (all-the-icons-material "move_to_inbox" :height 1)) nil nil :ascent center)))
)

(use-package! org-super-links
  :config
  (map! :map org-mode-map
        :localleader
        (:prefix ("m" . "backlinks")
         :nvm "l" #'org-super-links-link
         :nvm "s" #'org-super-links-store-link
         :nvm "i" #'org-super-links-insert-link
         :nvm "d" #'org-super-links-delete-link
         :nvm "c" #'org-super-links-convert-link-to-super)))

(after! org-agenda
  :config
  (setq org-agenda-files
   '("~/org")))

(after! org-agenda
  :config
  (setq org-agenda-span 'week
        org-agenda-use-time-grid nil
        org-agenda-start-day "-0d"
        org-agenda-block-separator nil
        org-agenda-show-future-repeats nil
        org-agenda-compact-blocks t
        org-agenda-show-all-dates nil
        org-agenda-prefix-format
         '((agenda . " %-12t")
           (todo . " %-12:c")
           (tags . " %-12:c")
           (search . " %-12:c")))
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)
  (setq org-habit-graph-column 32)
  (setq org-habit-following-days 2)
  (setq org-habit-preceding-days 20)
  (setq org-log-into-drawer t)
  (map! :leader "oz" #'agenda-z)
  (map! :map org-agenda-mode-map
        :localleader
        (:nvm "l" #'org-agenda-log-mode
         :nvm "h" #'org-agenda-habit-mode)))

(defun agenda-z ()
  (interactive)
  (org-agenda nil "z"))

(defun org-agenda-habit-mode (&optional junk)
  "Toggle showing all habits."
  (interactive "P")
  (setq org-habit-show-all-today (not org-habit-show-all-today))
  (org-agenda-redo)
  (message "All habits are %s" (if org-habit-show-all-today "on" "off")))

(defun make-qsags ()
 (-let* (((m d y) (calendar-gregorian-from-absolute (+ 6 (org-today))))
           (target-date (format "%d-%02d-%02d" y m d))
        )
  (setq org-super-agenda-groups
         `(
           (:name "clocked"
            :log clock)
           (:name "next"
            :todo "Next")
           (:name "refile"
            :category "refile")
           (:name "blocked"
            :todo "Blocked")
           (:name "fun"
            :and (:scheduled nil
                  :not (:log clock)
                  :tag ("fun"))
            :discard (:habit t))
           (:name "lemon"
            :and (:scheduled nil
                  :not (:log clock)
                  :tag ("lemon"))
            :discard (:habit t))
           (:name "site"
            :and (:scheduled nil
                  :not (:log clock)
                  :tag ("site"))
            :discard (:habit t))
           (:name "reading"
            :and (:scheduled nil
                  :not (:log clock)
                  :tag ("reading"))
            :discard (:habit t))
           (:name "repo"
            :and (:scheduled nil
                  :not (:log clock)
                  :tag ("repo"))
            :discard (:habit t))

           (:name "stuff"
            :and (:scheduled nil
                  :not (:log clock)
                  :not (:tag ("ignore")))
            :discard (:habit t))
           (:name "a while"
            :scheduled (after ,target-date)
            :discard (:scheduled t))
           (:name "scheduled"
            :scheduled t
            :discard (:habit t)
            :order 9)
           (:name "errors")
          ))))

(use-package! org-super-agenda
  :config
   (use-package origami
    :bind (:map org-super-agenda-header-map
            ("<tab>" . origami-toggle-node)
            ("j" . evil-next-visual-line)
            ("k" . evil-previous-visual-line))
    :hook ((org-agenda-mode . origami-mode)))
   (make-qsags)
   (org-super-agenda-mode 1)
   (setq org-agenda-custom-commands
         '(("z" "custom agenda"
            ((agenda "" ((org-agenda-span 'week)
                         (org-super-agenda-groups nil)
                         (org-agenda-overriding-header "")))
             (alltodo "" ((org-agenda-overriding-header "")
                          )))))))

(after! org
  :config
  (defun display-ansi-colors ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
   (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)

   (map! :map org-mode-map
        "C-c C-'" #'org-yank-into-new-block
        "C-c C-." #'org-yank-into-new-elisp-block)

   (map! :map org-mode-map
         :localleader
         (:prefix ("z" . "yank to block")
          :nvm "b" #'org-yank-into-new-block
          :nvm "e" #'org-yank-into-new-block-elisp
          :nvm "s" #'org-yank-into-new-block-sh
          :nvm "h" #'org-yank-into-new-block-haskell
          :nvm "q" #'org-yank-into-new-quote)))

(defun org-yank-into-new-block (&optional template)
    (interactive)
    (let ((begin (point))
          done)
      (unwind-protect
          (progn
            (end-of-line)
            (yank)
            (push-mark begin)
            (setq mark-active t)
            (if template
             (org-insert-structure-template template)
             (call-interactively #'org-insert-structure-template))
            (setq done t)
            (deactivate-mark)
            (let ((case-fold-search t))
              (re-search-forward (rx bol "#+END_")))
            (forward-line 1))
        (unless done
          (deactivate-mark)
          (delete-region begin (point))))))

(defun org-yank-into-new-block-elisp ()
  (interactive)
  (org-yank-into-new-block "src elisp"))

(defun org-yank-into-new-block-sh ()
  (interactive)
  (org-yank-into-new-block "src sh"))

(defun org-yank-into-new-block-haskell ()
  (interactive)
  (org-yank-into-new-block "src haskell"))

(defun org-yank-into-new-quote ()
  (interactive)
  (org-yank-into-new-block "quote"))

(after! org
  :config
  (use-package backtrace)
  (setq org-hugo-base-dir "~/site"
        org-hugo-auto-set-lastmod t
        org-hugo-use-code-for-kbd t
        org-hugo-date-format "%Y-%m-%d")
    (map! :map org-mode-map
        :localleader
        (:nvm "lp" #'org-hugo-export-wim-to-md)))

(use-package! org-wild-notifier
  :defer t
  :config
  (add-hook! 'after-init-hook 'org-wild-notifier-mode)
  (setq ;;org-wild-notifier-alert-time 15
        alert-default-style (if IS-MAC 'osx-notifier 'libnotify)))

(after! org
  (use-package! org-random-todo
    :defer-incrementally t
    :commands (org-random-todo-mode
               org-random-todo
               org-random-todo-goto-current
               org-random-todo-goto-new)
    :config
    (setq org-random-todo-how-often 60000)
    (org-random-todo-mode 1))

  (after! alert
    (alert-add-rule :mode 'org-mode
                    :category "random-todo"
                    :style 'osx-notifier
                    :continue t)))

(after! deft
  (setq
   deft-directory "~/org"
   deft-extensions '("org" "txt" "md")
   deft-recursive t
   deft-file-naming-rules
   (quote
    ((noslash . "-")
     (nospace . "-")
     (case-fn . downcase)))
   deft-strip-summary-regexp "\\([
	]\\|^#\\+.+:.*$\\)"
   delete-by-moving-to-trash nil
   ))

;; haskell
;;
(after! haskell
  (setq
   haskell-font-lock-symbols t
   ;; company-idle-delay 0.5
   haskell-interactive-popup-errors nil
   lsp-enable-folding nil
   lsp-response-timeout 120
   ;; lsp-ui-sideline-enable nil
   lsp-ui-doc-enable nil
   ;; lsp-enable-symbol-highlighting nil
   ;; +lsp-prompt-to-install-server 'quiet
   lsp-modeline-diagnostics-scope :project
   ;; lsp-modeline-code-actions-segments '(count icon)
   flycheck-check-syntax-automatically '(save)
   lsp-haskell-brittany-on nil
   lsp-haskell-floskell-on nil
   lsp-haskell-fourmolu-on nil
   lsp-haskell-stylish-haskell-on nil
   lsp-haskell-retrie-on nil
   ;; lsp-completion-provider :none
   haskell-process-show-debug-tips nil
   haskell-process-suggest-remove-import-lines nil
   haskell-process-suggest-restart nil
   ;;haskell-process-type 'stack-ghci
   haskell-process-type 'cabal-repl
   )
  (global-so-long-mode -1)
  ;; makes underscore an alphanumeric
  (add-hook! 'haskell-mode-hook (modify-syntax-entry ?_ "w")))

(after! haskell
  (map! :localleader
        :map haskell-mode-map
        "n" #'flycheck-next-error
        "p" #'flycheck-previous-error))

(map!
  :after company
  :map company-active-map
  "RET" nil
  "<return>" nil
  "<tab>" #'company-complete-selection
  "TAB" #'company-complete-selection)
(setq tab-always-indent 'complete)

(after! haskell
      (sp-with-modes '(haskell-mode haskell-interactive-mode)
        (sp-local-pair "{-" "-}" :actions :rem)
        (sp-local-pair "{-#" "#-}" :actions :rem)
        (sp-local-pair "{-@" "@-}" :actions :rem)
        (sp-local-pair "{-" "-")
        (sp-local-pair "{-#" "#-")
        (sp-local-pair "{-@" "@-")))

(with-eval-after-load 'lsp-mode
  (defun lsp-next-checker-haskell ()
    (flycheck-add-next-checker 'lsp '(warning . haskell-hlint)))
  (add-hook 'lsp-after-open-hook
            #'lsp-next-checker-haskell))

(use-package! fd-haskell)

(use-package! haskell-lite)

(use-package! tidal
    :init
    (progn
      ;; (setq tidal-interpreter "ghci")
      ;; (setq tidal-interpreter-arguments (list "ghci" "-XOverloadedStrings" "-package" "tidal"))
      ;; (setq tidal-boot-script-path "~/.emacs.doom/.local/straight/repos/Tidal/BootTidal.hs")
      ))

(use-package! haskell-snippets
  :after (haskell-mode yasnippet))

(use-package! corfu
  :config
  ;;(corfu-global-mode)
  (setq corfu-auto t))

(use-package! cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package elmo
  :config
  (map! :leader :desc "elmo" "te" #'elmo-toggle)
)

(defun elmo-toggle ()
  (interactive)
  (if elmo-mode
      (progn
        (elmo-mode -1)
        (vertico-mode 1)
        (message "elmo-mode disabled"))
    (progn
      (elmo-mode 1)
      (vertico-mode -1)
      (message "elmo-mode enabled"))))

(use-package easy-kill
  :config
  (map! "M-w" #'easy-kill)
)

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package! avy)
(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

(setq org-latex-packages-alist '(("" "tikz-cd" t) ("" "tikz" t)))
