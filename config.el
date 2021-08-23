;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; other configuration examples
;; https://github.com/hlissner/doom-emacs-private
;; https://github.com/zaiste/.doom.d/blob/master/init.el
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
(setq doom-theme 'doom-Iosvkem)

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

;; latest start.scd for SC
;;
;; Server.local.options.sampleRate = 44100;
;; SuperDirt.start;
;; s.reboot

(use-package! tidal
    :init
    (progn
      ;; (setq tidal-interpreter "ghci")
      ;; (setq tidal-interpreter-arguments (list "ghci" "-XOverloadedStrings" "-package" "tidal"))
      ;; (setq tidal-boot-script-path "~/.emacs.doom/.local/straight/repos/Tidal/BootTidal.hs")
      ))

(setq-default
        deft-directory "~/org/notes"
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
)

(setq-default flyspell-mode nil)

(setq-default
    org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
    org-startup-folded 'overview
)

(remove-hook 'org-mode-hook 'flyspell-mode)

;;irc
;;       '(("irc.libera.chat"
;;         :port "6697"
;;         :ssl t
;;         :nick "scootah")))

(defun org-ext-random-entry (&optional arg)
  "Select and goto a random todo item from the global agenda"
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq arg org-agenda-overriding-arguments))
  (if (and (stringp arg) (not (string-match "\\S-" arg))) (setq arg nil))
  (let* ((today (org-today))
         (date (calendar-gregorian-from-absolute today))
         (kwds org-todo-keywords-for-agenda)
         (lucky-entry nil)
         (completion-ignore-case t)
         (org-select-this-todo-keyword
          (if (stringp arg) arg
            (and arg (integerp arg) (> arg 0)
                 (nth (1- arg) kwds))))
         rtn rtnall files file pos marker buffer)
    (when (equal arg '(4))
      (setq org-select-this-todo-keyword
            (org-icompleting-read "Keyword (or KWD1|K2D2|...): "
                                  (mapcar 'list kwds) nil nil)))
    (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
    (catch 'exit
      (org-compile-prefix-format 'todo)
      (org-set-sorting-strategy 'todo)
      (setq files (org-agenda-files nil 'ifmode)
            rtnall nil)
      (while (setq file (pop files))
        (catch 'nextfile
          (org-check-agenda-file file)
          (setq rtn (org-agenda-get-day-entries file date :todo))
          (setq rtnall (append rtnall rtn))))

      (when rtnall
        (setq lucky-entry
              (nth (random
                    (safe-length
                     (setq entries rtnall)))
                   entries))

        (setq marker (or (get-text-property 0 'org-marker lucky-entry)
                         (org-agenda-error)))
        (setq buffer (marker-buffer marker))
        (setq pos (marker-position marker))
        (org-pop-to-buffer-same-window buffer)
        (widen)
        (goto-char pos)
        (when (derived-mode-p 'org-mode)
          (org-show-context 'agenda)
          (save-excursion
            (and (outline-next-heading)
                 (org-flag-heading nil))) ; show the next heading
          (when (outline-invisible-p)
            (show-entry))               ; display invisible text
          (run-hooks 'org-agenda-after-show-hook))))))


(defun style-ext/default-frame ()
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
      (set-frame-parameter (selected-frame) 'top 30)
      (set-frame-parameter (selected-frame) 'left 11)
      (set-frame-parameter (selected-frame) 'height 49)
      (set-frame-parameter (selected-frame) 'width 100)
      (message "default-frame set")))
   ((string-equal system-type "gnu/linux") ; linux
    (progn
      (message "Linux")))))

;; (default-frame)
;; (add-hook 'after-init-hook 'default-frame)


(defun style-ext/default-right-frame ()
  (interactive)
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    (progn
      (set-frame-parameter (selected-frame) 'fullscreen nil)
      (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'top 10)
      (set-frame-parameter (selected-frame) 'left 2000)
      (set-frame-parameter (selected-frame) 'height 60)
      (set-frame-parameter (selected-frame) 'width 120)))
   ((string-equal system-type "darwin") ; Mac OS X
    (progn
      (set-frame-parameter (selected-frame) 'fullscreen nil)
      (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'top 20)
      (set-frame-parameter (selected-frame) 'left 735)
      (set-frame-parameter (selected-frame) 'height 47)
      (set-frame-parameter (selected-frame) 'width 85)))
   ((string-equal system-type "gnu/linux") ; linux
    (progn
      (message "Linux")))))

(defun style-ext/max-frame ()
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

(defun style-ext/toggle-frame-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (min-frame)
    (max-frame)))

(defun style-ext/default-face-attribute ()
  (interactive)
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    (progn
      (set-face-attribute
       'default nil
       :family "Source Code Pro"
       :height 140
       :weight 'normal
       :width 'normal)))
   ((string-equal system-type "darwin") ; Mac OS X
    (progn
      (set-face-attribute
       'default nil
       :family "M+ 1m"
       :height 140
       :weight 'normal
       :width 'normal)))
   ((string-equal system-type "gnu/linux") ; linux
    (progn
      (message "Linux")))))

(setq-default
 haskell-font-lock-symbols t
 lsp-enable-folding nil
 lsp-response-timeout 120
 org-agenda-custom-commands
   (quote
    (("n" "Agenda and all TODO's"
      ((agenda ""
               ((org-agenda-overriding-header "today")))
       (tags "REFILE"
             ((org-agenda-overriding-header "refile me pls")
              (org-tags-match-list-sublevels nil)))
       (todo "+TODO='NEXT'" nil)
       (todo "+TODO='TODO'" nil))
      nil)
     ("z" "draft full agenda"
      ((agenda ""
               ((org-agenda-overriding-header "today")))
       (tags "REFILE"
             ((org-agenda-overriding-header "refiles")))
       (tags "TODO=\"TODO\"+SCHEDULED>\"<today>\""
             ((org-agenda-overriding-header "scheduled")))
       (tags "TODO=\"TODO\"-SCHEDULED<=\"<today>\"-SCHEDULED>\"<today>\""
             ((org-agenda-overriding-header "the rest")))
       (tags "TODO=\"DONE\""
             ((org-agenda-overriding-header "done and dusted"))))
      nil nil)
     ("x" "sandpit" tags "+SCHEDULED>=\"<2008-10-11>\"" nil)))
 org-capture-templates
 (quote
    (("r" "refile" entry
      (file "~/org/refile.org")
      "* TODO %?
")
     ("z" "bugz" entry
      (file+headline "~/org/bugz.org" "bugz!")
      "* TODO %?
%a")))
 org-support-shift-select t
 org-startup-folded t
 )


;; IMO, modern editors have trained a bad habit into us all: a burning need for
;; completion all the time -- as we type, as we breathe, as we pray to the
;; ancient ones -- but how often do you *really* need that information? I say
;; rarely. So opt for manual completion:
(setq company-idle-delay nil)

;; Disable invasive lsp-mode features
(setq lsp-ui-sideline-enable nil   ; not anymore useful than flycheck
      lsp-ui-doc-enable nil        ; slow and redundant with K
      lsp-enable-symbol-highlighting nil
      ;; If an LSP server isn't present when I start a prog-mode buffer, you
      ;; don't need to tell me. I know. On some systems I don't care to have a
      ;; whole development environment for some ecosystems.
      +lsp-prompt-to-install-server 'quiet)

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)
