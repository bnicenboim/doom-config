;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Bruno Nicenboim"
      user-mail-address "")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
;; Load
(load! "keybindings")
;; ESS


;; regions to be ignored by ispell. For .Rmd files
;; https://superuser.com/questions/345084/how-to-exclude-in-flyspell-mode-and-flyspell-buffer
;;https://emacs.stackexchange.com/questions/44132/mmm-mode-and-flyspell
;; https://emacs.stackexchange.com/questions/36011/make-flyspell-avoid-checking-includes-in-c
;;https://stackoverflow.com/questions/4671908/how-to-make-flyspell-bypass-some-words-by-context
;;https://stackoverflow.com/questions/8287330/exempt-code-chunks-in-an-sweave-document-from-emacs-spell-check
;;https://stackoverflow.com/questions/28942860/emacs-flyspell-disable-for-custom-latex-macros
(after! ispell
  (pushnew! ispell-skip-region-alist
            '("^```" . "```$")
            '("^`" . "`$")
            '("^[" . "]$")
            '("^{" . "}$"))
  )



;; For ess / R
(add-hook 'ess-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(after! ess
    ;; (require 'ess-site)
    ;; (require 'ess-mode)
    ;; (require 'ess-r-mode)
  (setq ;ess-ask-for-ess-directory nil        ; start R in the working directory by default
        ess-fancy-comments nil ; comments in r dont get tabbed
        ess-eval-empty t             ; don't skip non-code lines.
        ess-auto-width 'window      ;  auto witdh
        ;; ess-use-flymake nil    ; let lsp manage lintr?
        ess-eval-visibly 'nowait   ;; Stop R repl eval from blocking emacs.
        ess-ask-for-ess-directory t
        ess-local-process-name "R"
        ansi-color-for-comint-mode 'filter
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t
        )
  (set-company-backend! 'ess-r-mode
    '(
      ;;company-capf ;;not sure if needed or autoloaded
      ;;company-yasnippet
      company-R-args
      company-R-objects
      company-ess
      company-dabbrev-code
      company-files
      :separate))


  ;; Code indentation copied from my old config.
  ;; Follow Hadley Wickham's R style guide
  (setq
   ess-style 'RStudio
   ess-offset-continued 2
   ess-expression-offset 0)



  (defun ess-r-pkgdown-build-site (&optional arg)
  "Interface for `devtools::load_all()'.
With prefix ARG ask for extra args."
  (interactive "P")
  (ess-r-package-eval-linewise
   "pkgdown::build_site(%s)\n" "Building site %s" arg
   '("" (read-string "Arguments: " "preview = FALSE"))))


  )


;;;; STAN
;;; stan-mode.el
(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

;;; company-stan.el
(use-package company-stan
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil))

;;; eldoc-stan.el
(use-package eldoc-stan
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; flycheck-stan.el
(use-package flycheck-stan
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup))
  :config
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc2'
  (setq flycheck-stanc-executable nil)
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc3'
  (setq flycheck-stanc3-executable nil))

;;; stan-snippets.el
(use-package stan-snippets
  :hook (stan-mode . stan-snippets-initialize)
  ;;
  :config
  ;; No configuration options as of now.
  )




(use-package! hl-todo
  :config
  (setq global-hl-todo-mode t)
  )

(defun bookdown-git ()
    "Compile a gitbook." ; Doc string.
    (interactive)         ; Specify is command.
    (let ((fileNameSuffix (file-name-extension (buffer-file-name) ) ))
    (cond
     (;(eq major-mode 'poly-mode) ; First condition
      (string= fileNameSuffix "Rmd" )
      ;;
      (async-shell-command  "R -e  'bookdown::render_book(\"index.Rmd\", \"bookdown::gitbook\")'"))
      ;; (async-shell-command (concat "latex command" buffer-file-name)))
     )
     (t ; Default condition
      (message "ERR: My Comp command: Compilation command not defined for this major mode."))))

(defun bookdown-pdf ()
    "Compile a pdf_book." ; Doc string.
    (interactive)         ; Specify is command.
    (let ((fileNameSuffix (file-name-extension (buffer-file-name) ) ))
      (cond
       (;(eq major-mode 'poly-mode) ; First condition
        (string= fileNameSuffix "Rmd" )
        ;;
        (async-shell-command  "R -e  'bookdown::render_book(\"index.Rmd\", \"bookdown::pdf_book\")'"))
       ;; (async-shell-command (concat "latex command" buffer-file-name)))
       )
      (t ; Default condition
       (message "ERR: My Comp command: Compilation command not defined for this major mode."))))




  ;; Disable the mouse support in X11 terminals in order to enable copying/pasting with the mouse
  (xterm-mouse-mode -1)
  ;; Enable mouse support
  (unless window-system
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;;undo
(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space


 ;; soft wrap around words
(global-visual-line-mode 1)


(setq projectile-project-search-path '("~/dev"))


(setq evil-multiedit-follow-matches  't)



(setq-default history-length 1000)
(setq-default prescient-history-length 1000)




(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :config
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "C-p" #'lexic-search-history-backwards
        :n "C-n" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search))))


(defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
  :override #'+lookup/dictionary-definition
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (lexic-search identifier nil nil t))

;; dict
;;
(setq ispell-personal-dictionary "~/.doom.d/my-personal-dict")

;; https://community.languagetool.org/ruleEditor2/
;; https://dev.languagetool.org/tips-and-tricks
(use-package!  langtool
  :commands
  (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)

   :init
   (setq langtool-default-language "en-US")
   :config
   (setq langtool-language-tool-jar "/home/bruno/.language-tools/languagetool-commandline.jar")
   (setq langtool-java-classpath
      "/home/bruno/.language-tools/*")
;;  (langtool-language-tool-server-jar "~/bin/LanguageTool/languagetool-server.jar")
;;  (langtool-server-user-arguments '("-p" "8082")
   (setq langtool-disabled-rules '("WHITESPACE_RULE" "EN_QUOTES"))
  )

(after! evil-snipe
  (evil-snipe-mode -1))


;; config other checkers
;; check config in flycheck-verify-setup
;; https://www.macs.hw.ac.uk/~rs46/posts/2018-12-29-textlint-flycheck.html
;; https://aliquote.org/post/text-linting/
;;https://github.com/amperser/proselint
;;
(setq flycheck-disabled-checkers '(proselint))
(setq flycheck-markdown-mdl-rules '(
"MD001"  "MD002"  "MD003"  "MD004"  "MD005"  "MD006"  "MD007"  "MD008"  "MD009"
 "MD010" "MD011" "MD012"
 ;;"MD013" ;; long lines
 "MD014" "MD015" "MD016" "MD017" "MD018"
 "MD019" "MD020" "MD021" "MD022" "MD023" "MD024" "MD025" "MD026" "MD027"
 "MD028" "MD029" "MD030" "MD031" "MD032" "MD033" "MD034" "MD035" "MD036"
 "MD037" "MD038" "MD039" "MD040" "MD041" "MD042" "MD043" "MD044" "MD045"
 "MD046"
))

(eval-after-load 'markdown-mode            '(require 'smartparens-markdown))

(after! reformatter
  :config
  (defconst Rscript-command "Rscript")
  (reformatter-define styler
    :program Rscript-command
    :args (list "--vanilla" "-e" "con <- file(\"stdin\")
out <- styler::style_text(readLines(con))
close(con)
out")
    :lighter " styler"))

 ;; (use-package! helm-bibtex
;;   :config
  (setq bibtex-completion-bibliography
        "/home/bruno/ownCloud/BIBFILE/bib.bib")
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-notes-path "/home/bruno/ownCloud/BIBFILE/notes.org")
  (setq bibtex-completion-additional-search-fields '(keywords))
  (setq bibtex-completion-pdf-symbol "⌘")
   (setq bibtex-completion-notes-symbol "✎")
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (call-process "evince" nil 0 nil fpath)))

(global-whitespace-mode +1)
;; whitespace-mode
;; free of trailing whitespace and to use 80-column width, standard indentation
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 80)

;; remove the woven and exported from Rnw files to pdf
(setq polymode-weaver-output-file-format "%s")
  (setq polymode-exporter-output-file-format "%s")
