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
                                  ; regions to be ignored by ispell. For .Rmd files
(add-to-list 'ispell-skip-region-alist '("^```" . "```$"))
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

;; ;; standard control-enter evaluation
;; (define-key ess-mode-map (kbd "<C-return>") 'ess-eval-region-or-line-and-step)
;; (define-key ess-mode-map (kbd "<normal-state><C-return>") 'ess-eval-region-or-line-and-step)
;; (define-key ess-mode-map (kbd "<C-S-return>") 'ess-eval-buffer)
;; (define-key ess-mode-map [remap ess-indent-or-complete] #'company-indent-or-complete-common)

(setq-hook! 'ess-r-mode-hook comment-line-break-function nil)

;; from https://github.com/paullemmens/dot_doom.d/blob/master/config.el
  (add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)
  (setq! ess-use-flymake nil)
  (setq! lsp-ui-doc-enable nil
         lsp-ui-doc-delay 1.5)

  ;; Code indentation copied from my old config.
  ;; Follow Hadley Wickham's R style guide
  (setq
   ess-style 'RStudio
   ess-offset-continued 2
   ess-expression-offset 0)

  (setq comint-move-point-for-output t)

  ;; From https://emacs.readthedocs.io/en/latest/ess__emacs_speaks_statistics.html
  ;; TODO: find out a way to make settings generic so that I can also set ess-inf-R-font-lock-keywords
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers  . t)
          (ess-R-fl-keyword:fun-defs   . t)
          (ess-R-fl-keyword:keywords   . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants  . t)
          (ess-fl-keyword:fun-calls    . t)
          (ess-fl-keyword:numbers      . t)
          (ess-fl-keyword:operators    . t)
          (ess-fl-keyword:delimiters) ; don't because of rainbow delimiters
          (ess-fl-keyword:=            . t)
          (ess-R-fl-keyword:F&T        . t)
          (ess-R-fl-keyword:%op%       . t)))

  ;; ESS buffers should not be cleaned up automatically
  (add-hook 'inferior-ess-mode-hook #'doom-mark-buffer-as-real-h)
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



;; https://github.com/polymode/polymode/issues/214
(add-hook 'markdown-mode-hook (lambda ()
                                (when (null buffer-undo-tree) (setq buffer-undo-tree (make-undo-tree)))))

(after! evil-surround
  (add-hook 'evil-markdown-mode-hook (lambda ()
   (push '(?* . ("* " . " *")) evil-surround-pairs-alist)))
)


;; zoom in and out
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; window split
(map! :leader "w /" 'evil-window-vsplit)



  ;; Disable the mouse support in X11 terminals in order to enable copying/pasting with the mouse
  (xterm-mouse-mode -1)
  ;; Enable mouse support
  (unless window-system
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
  ;; finer undo
  (setq evil-want-fine-undo 'fine)

; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map (kbd "<down>") 'evil-next-visual-line)
  (define-key evil-motion-state-map  (kbd "<up>")  'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map  (kbd "<down>")  'evil-next-visual-line)
  (define-key evil-visual-state-map  (kbd "<up>")  'evil-previous-visual-line)
  ;; soft wrap around words
  (add-hook 'text-mode-hook #'visual-line-mode)

;; disallows Hebrew/arabic, it might speed text
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)



(setq avy-all-windows t) ;gs works across windows
                         ;

(setq
 projectile-project-search-path '("~/dev"))

;;; helm fuzzy
(setq helm-M-x-fuzzy-match                  t
      helm-bookmark-show-location           t
      helm-buffers-fuzzy-matching           t
      helm-completion-in-region-fuzzy-match t
      helm-file-cache-fuzzy-match           t
      helm-imenu-fuzzy-match                t
      helm-mode-fuzzy-match                 t
      helm-locate-fuzzy-match               t
      helm-quick-update                     t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t)
