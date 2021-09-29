;;; .keybindings.el -*- lexical-binding: t; -*-

(map!
 (:map ess-mode-map
  :nv "<C-return>" #'ess-eval-region-or-line-and-step
  :nviom "<C-S-return>" #'ess-eval-buffer)

;; (define-key ess-mode-map (kbd "<normal-state><C-return>") 'ess-eval-region-or-line-and-step)
;; (define-key ess-mode-map (kbd "<C-S-return>") 'ess-eval-buffer)

 (:map ess-roxy-mode-map
     :i     "RET" #'ess-indent-new-comment-line
     ;; below two "prettier" versions that should work, but don't (for me)
     ;; [remap newline]            #'ess-indent-new-comment-line
     ;; [remap newline-and-indent] #'ess-indent-new-comment-line
    )
   ;; Assign Ctrl-Up/Down to search in ESS history based on string entered. See
   ;; https://ess.r-project.org/Manual/ess.html#Command-History and
   ;; https://stat.ethz.ch/pipermail/ess-help/2007-June/004150.html
   (:map comint-mode-map
     :nviom [C-up]   #'comint-previous-matching-input-from-input
     :nviom [C-down] #'comint-next-matching-input-from-input
     ;; I want to have C-j/k to be next/prev command in normal and insert state,
     ;; but for normal I can't get it to work:
     :nviom "C-k"    #'comint-previous-input
     :nviom "C-j"    #'comint-next-input)


   ;; R studio style:
;; (ess-r-devtools-install-package &optional ARG)

   (:map ess-r-package-mode-map
    "C-S-b" #'ess-r-devtools-install-package
    "C-S-t" #'ess-r-devtools-test-package
    )

   )


(after! flyspell
  (map! :map flyspell-mouse-map
      "<down-mouse-3>" #'flyspell-correct-word
      "<mouse-3>" 'undefined
      "<down-mouse-1>" nil
      "<mouse-1>" nil
      )
  )

(map!
 "<f11>" #'define-word-at-point
 "<f12>" #'flyspell-correct-at-point
 )

;; zoom in and out
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; window split
(map! :leader "w /" 'evil-window-vsplit)

; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map (kbd "<down>") 'evil-next-visual-line)
  (define-key evil-motion-state-map  (kbd "<up>")  'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map  (kbd "<down>")  'evil-next-visual-line)
  (define-key evil-visual-state-map  (kbd "<up>")  'evil-previous-visual-line)


;; use capslock instead of esc
;; (global-set-key '[8711] '<escape>)

;; map! is a macro defined in core-keybinds.el.

;; Signature
;; (map! &rest REST)

;; Documentation
;; A convenience macro for defining keybinds, powered by general.

;; If evil isn't loaded, evil-specific bindings are ignored.

;; States
;;   :n  normal
;;   :v  visual
;;   :i  insert
;;   :e  emacs
;;   :o  operator
;;   :m  motion
;;   :r  replace
;;   :g  global  (binds the key without evil current-global-map)

;;   These can be combined in any order, e.g. :nvi will apply to normal, visual and
;;   insert mode. The state resets after the following key=>def pair. If states are
;;   omitted the keybind will be global (no emacs state; this is different from
;;   evil's Emacs state and will work in the absence of evil-mode).

;; Properties
;;   :leader [...]                   an alias for (:prefix doom-leader-key ...)
;;   :localleader [...]              bind to localleader; requires a keymap
;;   :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
;;   :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
;;   :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
;;                                   can be a cons cell: (PREFIX . DESCRIPTION)
;;   :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
;;                                   where the following keys will be bound. DO NOT
;;                                   USE THIS IN YOUR PRIVATE CONFIG.
;;   :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
;;   :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
;;   :when [CONDITION] [...]
;;   :unless [CONDITION] [...]

;;   Any of the above properties may be nested, so that they only apply to a
;;   certain group of keybinds.

;; Demos
;; #+BEGIN_SRC elisp :eval no
;; (map! :map magit-mode-map
;;       :m  "C-r" 'do-something           ; C-r in motion state
;;       :nv "q" 'magit-mode-quit-window   ; q in normal+visual states
;;       "C-x C-r" 'a-global-keybind
;;       :g "C-x C-r" 'another-global-keybind  ; same as above

;;       (:when IS-MAC
;;         :n "M-s" 'some-fn
;;         :i "M-o" (cmd! (message "Hi"))))

;; (map! (:when (featurep! :completion company) ; Conditional loading
;;         :i "C-@" #'+company/complete
;;         (:prefix "C-x"                       ; Use a prefix key
;;           :i "C-l" #'+company/whole-lines)))

;; (map! (:when (featurep! :lang latex)    ; local conditional
;;         (:map LaTeX-mode-map
;;           :localleader                  ; Use local leader
;;           :desc "View" "v" #'TeX-view)) ; Add which-key description
;;       :leader                           ; Use leader key from now on
;;       :desc "Eval expression" ";" #'eval-expression)
;; #+END_SRC

;; These are side-by-side comparisons, showing how to bind keys with and without
;; ~map!~:

;; #+BEGIN_SRC elisp :eval no
;; ;; bind a global key
;; (global-set-key (kbd "C-x y") #'do-something)
;; (map! "C-x y" #'do-something)

;; ;; bind a key on a keymap
;; (define-key emacs-lisp-mode-map (kbd "C-c p") #'do-something)
;; (map! :map emacs-lisp-mode-map "C-c p" #'do-something)

;; ;; unbind a key defined elsewhere
;; (define-key lua-mode-map (kbd "SPC m b") nil)
;; (map! :map lua-mode-map "SPC m b" nil)

;; ;; bind multiple keys
;; (global-set-key (kbd "C-x x") #'do-something)
;; (global-set-key (kbd "C-x y") #'do-something-else)
;; (global-set-key (kbd "C-x z") #'do-another-thing)
;; (map! "C-x x" #'do-something
;;       "C-x y" #'do-something-else
;;       "C-x z" #'do-another-thing)

;; ;; bind global keys in normal mode
;; (evil-define-key* 'normal 'global
;;   (kbd "C-x x") #'do-something
;;   (kbd "C-x y") #'do-something-else
;;   (kbd "C-x z") #'do-another-thing)
;; (map! :n "C-x x" #'do-something
;;       :n "C-x y" #'do-something-else
;;       :n "C-x z" #'do-another-thing)

;; ;; or on a deferred keymap
;; (evil-define-key 'normal emacs-lisp-mode-map
;;   (kbd "C-x x") #'do-something
;;   (kbd "C-x y") #'do-something-else
;;   (kbd "C-x z") #'do-another-thing)
;; (map! :map emacs-lisp-mode-map
;;       :n "C-x x" #'do-something
;;       :n "C-x y" #'do-something-else
;;       :n "C-x z" #'do-another-thing)

;; ;; or multiple maps
;; (dolist (map (list emacs-lisp-mode go-mode-map ivy-minibuffer-map))
;;   (evil-define-key '(normal insert) map
;;     "a" #'a
;;     "b" #'b
;;     "c" #'c))
;; (map! :map (emacs-lisp-mode go-mode-map ivy-minibuffer-map)
;;       :ni "a" #'a
;;       :ni "b" #'b
;;       :ni "c" #'c)

;; ;; or in multiple states (order of states doesn't matter)
;; (evil-define-key* '(normal visual) emacs-lisp-mode-map (kbd "C-x x") #'do-something)
;; (evil-define-key* 'insert emacs-lisp-mode-map (kbd "C-x x") #'do-something-else)
;; (evil-define-key* '(visual normal insert emacs) emacs-lisp-mode-map (kbd "C-x z") #'do-another-thing)
;; (map! :map emacs-lisp-mode
;;       :nv   "C-x x" #'do-something      ; normal+visual
;;       :i    "C-x y" #'do-something-else ; insert
;;       :vnie "C-x z" #'do-another-thing) ; visual+normal+insert+emacs

;; ;; You can nest map! calls:
;; (evil-define-key* '(normal visual) emacs-lisp-mode-map (kbd "C-x x") #'do-something)
;; (evil-define-key* 'normal go-lisp-mode-map (kbd "C-x x") #'do-something-else)
;; (map! (:map emacs-lisp-mode :nv "C-x x" #'do-something)
;;       (:map go-lisp-mode    :n  "C-x x" #'do-something-else))
;; #+END_SRC
