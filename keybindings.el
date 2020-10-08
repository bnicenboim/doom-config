;;; .keybindings.el -*- lexical-binding: t; -*-

(map!

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


;; (ess-r-devtools-install-package &optional ARG)

"C-S-b" #'ess-r-devtools-install-package
)
