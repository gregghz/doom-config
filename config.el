;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gregg Hernandez"
      user-mail-address "gregg.hernandez@nike.com")

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
(setq doom-font (font-spec :family "Source Code Variable" :size 13))

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
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory)))

(after! compile
  (add-to-list 'compilation-error-regexp-alist-alist '(bloop "^\\[E\\] \\([A-Za-z0-9\\._/-]+\\):\\([0-9]+\\):\\([0-9]+\\):.*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'bloop)
  ;(add-hook 'comint-mode-hook +word-wrap-mode)
)

(add-hook 'compilation-mode #'+word-wrap-mode)

(use-package! bloop
  :load-path "~/.doom.d/bloop"
  :after scala-mode
  :config
  (set-popup-rule! "^\\*bloop-.*\\*$" :side 'bottom :quit 'current :select nil :height 30)
  (map! :map scala-mode-map
        :leader
        (:prefix ("c b" . "Bloop")
         :desc "Compile" "c" 'bloop-compile
         :desc "Compile Cascade" "y" 'bloop-cascade-compile
         :desc "Test Compile" "j" 'bloop-test-compile
         :desc "Test" "t" 'bloop-test
         :desc "Test-only" "o" 'bloop-test-only
         :desc "Clean" "l" 'bloop-clean
         :desc "Repeat" "a" 'bloop-repeat)))

(use-package! groovy-mode
  :config
  (setq groovy-indent-offset 2))

(use-package! lsp-metals
  :config
  (setq
   lsp-metals-super-method-lenses-enabled t
   lsp-metals-show-implicit-arguments t
   lsp-metals-show-implicit-conversions-and-classes t
   lsp-metals-show-inferred-type t))

(use-package! lsp-mode
  :config
  (setq
   lsp-file-watch-threshold 20000
   lsp-headerline-breadcrumb-enable t))

(use-package! projectile
  :config
  (setq projectile-project-search-path "~/workspace")

  (defun scalafilep (file)
    (and (or (string-suffix-p ".scala" file) (string-suffix-p ".sbt" file)) (file-exists-p file)))

  (defun prepend-path (files path)
    (defun pp (file)
      (concat path "/" file))

    (mapcar 'pp files))

  (defun scalafmt-project ()
    (interactive)
    (let* ((root (projectile-project-root))
           (changed-files (prepend-path (magit-changed-files "master") root))
           (filtered-changed-files (seq-filter 'scalafilep changed-files))
           (changed-files-str (string-join filtered-changed-files " "))
           (cmd (format "scalafmt -c %s/.scalafmt.conf %s" root changed-files-str)))
      (message (format "Formatting %s" changed-files-str))
      (shell-command cmd))))

(use-package! sbt-mode
  :bind (("C-c C-h" . sbt-hydra))
  :after scala-mode
  :hook
  (sbt-mode . +word-wrap-mode)
  :config
  (setq compilation-auto-jump-to-first-error t)
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package! scala-mode
  :after bloop
  :mode "\\.s\\(cala\\|bt\\|c\\)$"
  :bind (("C-c C-h" . sbt-hydra)))

(use-package! typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package! web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
