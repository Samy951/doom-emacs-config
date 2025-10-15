;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Configure lsp-mode with lsp-booster for performance
(after! lsp-mode
  ;; Enable lsp-booster for better performance
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)
               (not (file-remote-p default-directory))
               lsp-use-plists
               (not (functionp 'json-rpc-connection))
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))

  ;; Enable plists for better performance
  (setq lsp-use-plists t)

  ;; Add advice for lsp-booster
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  ;; Enable inlay hints
  (setq lsp-inlay-hint-enable t)

  ;; Configure JavaScript inlay hints (works for TypeScript too)
  (setq lsp-javascript-display-enum-member-value-hints t
        lsp-javascript-display-parameter-name-hints "all"
        lsp-javascript-display-parameter-name-hints-when-argument-matches-name t
        lsp-javascript-display-parameter-type-hints t
        lsp-javascript-display-property-declaration-type-hints t
        lsp-javascript-display-return-type-hints t
        lsp-javascript-display-variable-type-hints t)

  ;; Configure TypeScript inlay hints
  (setq lsp-typescript-display-enum-member-value-hints t
        lsp-typescript-display-parameter-name-hints "all"
        lsp-typescript-display-parameter-name-hints-when-argument-matches-name t
        lsp-typescript-display-parameter-type-hints t
        lsp-typescript-display-property-declaration-type-hints t
        lsp-typescript-display-return-type-hints t
        lsp-typescript-display-variable-type-hints t)

  ;; Performance tuning
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-completion-provider :capf
        lsp-enable-file-watchers nil)

  ;; UI settings
  (setq lsp-headerline-breadcrumb-enable t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t)

  ;; Enable code lens (show references, implementations inline)
  (setq lsp-lens-enable t))

;; Configure lsp-ui for optimal TypeScript/NestJS development
(after! lsp-ui
  ;; lsp-ui-sideline: Show hover info, diagnostics and code actions on the side
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t           ; Show hover messages
        lsp-ui-sideline-show-diagnostics t     ; Show flycheck errors
        lsp-ui-sideline-show-code-actions t    ; Show quick fixes
        lsp-ui-sideline-update-mode 'point     ; Update when cursor moves
        lsp-ui-sideline-delay 0.2)             ; Show quickly

  ;; lsp-ui-peek: Preview definitions/references without leaving current file
  (setq lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t           ; Show file paths
        lsp-ui-peek-fontify 'always)           ; Syntax highlight previews

  ;; lsp-ui-doc: Show documentation in child frame
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point          ; Show doc at cursor (less intrusive)
        lsp-ui-doc-delay 0.5                   ; Wait 0.5s before showing
        lsp-ui-doc-show-with-cursor t          ; Show when hovering with cursor
        lsp-ui-doc-show-with-mouse nil)        ; Don't show with mouse (prefer K key)

  ;; lsp-ui-imenu: Hierarchical view of file symbols
  (setq lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'left       ; Show symbol kind on left
        lsp-ui-imenu-auto-refresh t))          ; Auto-refresh on changes

;; Customize lsp-ui-sideline appearance to make it distinct from comments
(custom-set-faces!
  '(lsp-ui-sideline-global :foreground "#88C0D0" :background "#2E3440" :italic nil)
  '(lsp-ui-sideline-current-symbol :foreground "#88C0D0" :background "#3B4252" :weight bold)
  '(lsp-ui-sideline-code-action :foreground "#EBCB8B" :weight bold)
  '(lsp-ui-sideline-symbol-info :foreground "#81A1C1" :italic t))

;; Remove underline from breadcrumb to avoid confusion with type errors
(custom-set-faces!
  '(lsp-headerline-breadcrumb-path-face :underline nil)
  '(lsp-headerline-breadcrumb-separator-face :underline nil)
  '(lsp-headerline-breadcrumb-symbols-face :underline nil))

;; Force tree-sitter modes for TypeScript
(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))

;; Auto-start lsp in TypeScript files
(add-hook 'typescript-ts-mode-hook #'lsp!)
(add-hook 'tsx-ts-mode-hook #'lsp!)
(add-hook 'js-ts-mode-hook #'lsp!)

;; Configure tree-sitter grammars location for Doom
(setq treesit-extra-load-path '("~/.config/emacs/.local/etc/tree-sitter"))

;; Configure tree-sitter grammars
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")))

;; Auto-install tree-sitter grammars on first use
(defun +treesit-install-all-languages ()
  "Install all configured tree-sitter language grammars."
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; Configure dape for Node.js/TypeScript debugging
(after! dape
  ;; Info buffers to the right (like VSCode)
  (setq dape-buffer-window-arrangement 'right)

  ;; Enable inlay hints during debugging
  (setq dape-inlay-hints t)

  ;; Save breakpoints between sessions
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  (add-hook 'after-init-hook #'dape-breakpoint-load)

  ;; Configuration personnalisée pour NestJS
  ;; D'abord installer l'adapter: M-x dape-install-adapter RET js-debug RET
  (add-to-list 'dape-configs
               `(nestjs
                 modes (typescript-ts-mode typescript-mode js-ts-mode js-mode)
                 ensure dape-ensure-command
                 fn (dape-config-autoport dape-config-tramp)
                 command "node"
                 command-args ["~/.config/emacs/.local/etc/dape/js-debug/src/dapDebugServer.js" :autoport]
                 :type "pwa-node"
                 :request "attach"
                 :cwd dape-cwd-fn
                 :address "127.0.0.1"
                 :port 9229
                 :skipFiles ["<node_internals>/**" "**/node_modules/**"]
                 :sourceMaps t
                 :resolveSourceMapLocations ["**" "!**/node_modules/**"]
                 :outFiles ["${workspaceFolder}/dist/**/*.js"]
                 :sourceMapPathOverrides (:./* "${workspaceFolder}/src/*"
                                          :../../..//* "${workspaceFolder}/src/*"
                                          :../../../..//* "${workspaceFolder}/src/*"
                                          :webpack:///./* "${workspaceFolder}/*"
                                          :webpack:///* "${workspaceFolder}/*"))))
