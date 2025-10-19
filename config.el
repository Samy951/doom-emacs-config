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
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;;; ============================================================================
;;; Configuration Org Mode & Agenda
;;; ============================================================================

;; Fichiers pour l'agenda - ajoutez tous vos fichiers .org ici
(setq org-agenda-files '("~/org/agenda.org"
                         "~/org/tasks.org"
                         "~/org/work.org"
                         "~/org/personal.org"))

;; États des tâches avec raccourcis clavier
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
          (sequence "IDEA(i)" "PLANNING(l)" "READY(r)" "|" "COMPLETED(C)")))

  ;; Couleurs pour les états
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ff6c6b" :weight bold))
          ("IN-PROGRESS" . (:foreground "#ECBE7B" :weight bold))
          ("WAITING" . (:foreground "#a9a1e1" :weight bold))
          ("DONE" . (:foreground "#98be65" :weight bold))
          ("CANCELLED" . (:foreground "#5B6268" :weight bold))
          ("IDEA" . (:foreground "#51afef" :weight bold))
          ("PLANNING" . (:foreground "#46D9FF" :weight bold))
          ("READY" . (:foreground "#a9a1e1" :weight bold))
          ("COMPLETED" . (:foreground "#98be65" :weight bold))))

  ;; Tags personnalisés
  (setq org-tag-alist
        '((:startgroup . nil)
          ("@work" . ?w)
          ("@home" . ?h)
          ("@errands" . ?e)
          (:endgroup . nil)
          ("urgent" . ?u)
          ("important" . ?i)
          ("meeting" . ?m)
          ("dev" . ?d)
          ("bug" . ?b)
          ("feature" . ?f)))

  ;; Configuration de l'agenda
  (setq org-agenda-span 7                    ; Vue sur 7 jours
        org-agenda-start-on-weekday 1        ; Commence le lundi
        org-agenda-start-day nil             ; Commence aujourd'hui
        org-agenda-show-all-dates t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-timestamp-if-done t)

  ;; Vues personnalisées de l'agenda
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))
            (todo "IN-PROGRESS"
                  ((org-agenda-overriding-header "En cours")))
            (todo "TODO"
                  ((org-agenda-overriding-header "À faire")
                   (org-agenda-max-entries 5)))
            (tags-todo "urgent"
                       ((org-agenda-overriding-header "Urgent")))))

          ("w" "Vue Travail"
           ((tags-todo "@work"
                       ((org-agenda-overriding-header "Tâches travail")))))

          ("h" "Vue Maison"
           ((tags-todo "@home"
                       ((org-agenda-overriding-header "Tâches maison")))))

          ("u" "Urgent"
           ((tags-todo "urgent"
                       ((org-agenda-overriding-header "Tâches urgentes")))))

          ("n" "Prochaines 7 jours"
           ((agenda ""
                    ((org-agenda-span 7)
                     (org-agenda-start-day "+0d")))))))

  ;; Configuration des priorités
  (setq org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-default ?C
        org-priority-faces
        '((?A . (:foreground "#ff6c6b" :weight bold))
          (?B . (:foreground "#ECBE7B" :weight bold))
          (?C . (:foreground "#51afef"))
          (?D . (:foreground "#a9a1e1"))
          (?E . (:foreground "#5B6268"))))

  ;; Archivage
  (setq org-archive-location "~/org/archive/%s_archive::")

  ;; Log quand une tâche est complétée
  (setq org-log-done 'time
        org-log-into-drawer t)

  ;; Refile targets - pour déplacer des tâches entre fichiers
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil))

;; Templates de capture pour créer rapidement des notes
(after! org
  (setq org-capture-templates
        '(("t" "Tâche" entry (file+headline "~/org/tasks.org" "Inbox")
           "* TODO %?\n  SCHEDULED: %t\n  %i\n  %a")

          ("T" "Tâche avec deadline" entry (file+headline "~/org/tasks.org" "Inbox")
           "* TODO %?\n  DEADLINE: %^t\n  %i\n  %a")

          ("m" "Réunion" entry (file+headline "~/org/agenda.org" "Réunions")
           "* MEETING %? :meeting:\n  SCHEDULED: %^t\n  %i")

          ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
           "* %?\n  %i\n  %a")

          ("i" "Idée" entry (file+headline "~/org/ideas.org" "Idées")
           "* IDEA %?\n  %i\n  %U")

          ("b" "Bug" entry (file+headline "~/org/work.org" "Bugs")
           "* TODO %? :bug:\n  %i\n  %a")

          ("f" "Feature" entry (file+headline "~/org/work.org" "Features")
           "* TODO %? :feature:\n  %i\n  %a"))))

;; Configuration Org Roam (équivalent Obsidian)
(after! org-roam
  (setq org-roam-directory "~/org/roam/")

  ;; Templates pour org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n\n")
           :unnarrowed t)

          ("p" "project" plain "* Objectifs\n\n%?\n\n* Tâches\n\n* Notes"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :project:\n#+date: %U\n\n")
           :unnarrowed t)

          ("n" "note" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :note:\n#+date: %U\n\n")
           :unnarrowed t)))

  ;; Afficher le graph dans le navigateur
  (setq org-roam-graph-viewer (executable-find "firefox"))

  ;; Sync automatique
  (org-roam-db-autosync-mode))

;; Raccourcis clavier pour Org mode
(map! :leader
      (:prefix ("o" . "org")
       ;; :desc "Org Agenda" "a" #'org-agenda  ; Commenté: conflit avec Doom (utiliser SPC o a a)
       :desc "Org Capture" "c" #'org-capture
       :desc "Org Todo List" "t" #'org-todo-list
       :desc "Org Tags View" "m" #'org-tags-view
       :desc "Org Store Link" "l" #'org-store-link
       :desc "Org Insert Link" "L" #'org-insert-link
       :desc "Org Refile" "r" #'org-refile
       :desc "Org Archive" "A" #'org-archive-subtree)

      (:prefix ("n" . "notes")
       (:prefix ("r" . "roam")
        :desc "Find node" "f" #'org-roam-node-find
        :desc "Insert node" "i" #'org-roam-node-insert
        :desc "Capture" "c" #'org-roam-capture
        :desc "Graph" "g" #'org-roam-graph
        :desc "Show node" "r" #'org-roam-buffer-toggle
        :desc "Random node" "R" #'org-roam-node-random)
       (:prefix ("j" . "journal")
        :desc "Today" "j" #'org-journal-new-entry
        :desc "Search" "s" #'org-journal-search)))

;; Raccourcis dans les buffers org
(map! :map org-mode-map
      :localleader
      :desc "Schedule" "s" #'org-schedule
      :desc "Deadline" "d" #'org-deadline
      :desc "Priority" "p" #'org-priority
      :desc "Tags" "t" #'org-set-tags-command
      :desc "Refile" "r" #'org-refile
      :desc "Archive" "A" #'org-archive-subtree
      :desc "Toggle" "," #'org-ctrl-c-ctrl-c)

;; Configuration du journal
(after! org-journal
  (setq org-journal-dir "~/org/journal/"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-agenda-integration t)

  ;; Fonction helper pour org-capture
  (defun org-journal-find-location ()
    "Trouve ou crée l'entrée journal du jour pour org-capture."
    (org-journal-new-entry t)
    (unless (eq (point) (point-max))
      (org-forward-heading-same-level 1))
    (goto-char (point-max))))

;; Amélioration visuelle
(after! org
  ;; Indentation automatique
  (setq org-startup-indented t
        org-hide-leading-stars t
        org-odd-levels-only nil)

  ;; Rendu des images
  (setq org-startup-with-inline-images t)

  ;; Latex preview
  (setq org-preview-latex-default-process 'dvipng)

  ;; Beautify bullets
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))

  ;; Emphasis markers (gras, italique, etc.)
  (setq org-hide-emphasis-markers t))


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
  ;; Disable company auto-configuration (we use corfu)
  (setq lsp-completion-provider :none)
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
  (setq lsp-lens-enable t)

  ;; Enable GitHub Copilot
  (setq lsp-copilot-enabled t))

;; Configure lsp-ui for optimal TypeScript/NestJS development
(after! lsp-ui
  ;; lsp-ui-sideline: Uniquement les erreurs/diagnostics
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil         ; Désactivé
        lsp-ui-sideline-show-diagnostics t     ; UNIQUEMENT les erreurs TypeScript
        lsp-ui-sideline-show-code-actions nil  ; Désactivé (causait spam messages)
        lsp-ui-sideline-update-mode 'line      ; Update par ligne (moins agressif)
        lsp-ui-sideline-delay 0.5)             ; Délai augmenté pour éviter spam

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

;; Customize inlay hints to be distinct from comments (comments are green in homage-black)
(custom-set-faces!
  '(lsp-inlay-hint-face :foreground "#7c9fc9" :background unspecified :slant italic :height 0.9)
  '(lsp-inlay-hint-type-face :foreground "#7c9fc9" :background unspecified :slant italic :height 0.9)
  '(lsp-inlay-hint-parameter-face :foreground "#9d9fc9" :background unspecified :slant italic :height 0.9))

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

;;; ============================================================================
;;; Configuration DAP-mode pour NestJS/TypeScript debugging
;;; ============================================================================
;;;
;;; USAGE POUR DEBUGGER NESTJS:
;;; 1. Terminal: npm run start:debug
;;; 2. Emacs: Ouvre ton fichier .ts
;;; 3. Emacs: Ajoute "debugger;" dans ton code (les breakpoints visuels ne marchent pas avec ts-node)
;;; 4. Emacs: SPC m d d -> sélectionne "NestJS::Attach"
;;; 5. Fais ta requête HTTP
;;; 6. Le debugger s'arrêtera au "debugger;"
;;;
;;; Pour les tests Jest: SPC m d d -> "Node::Jest Test Current File"
;;; ============================================================================

(after! dap-mode
  ;; Active les contrôles UI pour le debugging
  (require 'dap-node)
  (dap-node-setup)  ;; Installer automatiquement le debugger Node.js

  ;; Configuration des features (SANS controls et tooltip qui causent window-live-p errors)
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions repl))

  ;; Active l'UI complète de DAP
  (dap-ui-mode 1)

  ;; NE PAS activer dap-ui-controls-mode - il cause des erreurs window-live-p
  ;; NE PAS activer dap-tooltip-mode - il cause aussi des erreurs

  ;; Configuration pour l'affichage des variables et de la ligne courante
  (setq dap-ui-variable-length 100)  ;; Longueur max des variables affichées

  ;; NE PAS configurer dap-ui-buffer-configurations - laisser les valeurs par défaut

  ;; Personnaliser l'apparence de la ligne d'exécution courante
  ;; dap-mode utilise automatiquement des overlays pour marquer la ligne courante
  (custom-set-faces!
   '(dap-ui-pending-breakpoint-face :background "#4c566a" :foreground "#d08770")
   '(dap-ui-verified-breakpoint-face :background "#3b4252" :foreground "#a3be8c")
   '(dap-ui-compile-errline :background "#bf616a" :foreground "#eceff4")
   '(dap-stopped-stack-frame :background "#5e81ac" :foreground "#eceff4" :weight bold))

  ;; Activer hl-line-mode dans les buffers de debug pour mieux voir la ligne courante
  ;; Version simplifiée qui ne dépend pas de fonctions internes
  (add-hook 'dap-stopped-hook
            (lambda (arg)
              (hl-line-mode 1)))

  ;; Ouvrir TOUS les panneaux quand le debugger démarre
  (add-hook 'dap-session-created-hook
            (lambda (session)
              (run-with-timer 0.1 nil #'dap-ui-show-many-windows)))

  ;; Sauvegarde des breakpoints entre les sessions
  (setq dap-breakpoints-file (expand-file-name ".dap-breakpoints" doom-cache-dir))

  ;; Configuration pour Jest avec Node.js (d'après doc dap-mode)
  (dap-register-debug-template
   "Node::Jest Test Current File"
   (list :type "node"
         :request "launch"
         :name "Jest Test Current File"
         :program "${workspaceFolder}/node_modules/.bin/jest"
         :args (list "--runInBand" "--no-cache" "--no-coverage" "${file}")
         :cwd "${workspaceFolder}"
         :sourceMaps t
         :protocol "inspector"
         :console "integratedTerminal"))

  ;; Configuration pour Jest avec tous les tests
  (dap-register-debug-template
   "Node::Jest All Tests"
   (list :type "node"
         :request "launch"
         :name "Jest All Tests"
         :program "${workspaceFolder}/node_modules/.bin/jest"
         :args (list "--runInBand" "--no-cache" "--no-coverage")
         :cwd "${workspaceFolder}"
         :sourceMaps t
         :protocol "inspector"
         :console "integratedTerminal"))

  ;; Configuration pour Jest avec un test spécifique (e2e)
  (dap-register-debug-template
   "Node::Jest E2E Test"
   (list :type "node"
         :request "launch"
         :name "Jest E2E Test"
         :program "${workspaceFolder}/node_modules/.bin/jest"
         :args (list "--config" "./test/jest-e2e.json" "--runInBand" "${file}")
         :cwd "${workspaceFolder}"
         :sourceMaps t
         :protocol "inspector"
         :console "integratedTerminal"))

  ;; Configuration pour attacher à un process Node.js existant
  (dap-register-debug-template
   "Node::Attach to Process"
   (list :type "node"
         :request "attach"
         :name "Attach to Process"
         :port 9229
         :address "localhost"
         :sourceMaps t
         :protocol "inspector"))

  ;; Configuration pour attacher à NestJS déjà lancé en mode debug
  ;; USAGE: Lance "npm run start:debug" dans un terminal, puis SPC m d d -> NestJS::Attach
  (dap-register-debug-template
   "NestJS::Attach"
   (list :type "node"
         :request "attach"
         :name "Attach to NestJS"
         :port 9229
         :address "localhost"
         :sourceMaps t
         :protocol "inspector"
         :restart t
         :skipFiles (list "<node_internals>/**")
         :program "__ignored"))  ;; Évite de demander un fichier à lancer

  ;; Activer le support des fichiers launch.json de VSCode
  (setq dap-auto-configure-mode t)

;; Raccourcis clavier pour DAP (SANS touches de fonction)
;; Tout est accessible via SPC m d (local leader + d pour debug)
(map! :map typescript-ts-mode-map
      :localleader
      (:prefix ("d" . "debug")
       :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle
       :desc "Debug test (current file)" "d" #'dap-debug
       :desc "Debug last configuration" "l" #'dap-debug-last
       :desc "Debug recent" "r" #'dap-debug-recent
       :desc "Step over (next)" "n" #'dap-next
       :desc "Step into" "i" #'dap-step-in
       :desc "Step out" "o" #'dap-step-out
       :desc "Continue" "c" #'dap-continue
       :desc "Restart" "R" #'dap-debug-restart
       :desc "Disconnect" "q" #'dap-disconnect
       :desc "Delete breakpoint" "B" #'dap-breakpoint-delete
       :desc "Delete all breakpoints" "D" #'dap-breakpoint-delete-all
       :desc "Eval at point" "e" #'dap-eval-thing-at-point
       :desc "Eval region" "E" #'dap-eval-region
       :desc "Show UI panels" "u" #'dap-ui-show-many-windows
       :desc "Hide UI panels" "U" #'dap-ui-hide-many-windows
       :desc "REPL" "'" #'dap-ui-repl))

;; Mêmes raccourcis pour les autres modes TypeScript/JavaScript
(map! :map tsx-ts-mode-map
      :localleader
      (:prefix ("d" . "debug")
       :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle
       :desc "Debug test (current file)" "d" #'dap-debug
       :desc "Debug last configuration" "l" #'dap-debug-last
       :desc "Debug recent" "r" #'dap-debug-recent
       :desc "Step over (next)" "n" #'dap-next
       :desc "Step into" "i" #'dap-step-in
       :desc "Step out" "o" #'dap-step-out
       :desc "Continue" "c" #'dap-continue
       :desc "Restart" "R" #'dap-debug-restart
       :desc "Disconnect" "q" #'dap-disconnect
       :desc "Delete breakpoint" "B" #'dap-breakpoint-delete
       :desc "Delete all breakpoints" "D" #'dap-breakpoint-delete-all
       :desc "Eval at point" "e" #'dap-eval-thing-at-point
       :desc "Eval region" "E" #'dap-eval-region
       :desc "Show UI panels" "u" #'dap-ui-show-many-windows
       :desc "Hide UI panels" "U" #'dap-ui-hide-many-windows
       :desc "REPL" "'" #'dap-ui-repl))

(map! :map js-ts-mode-map
      :localleader
      (:prefix ("d" . "debug")
       :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle
       :desc "Debug test (current file)" "d" #'dap-debug
       :desc "Debug last configuration" "l" #'dap-debug-last
       :desc "Debug recent" "r" #'dap-debug-recent
       :desc "Step over (next)" "n" #'dap-next
       :desc "Step into" "i" #'dap-step-in
       :desc "Step out" "o" #'dap-step-out
       :desc "Continue" "c" #'dap-continue
       :desc "Restart" "R" #'dap-debug-restart
       :desc "Disconnect" "q" #'dap-disconnect
       :desc "Delete breakpoint" "B" #'dap-breakpoint-delete
       :desc "Delete all breakpoints" "D" #'dap-breakpoint-delete-all
       :desc "Eval at point" "e" #'dap-eval-thing-at-point
       :desc "Eval region" "E" #'dap-eval-region
       :desc "Show UI panels" "u" #'dap-ui-show-many-windows
       :desc "Hide UI panels" "U" #'dap-ui-hide-many-windows
       :desc "REPL" "'" #'dap-ui-repl))

;; Auto-load DAP pour les modes TypeScript/JavaScript
(add-hook 'typescript-ts-mode-hook #'dap-mode)
(add-hook 'tsx-ts-mode-hook #'dap-mode)
(add-hook 'js-ts-mode-hook #'dap-mode)
)  ;; Fin du bloc (after! dap-mode)
