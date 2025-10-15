# Ma Configuration Doom Emacs

Configuration personnalisée de Doom Emacs pour le développement TypeScript/NestJS.

## 📋 Prérequis système

Avant de synchroniser cette configuration, installez les dépendances suivantes :

### 1. Emacs-lsp-booster (OBLIGATOIRE)
Améliore considérablement les performances de LSP.

**Installation sur Arch Linux :**
```bash
yay -S emacs-lsp-booster
# ou
paru -S emacs-lsp-booster
```

**Installation manuelle (autres distributions) :**
```bash
cargo install --git https://github.com/blahgeek/emacs-lsp-booster
```

### 2. Tree-sitter grammars
Pour la coloration syntaxique améliorée.

**Après avoir installé Doom, dans Emacs :**
```
M-x +treesit-install-all-languages
```

### 3. Node.js et npm/yarn
Pour le développement TypeScript/NestJS.

```bash
# Arch Linux
sudo pacman -S nodejs npm

# Ou utilisez nvm, mise, etc.
```

### 4. Fonts (optionnel mais recommandé)
Pour les icônes et ligatures.

```bash
# Arch Linux
sudo pacman -S ttf-fira-code nerd-fonts-complete
```

## 🚀 Installation sur un nouveau PC

### 1. Installer Doom Emacs
```bash
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

### 2. Cloner cette configuration
```bash
# Supprimer la config par défaut
rm -rf ~/.config/doom

# Cloner votre configuration (remplacez par votre URL GitHub)
git clone git@github.com:VOTRE_USERNAME/doom-config.git ~/.config/doom
```

### 3. Installer les prérequis
Suivez les instructions de la section "Prérequis système" ci-dessus.

### 4. Synchroniser Doom
```bash
~/.config/emacs/bin/doom sync
```

### 5. Redémarrer Emacs
```bash
~/.config/emacs/bin/doom reload
# ou redémarrez Emacs complètement
```

## 🔧 Fonctionnalités principales

- **LSP** : Configuration optimisée avec lsp-booster pour TypeScript/JavaScript
- **Tree-sitter** : Coloration syntaxique améliorée
- **Dape** : Debugging pour Node.js/NestJS
- **Inlay hints** : Affichage des types TypeScript inline
- **Doom theme** : Thème sombre

## 📦 Packages personnalisés

Voir `packages.el` pour la liste complète.

## 🐛 Debugging NestJS

### Configuration dape
La configuration pour NestJS est déjà présente dans `config.el`.

**Pour débugger :**
1. Lancez votre serveur NestJS : `npm run start:debug`
2. Dans Emacs : `M-x dape RET nestjs RET`
3. Placez des breakpoints avec : `SPC m d b`

**Note :** Les breakpoints avec dape peuvent être problématiques. En cas de souci, utilisez `debugger;` directement dans votre code TypeScript.

## 📝 Notes

- Le fichier `custom.el` est ignoré par Git (configurations spécifiques à la machine)
- Le dossier `.local/` est ignoré (cache et données temporaires)

## 🔄 Mise à jour de Doom

```bash
~/.config/emacs/bin/doom upgrade
~/.config/emacs/bin/doom sync
```

## 📧 Support

Pour les problèmes liés à :
- **Doom Emacs** : https://github.com/doomemacs/doomemacs/issues
- **Cette config** : Créez une issue dans ce repo
