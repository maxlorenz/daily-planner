# Daily Planner for Emacs

A beautiful dashboard for daily planning that aggregates:
- **GitHub**: Your open PRs and review requests (with optional repo filtering)
- **Linear**: Assigned tickets and comment mentions
- **Hacker News**: News filtered by customizable keywords

![Screenshot](screenshot.png)

## Features

- Unified view of GitHub PRs, Linear tickets, and Hacker News
- Optional **nerd-icons** support for beautiful icons
- **Fuzzy search** with `/` key
- **Evil mode** compatible
- Configurable repo filtering for GitHub
- Auto-refresh support
- Keyboard-driven navigation

## Installation

### Doom Emacs

In `~/.doom.d/packages.el`:

```elisp
(package! daily-planner
  :recipe (:host github :repo "maxlorenz/daily-planner"
           :files ("daily-planner.el")))
```

In `~/.doom.d/config.el`:

```elisp
(use-package! daily-planner
  :commands (daily-planner-open)
  :config
  ;; GitHub - use CLI (recommended)
  (setq daily-planner-github-use-cli t)
  ;; OR use API token:
  ;; (setq daily-planner-github-token (getenv "GITHUB_TOKEN"))
  ;; (setq daily-planner-github-username "your-username")
  
  ;; Linear
  (setq daily-planner-linear-api-key (getenv "LINEAR_API_KEY"))
  
  ;; Optional: filter GitHub to specific repos
  (setq daily-planner-github-repo-filter '("my-repo" "other-repo"))
  
  ;; Optional: customize HN keywords
  (setq daily-planner-hn-keywords
        '("OpenAI" "Anthropic" "Claude" "GPT" "LLM" "AI"
          "Airbus" "Boeing" "Aviation" "Aircraft")))
```

### Straight.el

```elisp
(straight-use-package
 '(daily-planner :host github :repo "maxlorenz/daily-planner"))
```

### use-package with vc (Emacs 29+)

```elisp
(use-package daily-planner
  :vc (:url "https://github.com/maxlorenz/daily-planner")
  :commands (daily-planner-open))
```

## Configuration

### GitHub

```elisp
;; Option 1: Use gh CLI (recommended - searches all repos)
(setq daily-planner-github-use-cli t)

;; Option 2: Use API token
(setq daily-planner-github-token "ghp_xxxx")
(setq daily-planner-github-username "your-username")

;; Optional: Filter to specific repositories
(setq daily-planner-github-repo-filter '("dev-setup" "my-project"))
```

### Linear

```elisp
(setq daily-planner-linear-api-key "lin_api_xxxx")
;; Or via environment variable: LINEAR_API_KEY
```

### Hacker News

```elisp
(setq daily-planner-hn-keywords '("AI" "LLM" "Emacs" "Lisp"))
```

### Display

```elisp
;; Max items per section (default: 10)
(setq daily-planner-max-items-per-section 10)

;; Auto-refresh interval in seconds (default: nil = disabled)
(setq daily-planner-auto-refresh-interval 300)

;; Use nerd-icons (default: t, requires nerd-icons package)
(setq daily-planner-use-nerd-icons t)

;; Separator customization
(setq daily-planner-separator-char ?─)
(setq daily-planner-separator-width 60)
```

## Usage

Open the dashboard with `M-x daily-planner-open`.

### Keybindings

| Key | Action |
|-----|--------|
| `/` | Fuzzy search |
| `g` | Refresh data |
| `RET` / `o` | Open item in browser |
| `j` / `n` | Next item |
| `k` / `p` | Previous item |
| `q` | Close dashboard |

### Search

Press `/` to start a fuzzy search:
- Type your query (e.g., `sfj` matches `[SFJ] Update report`)
- Press `RET` to jump to first match
- After searching:
  - `n` - next match
  - `N` - previous match
  - `C-g` - clear highlights

## Optional Dependencies

- **nerd-icons**: For beautiful section icons. Install with `M-x package-install RET nerd-icons RET`
- **gh CLI**: For GitHub integration without API token. Install from https://cli.github.com

## API Keys

### GitHub Token

Create at https://github.com/settings/tokens with scopes:
- `repo` (for private repos)
- `read:org` (if using org repos)

Or just use `gh` CLI which handles auth automatically.

### Linear API Key

Create at https://linear.app/settings/api

## Requirements

- Emacs 27.1+
- Optional: `gh` CLI (if using `daily-planner-github-use-cli`)
- Optional: `nerd-icons` package (for icons)

## License

MIT
