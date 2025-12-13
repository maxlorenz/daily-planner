# Daily Planner for Emacs

A dashboard for daily planning that aggregates:
- **GitHub**: Your open PRs and review requests
- **Linear**: Assigned tickets and comment mentions
- **Hacker News**: AI and aviation news filtered by keywords

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
  (setq daily-planner-github-token (getenv "GITHUB_TOKEN"))
  (setq daily-planner-github-username "your-username")
  (setq daily-planner-linear-api-key (getenv "LINEAR_API_KEY"))
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

### Required

```elisp
;; GitHub (either token or use CLI)
(setq daily-planner-github-token "ghp_xxxx")
(setq daily-planner-github-username "your-username")
;; OR use gh CLI:
(setq daily-planner-github-use-cli t)

;; Linear
(setq daily-planner-linear-api-key "lin_api_xxxx")
```

### Optional

```elisp
;; Custom HN keywords
(setq daily-planner-hn-keywords '("AI" "LLM" "Aviation"))

;; Display settings
(setq daily-planner-max-items-per-section 10)
(setq daily-planner-auto-refresh-interval 300)  ; 5 minutes
```

## Usage

- `M-x daily-planner-open` - Open the dashboard
- `g` - Refresh data
- `RET` - Open item in browser
- `n` / `p` - Navigate items
- `q` - Close dashboard

## API Keys

### GitHub Token

Create at https://github.com/settings/tokens with scopes:
- `repo` (for private repos)
- `read:org` (if using org repos)

### Linear API Key

Create at https://linear.app/settings/api

## Requirements

- Emacs 27.1+
- Optional: `gh` CLI (if using `daily-planner-github-use-cli`)

## License

MIT
