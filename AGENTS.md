# AI Agent Guidelines for daily-planner

## Project Overview

`daily-planner.el` is an Emacs package that displays a unified dashboard aggregating data from:
- **GitHub**: PRs authored by user and PRs requesting review
- **Linear**: Assigned issues and notifications with comments
- **Hacker News**: Stories matching configurable keywords

## Architecture

### File Structure

```
daily-planner/
├── daily-planner.el      # Main package (all code in single file)
├── test/
│   ├── daily-planner-test.el  # ERT test suite
│   └── test-helper.el         # Test utilities
├── scripts/
│   ├── run-tests.sh      # Run ERT tests
│   └── cli-test.sh       # Manual CLI testing
├── .github/workflows/
│   └── test.yml          # CI for Emacs 27.1, 28.2, 29.1
└── README.md
```

### Code Organization (daily-planner.el)

1. **Custom Variables** (lines ~34-117): User configuration options
2. **Faces** (lines ~119-175): Visual styling definitions
3. **Internal Variables** (lines ~177-190): Runtime state
4. **Mode Definition** (lines ~192-210): Major mode and keymap
5. **Utility Functions** (lines ~212-320): HTTP requests, icons, helpers
6. **GitHub Module** (lines ~323-453): GitHub API/CLI integration
7. **Linear Module** (lines ~455-589): Linear GraphQL API
8. **Hacker News Module** (lines ~591-678): HN Firebase API
9. **Dashboard Rendering** (lines ~680-735): Buffer rendering
10. **Interactive Commands** (lines ~737-830): User commands
11. **Search** (lines ~780-845): Fuzzy search implementation
12. **Auto-refresh** (lines ~850-870): Timer-based refresh
13. **Evil Support** (lines ~872-880): Evil mode compatibility

## Key Implementation Details

### API Integrations

**GitHub:**
- Supports both `gh` CLI (`gh search prs`) and GraphQL API
- CLI is recommended as it searches across all repos
- Optional repo filtering via `daily-planner-github-repo-filter`

**Linear:**
- Uses GraphQL API at `https://api.linear.app/graphql`
- API key goes in Authorization header WITHOUT "Bearer " prefix
- Uses inline fragments (`... on IssueNotification`) for polymorphic types

**Hacker News:**
- Uses Firebase REST API
- Fetches top stories, filters by keywords and today's date

### Evil Mode Compatibility

- Buffer uses `emacs` state (not `normal` or `insert`)
- Keybindings defined in `daily-planner-mode-map` work in emacs state
- Setup via `daily-planner--setup-evil` hook

### Text Properties

Items use `daily-planner-url` text property for the entire item block (title + metadata lines). This allows `RET` to work anywhere on an item.

### Vector vs List Handling

JSON parser returns arrays as vectors. Use `(seq-into vector 'list)` before passing to functions that expect lists (like `dolist`, `mapconcat`).

## Testing

### Run Tests

```bash
./scripts/run-tests.sh
```

### Test with Real APIs

```bash
LINEAR_API_KEY="lin_api_xxx" ./scripts/cli-test.sh
```

### CI

GitHub Actions runs tests on Emacs 27.1, 28.2, and 29.1.

## Common Tasks

### Adding a New Section

1. Create fetch function: `daily-planner-SECTION--fetch`
2. Create format functions: `daily-planner-SECTION--format-item`, `daily-planner-SECTION--format-section`
3. Add to `daily-planner-refresh` data fetching
4. Add to `daily-planner--render` buffer output

### Adding a New Icon

Add to `daily-planner--icon` function:
```elisp
('icon-name (nerd-icons-octicon "nf-oct-icon_name"))
```

### Adding a New Keybinding

1. Add to `daily-planner-mode-map`
2. Create command function
3. Update footer in `daily-planner--render`

## Code Style

- Use `daily-planner-` prefix for public, `daily-planner--` for private
- Module functions use `daily-planner-MODULE--function` pattern
- All functions should have docstrings
- Use `condition-case` for error handling in API calls
- Prefer `alist-get` for accessing JSON data

## Dependencies

- **Required**: Emacs 27.1+ (for `json-parse-string`, `url-retrieve-synchronously` improvements)
- **Optional**: `nerd-icons` for icons, `gh` CLI for GitHub
