;;; daily-planner.el --- Daily planning dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Your Name <your@email.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/USERNAME/daily-planner

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A dashboard for daily planning that aggregates:
;; - GitHub PRs and review requests
;; - Linear assigned tickets and notifications
;; - AI/Aviation news from Hacker News
;;
;; Optional dependency: nerd-icons for beautiful icons

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

;; Optional nerd-icons support
(declare-function nerd-icons-octicon "nerd-icons")
(declare-function nerd-icons-faicon "nerd-icons")
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function nerd-icons-codicon "nerd-icons")

;; Optional evil support
(declare-function evil-set-initial-state "evil-core")

;;; Custom Variables

(defgroup daily-planner nil
  "Daily planning dashboard."
  :group 'tools
  :prefix "daily-planner-")

;; GitHub Configuration
(defcustom daily-planner-github-token nil
  "GitHub personal access token.
Can also be set via GITHUB_TOKEN environment variable."
  :type '(choice (const nil) string)
  :group 'daily-planner)

(defcustom daily-planner-github-username nil
  "GitHub username for fetching PRs."
  :type '(choice (const nil) string)
  :group 'daily-planner)

(defcustom daily-planner-github-use-cli nil
  "If non-nil, use `gh` CLI instead of API."
  :type 'boolean
  :group 'daily-planner)

(defcustom daily-planner-github-repo-filter nil
  "List of repository name substrings to filter GitHub results.
When non-nil, only PRs from repositories matching any of these
substrings will be shown. Matching is case-insensitive.
Example: \\='(\"dev-setup\" \"my-project\")"
  :type '(choice (const nil) (repeat string))
  :group 'daily-planner)

;; Linear Configuration
(defcustom daily-planner-linear-api-key nil
  "Linear API key.
Can also be set via LINEAR_API_KEY environment variable."
  :type '(choice (const nil) string)
  :group 'daily-planner)

(defcustom daily-planner-linear-user-id nil
  "Linear user ID for filtering assigned issues."
  :type '(choice (const nil) string)
  :group 'daily-planner)

;; Hacker News Configuration
(defcustom daily-planner-hn-keywords
  '("OpenAI" "Anthropic" "Claude" "GPT" "LLM" "AI"
    "Airbus" "Boeing" "Aviation" "Aircraft" "MRO")
  "Keywords to filter Hacker News stories.
Matching is case-insensitive."
  :type '(repeat string)
  :group 'daily-planner)

;; Display Configuration
(defcustom daily-planner-max-items-per-section 10
  "Maximum number of items to display per section."
  :type 'integer
  :group 'daily-planner)

(defcustom daily-planner-auto-refresh-interval nil
  "Auto-refresh interval in seconds, or nil to disable."
  :type '(choice (const nil) integer)
  :group 'daily-planner)

(defcustom daily-planner-buffer-name "*Daily Planner*"
  "Name of the daily planner buffer."
  :type 'string
  :group 'daily-planner)

(defcustom daily-planner-use-nerd-icons t
  "If non-nil, use nerd-icons for visual elements.
Requires the `nerd-icons' package to be installed."
  :type 'boolean
  :group 'daily-planner)

(defcustom daily-planner-separator-char ?─
  "Character used for section separators."
  :type 'character
  :group 'daily-planner)

(defcustom daily-planner-separator-width 60
  "Width of section separators."
  :type 'integer
  :group 'daily-planner)

;;; Faces

(defface daily-planner-title-face
  '((t :inherit font-lock-keyword-face :height 1.5 :weight bold))
  "Face for the main title."
  :group 'daily-planner)

(defface daily-planner-date-face
  '((t :inherit font-lock-comment-face :height 1.1))
  "Face for the date display."
  :group 'daily-planner)

(defface daily-planner-section-header
  '((t :inherit font-lock-keyword-face :height 1.2 :weight bold))
  "Face for section headers."
  :group 'daily-planner)

(defface daily-planner-subsection-header
  '((t :inherit font-lock-type-face :weight semi-bold))
  "Face for subsection headers."
  :group 'daily-planner)

(defface daily-planner-item-title
  '((t :inherit font-lock-function-name-face))
  "Face for item titles."
  :group 'daily-planner)

(defface daily-planner-item-meta
  '((t :inherit font-lock-comment-face :height 0.9))
  "Face for item metadata."
  :group 'daily-planner)

(defface daily-planner-urgent
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for urgent items."
  :group 'daily-planner)

(defface daily-planner-separator-face
  '((t :inherit font-lock-comment-face))
  "Face for separators."
  :group 'daily-planner)

(defface daily-planner-icon-face
  '((t :inherit font-lock-constant-face))
  "Face for icons."
  :group 'daily-planner)

(defface daily-planner-shortcut-face
  '((t :inherit font-lock-keyword-face :weight normal))
  "Face for keyboard shortcuts."
  :group 'daily-planner)

(defface daily-planner-search-highlight
  '((t :inherit highlight :weight bold))
  "Face for search match highlights."
  :group 'daily-planner)

;;; Internal Variables

(defvar daily-planner--refresh-timer nil
  "Timer for auto-refresh.")

(defvar daily-planner--data nil
  "Cached dashboard data.")

(defvar daily-planner--search-overlays nil
  "List of overlays for search highlights.")

(defvar daily-planner--search-matches nil
  "List of match positions from last search.")

(defvar daily-planner--search-index 0
  "Current index in search matches.")

;;; Mode Definition

(defvar daily-planner-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'daily-planner-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'daily-planner-open-item-at-point)
    (define-key map (kbd "<return>") #'daily-planner-open-item-at-point)
    (define-key map (kbd "o") #'daily-planner-open-item-at-point)
    (define-key map (kbd "n") #'daily-planner-next-item)
    (define-key map (kbd "p") #'daily-planner-prev-item)
    (define-key map (kbd "j") #'daily-planner-next-item)
    (define-key map (kbd "k") #'daily-planner-prev-item)
    (define-key map (kbd "/") #'daily-planner-search)
    (define-key map (kbd "C-s") #'daily-planner-search)
    map)
  "Keymap for `daily-planner-mode'.")

(define-derived-mode daily-planner-mode special-mode "DailyPlanner"
  "Major mode for the daily planning dashboard.

\\{daily-planner-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local cursor-type nil)
  ;; Evil mode compatibility
  (when (bound-and-true-p evil-mode)
    (evil-set-initial-state 'daily-planner-mode 'emacs))
  (when daily-planner-auto-refresh-interval
    (daily-planner--start-auto-refresh)))

;;; Utility Functions

(defun daily-planner--get-github-token ()
  "Get GitHub token from variable or environment."
  (or daily-planner-github-token
      (getenv "GITHUB_TOKEN")))

(defun daily-planner--get-linear-key ()
  "Get Linear API key from variable or environment."
  (or daily-planner-linear-api-key
      (getenv "LINEAR_API_KEY")))

(defun daily-planner--format-relative-time (iso-time)
  "Format ISO-TIME as relative time string."
  (let* ((time (date-to-time iso-time))
         (diff (float-time (time-subtract (current-time) time)))
         (hours (/ diff 3600))
         (days (/ hours 24)))
    (cond
     ((< hours 1) "just now")
     ((< hours 24) (format "%dh ago" (floor hours)))
     ((< days 7) (format "%dd ago" (floor days)))
     (t (format-time-string "%b %d" time)))))

(defun daily-planner--url-retrieve-sync (url &optional headers)
  "Synchronously fetch URL with optional HEADERS.
Returns parsed JSON or nil on error."
  (let ((url-request-extra-headers headers)
        (url-request-method "GET"))
    (condition-case nil
        (with-current-buffer (url-retrieve-synchronously url t t 30)
          (goto-char (point-min))
          (re-search-forward "^$" nil t)
          (let ((json-string (buffer-substring-no-properties (point) (point-max))))
            (kill-buffer)
            (condition-case nil
                (json-read-from-string json-string)
              (error nil))))
      (error nil))))

(defun daily-planner--graphql-request (url query &optional variables token)
  "Make GraphQL request to URL with QUERY, VARIABLES, and TOKEN.
TOKEN is sent as Bearer token in Authorization header."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ,@(when token `(("Authorization" . ,(concat "Bearer " token))))))
         (body (json-encode `((query . ,query)
                              ,@(when variables `((variables . ,variables))))))
         (url-request-data body))
    (condition-case nil
        (with-current-buffer (url-retrieve-synchronously url t t 30)
          (goto-char (point-min))
          (re-search-forward "^$" nil t)
          (let ((json-string (buffer-substring-no-properties (point) (point-max))))
            (kill-buffer)
            (condition-case nil
                (json-read-from-string json-string)
              (error nil))))
      (error nil))))

(defun daily-planner--linear-graphql-request (query &optional variables)
  "Make GraphQL request to Linear API with QUERY and VARIABLES.
Linear API keys should not use Bearer prefix."
  (let* ((token (daily-planner--get-linear-key))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ,@(when token `(("Authorization" . ,token)))))
         (body (json-encode `((query . ,query)
                              ,@(when variables `((variables . ,variables))))))
         (url-request-data body))
    (condition-case nil
        (with-current-buffer (url-retrieve-synchronously "https://api.linear.app/graphql" t t 30)
          (goto-char (point-min))
          (re-search-forward "^$" nil t)
          (let ((json-string (buffer-substring-no-properties (point) (point-max))))
            (kill-buffer)
            (condition-case nil
                (json-read-from-string json-string)
              (error nil))))
      (error nil))))

;;; Icons

(defun daily-planner--nerd-icons-available-p ()
  "Check if nerd-icons is available."
  (and daily-planner-use-nerd-icons
       (require 'nerd-icons nil t)))

(defun daily-planner--icon (icon-name &optional fallback)
  "Get nerd-icon ICON-NAME or FALLBACK string."
  (if (daily-planner--nerd-icons-available-p)
      (propertize (pcase icon-name
                    ('github (nerd-icons-octicon "nf-oct-mark_github"))
                    ('pr (nerd-icons-octicon "nf-oct-git_pull_request"))
                    ('review (nerd-icons-octicon "nf-oct-eye"))
                    ('linear (nerd-icons-mdicon "nf-md-vector_line"))
                    ('issue (nerd-icons-octicon "nf-oct-issue_opened"))
                    ('comment (nerd-icons-octicon "nf-oct-comment"))
                    ('hn (nerd-icons-faicon "nf-fa-hacker_news"))
                    ('news (nerd-icons-octicon "nf-oct-flame"))
                    ('bullet (nerd-icons-codicon "nf-cod-circle_filled"))
                    ('urgent (nerd-icons-codicon "nf-cod-warning"))
                    ('clock (nerd-icons-octicon "nf-oct-clock"))
                    ('calendar (nerd-icons-octicon "nf-oct-calendar"))
                    (_ (nerd-icons-codicon "nf-cod-circle_small_filled")))
                  'face 'daily-planner-icon-face)
    (or fallback "")))

(defun daily-planner--separator ()
  "Return a separator line."
  (propertize (make-string daily-planner-separator-width daily-planner-separator-char)
              'face 'daily-planner-separator-face))

;;; GitHub Module

(defun daily-planner-github--matches-repo-filter-p (item)
  "Check if ITEM's repository matches the configured filter.
Returns t if no filter is set or if repo matches any filter substring."
  (if (null daily-planner-github-repo-filter)
      t
    (let* ((repo (or (alist-get 'nameWithOwner (alist-get 'repository item))
                     (alist-get 'name (alist-get 'repository item))
                     ""))
           (repo-lower (downcase repo)))
      (seq-some (lambda (filter)
                  (string-match-p (regexp-quote (downcase filter)) repo-lower))
                daily-planner-github-repo-filter))))

(defun daily-planner-github--filter-by-repo (items)
  "Filter ITEMS by repository if filter is configured."
  (if (null daily-planner-github-repo-filter)
      items
    (seq-filter #'daily-planner-github--matches-repo-filter-p
                (seq-into items 'list))))

(defun daily-planner-github--fetch-with-cli ()
  "Fetch GitHub data using gh CLI.
Uses `gh search prs` to find PRs across all repositories."
  (let* ((prs-json (shell-command-to-string
                    "gh search prs --author @me --state open --json title,url,repository,createdAt,state --limit 20 2>/dev/null"))
         (reviews-json (shell-command-to-string
                        "gh search prs --review-requested @me --state open --json title,url,repository,createdAt,state --limit 20 2>/dev/null"))
         (prs (condition-case nil (json-read-from-string prs-json) (error nil)))
         (reviews (condition-case nil (json-read-from-string reviews-json) (error nil))))
    (list
     (cons 'prs (daily-planner-github--filter-by-repo prs))
     (cons 'reviews (daily-planner-github--filter-by-repo reviews)))))

(defun daily-planner-github--fetch-with-api ()
  "Fetch GitHub data using GraphQL API."
  (let* ((token (daily-planner--get-github-token))
         (username daily-planner-github-username)
         (query (format "
query {
  prs: search(query: \"is:pr is:open author:%s\", type: ISSUE, first: 20) {
    nodes {
      ... on PullRequest {
        title
        url
        repository { nameWithOwner }
        createdAt
        state
      }
    }
  }
  reviews: search(query: \"is:pr is:open review-requested:%s\", type: ISSUE, first: 20) {
    nodes {
      ... on PullRequest {
        title
        url
        repository { nameWithOwner }
        createdAt
        state
      }
    }
  }
}" username username))
         (response (daily-planner--graphql-request
                    "https://api.github.com/graphql"
                    query nil token)))
    (when response
      (let ((data (alist-get 'data response)))
        (list
         (cons 'prs (daily-planner-github--filter-by-repo
                     (alist-get 'nodes (alist-get 'prs data))))
         (cons 'reviews (daily-planner-github--filter-by-repo
                         (alist-get 'nodes (alist-get 'reviews data)))))))))

(defun daily-planner-github--fetch ()
  "Fetch GitHub PRs and review requests."
  (if daily-planner-github-use-cli
      (daily-planner-github--fetch-with-cli)
    (daily-planner-github--fetch-with-api)))

(defun daily-planner-github--format-item (item)
  "Format a GitHub ITEM for display."
  (let* ((title (or (alist-get 'title item) "No title"))
         (url (alist-get 'url item))
         (repo (or (alist-get 'nameWithOwner (alist-get 'repository item))
                   (alist-get 'name (alist-get 'repository item))
                   "unknown"))
         (created (alist-get 'createdAt item))
         (time-str (if created (daily-planner--format-relative-time created) ""))
         (bullet (daily-planner--icon 'bullet "  *")))
    (propertize
     (concat
      (format "   %s " bullet)
      (propertize title 'face 'daily-planner-item-title)
      "\n"
      (propertize (format "       %s  %s" repo time-str) 'face 'daily-planner-item-meta)
      "\n")
     'daily-planner-url url)))

(defun daily-planner-github--format-section (data)
  "Format GitHub section from DATA."
  (let ((prs (alist-get 'prs data))
        (reviews (alist-get 'reviews data))
        (gh-icon (daily-planner--icon 'github ""))
        (pr-icon (daily-planner--icon 'pr ""))
        (review-icon (daily-planner--icon 'review "")))
    (concat
     (format "%s %s\n\n"
             gh-icon
             (propertize "GitHub" 'face 'daily-planner-section-header))
     (if (or (and prs (> (length prs) 0))
             (and reviews (> (length reviews) 0)))
         (concat
          (when (and prs (> (length prs) 0))
            (concat
             (format "  %s %s\n\n"
                     pr-icon
                     (propertize "My Open PRs" 'face 'daily-planner-subsection-header))
             (mapconcat #'daily-planner-github--format-item
                        (seq-into (seq-take prs daily-planner-max-items-per-section) 'list) "")
             "\n"))
          (when (and reviews (> (length reviews) 0))
            (concat
             (format "  %s %s\n\n"
                     review-icon
                     (propertize "Review Requested" 'face 'daily-planner-subsection-header))
             (mapconcat #'daily-planner-github--format-item
                        (seq-into (seq-take reviews daily-planner-max-items-per-section) 'list) ""))))
       (propertize "   No open PRs or review requests\n" 'face 'daily-planner-item-meta))
     "\n")))

;;; Linear Module

(defun daily-planner-linear--fetch ()
  "Fetch Linear assigned issues and notifications."
  (let* ((query "
query {
  viewer {
    assignedIssues(first: 20, filter: { state: { type: { nin: [\"completed\", \"canceled\"] } } }) {
      nodes {
        title
        url
        identifier
        priority
        state { name }
        project { name }
        createdAt
      }
    }
  }
  notifications(first: 20) {
    nodes {
      type
      createdAt
      ... on IssueNotification {
        issue {
          title
          url
          identifier
        }
        comment {
          body
          user { name }
        }
      }
    }
  }
}")
         (response (daily-planner--linear-graphql-request query)))
    (when response
      (let ((data (alist-get 'data response)))
        (list
         (cons 'issues (alist-get 'nodes (alist-get 'assignedIssues (alist-get 'viewer data))))
         (cons 'notifications (alist-get 'nodes (alist-get 'notifications data))))))))

(defun daily-planner-linear--priority-label (priority)
  "Convert PRIORITY number to label."
  (pcase priority
    (0 "No priority")
    (1 "Urgent")
    (2 "High")
    (3 "Medium")
    (4 "Low")
    (_ "Unknown")))

(defun daily-planner-linear--format-issue (issue)
  "Format a Linear ISSUE for display."
  (let* ((title (alist-get 'title issue))
         (url (alist-get 'url issue))
         (identifier (alist-get 'identifier issue))
         (priority (alist-get 'priority issue))
         (state (alist-get 'name (alist-get 'state issue)))
         (project (alist-get 'name (alist-get 'project issue)))
         (urgent (and priority (= priority 1)))
         (bullet (if urgent
                     (daily-planner--icon 'urgent "!")
                   (daily-planner--icon 'bullet "*"))))
    (propertize
     (concat
      (format "   %s " bullet)
      (propertize (format "[%s] " identifier) 'face 'daily-planner-item-meta)
      (propertize title 'face (if urgent 'daily-planner-urgent 'daily-planner-item-title))
      "\n"
      (propertize (format "       %s  %s%s"
                          (or state "Unknown")
                          (daily-planner-linear--priority-label priority)
                          (if project (concat "  " project) ""))
                  'face 'daily-planner-item-meta)
      "\n")
     'daily-planner-url url)))

(defun daily-planner-linear--format-notification (notification)
  "Format a Linear NOTIFICATION for display."
  (let* ((type (alist-get 'type notification))
         (issue (alist-get 'issue notification))
         (comment (alist-get 'comment notification))
         (title (alist-get 'title issue))
         (url (alist-get 'url issue))
         (identifier (alist-get 'identifier issue))
         (commenter (alist-get 'name (alist-get 'user comment)))
         (bullet (daily-planner--icon 'bullet "*")))
    (when (and issue (string-match-p "comment\\|mention" (downcase (or type ""))))
      (propertize
       (concat
        (format "   %s " bullet)
        (propertize (format "[%s] " identifier) 'face 'daily-planner-item-meta)
        (propertize title 'face 'daily-planner-item-title)
        "\n"
        (propertize (format "       %s commented" (or commenter "Someone"))
                    'face 'daily-planner-item-meta)
        "\n")
       'daily-planner-url url))))

(defun daily-planner-linear--format-section (data)
  "Format Linear section from DATA."
  (let ((issues (alist-get 'issues data))
        (notifications (alist-get 'notifications data))
        (linear-icon (daily-planner--icon 'linear ""))
        (issue-icon (daily-planner--icon 'issue ""))
        (comment-icon (daily-planner--icon 'comment "")))
    (concat
     (format "%s %s\n\n"
             linear-icon
             (propertize "Linear" 'face 'daily-planner-section-header))
     (if (or (and issues (> (length issues) 0))
             (and notifications (> (length notifications) 0)))
         (concat
          (when (and issues (> (length issues) 0))
            (concat
             (format "  %s %s\n\n"
                     issue-icon
                     (propertize "Assigned Issues" 'face 'daily-planner-subsection-header))
             (mapconcat #'daily-planner-linear--format-issue
                        (seq-into (seq-take issues daily-planner-max-items-per-section) 'list) "")
             "\n"))
          (let ((filtered-notifs (seq-filter #'identity
                                             (mapcar #'daily-planner-linear--format-notification
                                                     (seq-into (seq-take notifications daily-planner-max-items-per-section) 'list)))))
            (when filtered-notifs
              (concat
               (format "  %s %s\n\n"
                       comment-icon
                       (propertize "Recent Mentions" 'face 'daily-planner-subsection-header))
               (apply #'concat filtered-notifs)))))
       (propertize "   No assigned issues or mentions\n" 'face 'daily-planner-item-meta))
     "\n")))

;;; Hacker News Module

(defconst daily-planner-hn--api-base "https://hacker-news.firebaseio.com/v0"
  "Hacker News API base URL.")

(defun daily-planner-hn--fetch-top-stories ()
  "Fetch top story IDs from HN."
  (daily-planner--url-retrieve-sync
   (concat daily-planner-hn--api-base "/topstories.json")))

(defun daily-planner-hn--fetch-item (id)
  "Fetch HN item by ID."
  (daily-planner--url-retrieve-sync
   (format "%s/item/%s.json" daily-planner-hn--api-base id)))

(defun daily-planner-hn--matches-keywords-p (title)
  "Check if TITLE matches any configured keywords."
  (let ((title-lower (downcase title)))
    (seq-some (lambda (kw)
                (string-match-p (regexp-quote (downcase kw)) title-lower))
              daily-planner-hn-keywords)))

(defun daily-planner-hn--is-today-p (timestamp)
  "Check if TIMESTAMP is from today."
  (let* ((item-time (seconds-to-time timestamp))
         (today (decode-time))
         (item-decoded (decode-time item-time)))
    (and (= (nth 3 today) (nth 3 item-decoded))
         (= (nth 4 today) (nth 4 item-decoded))
         (= (nth 5 today) (nth 5 item-decoded)))))

(defun daily-planner-hn--fetch ()
  "Fetch and filter Hacker News stories."
  (let* ((story-ids (daily-planner-hn--fetch-top-stories))
         (stories nil)
         (max-check 100))
    (when story-ids
      (catch 'done
        (dolist (id (seq-into (seq-take story-ids max-check) 'list))
          (let ((story (daily-planner-hn--fetch-item id)))
            (when story
              (let ((title (alist-get 'title story))
                    (time (alist-get 'time story)))
                (when (and title
                           (daily-planner-hn--matches-keywords-p title)
                           (or (null time) (daily-planner-hn--is-today-p time)))
                  (push story stories)
                  (when (>= (length stories) daily-planner-max-items-per-section)
                    (throw 'done nil)))))))))
    (list (cons 'stories (nreverse stories)))))

(defun daily-planner-hn--format-story (story)
  "Format a HN STORY for display."
  (let* ((title (alist-get 'title story))
         (url (or (alist-get 'url story)
                  (format "https://news.ycombinator.com/item?id=%s" (alist-get 'id story))))
         (score (alist-get 'score story))
         (comments (alist-get 'descendants story))
         (bullet (daily-planner--icon 'bullet "*")))
    (propertize
     (concat
      (format "   %s " bullet)
      (propertize title 'face 'daily-planner-item-title)
      "\n"
      (propertize (format "       %s points  %s comments"
                          (or score 0)
                          (or comments 0))
                  'face 'daily-planner-item-meta)
      "\n")
     'daily-planner-url url)))

(defun daily-planner-hn--format-section (data)
  "Format Hacker News section from DATA."
  (let ((stories (alist-get 'stories data))
        (hn-icon (daily-planner--icon 'hn ""))
        (news-icon (daily-planner--icon 'news "")))
    (concat
     (format "%s %s\n\n"
             hn-icon
             (propertize "Hacker News" 'face 'daily-planner-section-header))
     (format "  %s %s\n\n"
             news-icon
             (propertize "Matching Stories" 'face 'daily-planner-subsection-header))
     (if (and stories (> (length stories) 0))
         (mapconcat #'daily-planner-hn--format-story
                    (seq-into stories 'list) "")
       (propertize "   No matching stories today\n" 'face 'daily-planner-item-meta))
     "\n")))

;;; Dashboard Rendering

(defun daily-planner--render ()
  "Render the dashboard buffer with current data."
  (let ((inhibit-read-only t)
        (github-data (alist-get 'github daily-planner--data))
        (linear-data (alist-get 'linear daily-planner--data))
        (hn-data (alist-get 'hn daily-planner--data))
        (calendar-icon (daily-planner--icon 'calendar "")))
    (erase-buffer)
    ;; Header
    (insert "\n")
    (insert (propertize "  Daily Planner" 'face 'daily-planner-title-face))
    (insert "\n")
    (insert (format "  %s %s"
                    calendar-icon
                    (propertize (format-time-string "%A, %B %d, %Y") 'face 'daily-planner-date-face)))
    (insert "\n\n")
    (insert "  ")
    (insert (daily-planner--separator))
    (insert "\n\n")
    ;; GitHub Section
    (when github-data
      (insert (daily-planner-github--format-section github-data)))
    (insert "  ")
    (insert (daily-planner--separator))
    (insert "\n\n")
    ;; Linear Section
    (when linear-data
      (insert (daily-planner-linear--format-section linear-data)))
    (insert "  ")
    (insert (daily-planner--separator))
    (insert "\n\n")
    ;; Hacker News Section
    (when hn-data
      (insert (daily-planner-hn--format-section hn-data)))
    ;; Footer
    (insert "\n")
    (insert "  ")
    (insert (daily-planner--separator))
    (insert "\n\n")
    (insert (propertize "  Keys: " 'face 'daily-planner-item-meta))
    (insert (propertize "/" 'face 'daily-planner-shortcut-face))
    (insert (propertize " search  " 'face 'daily-planner-item-meta))
    (insert (propertize "g" 'face 'daily-planner-shortcut-face))
    (insert (propertize " refresh  " 'face 'daily-planner-item-meta))
    (insert (propertize "RET/o" 'face 'daily-planner-shortcut-face))
    (insert (propertize " open  " 'face 'daily-planner-item-meta))
    (insert (propertize "j/k" 'face 'daily-planner-shortcut-face))
    (insert (propertize " navigate  " 'face 'daily-planner-item-meta))
    (insert (propertize "q" 'face 'daily-planner-shortcut-face))
    (insert (propertize " quit" 'face 'daily-planner-item-meta))
    (insert "\n")
    (goto-char (point-min))))

;;; Interactive Commands

(defun daily-planner-refresh ()
  "Refresh all dashboard data."
  (interactive)
  (message "Refreshing daily planner...")
  (setq daily-planner--data
        (list
         (cons 'github (condition-case err
                           (daily-planner-github--fetch)
                         (error (message "GitHub fetch error: %s" err) nil)))
         (cons 'linear (condition-case err
                           (daily-planner-linear--fetch)
                         (error (message "Linear fetch error: %s" err) nil)))
         (cons 'hn (condition-case err
                       (daily-planner-hn--fetch)
                     (error (message "HN fetch error: %s" err) nil)))))
  (when (get-buffer daily-planner-buffer-name)
    (with-current-buffer daily-planner-buffer-name
      (daily-planner--render)))
  (message "Daily planner refreshed"))

(defun daily-planner-open-item-at-point ()
  "Open the URL of the item at point."
  (interactive)
  (let ((url (get-text-property (point) 'daily-planner-url)))
    (if url
        (browse-url url)
      (message "No item at point"))))

(defun daily-planner-next-item ()
  "Move to the next item."
  (interactive)
  (let ((pos (next-single-property-change (point) 'daily-planner-url)))
    (when pos (goto-char pos))))

(defun daily-planner-prev-item ()
  "Move to the previous item."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'daily-planner-url)))
    (when pos (goto-char pos))))

;;; Search

(defun daily-planner--fuzzy-regex (query)
  "Convert QUERY into a fuzzy matching regex.
Each character in QUERY can have any characters between them."
  (let ((chars (string-to-list query)))
    (mapconcat (lambda (c)
                 (regexp-quote (char-to-string c)))
               chars
               ".*?")))

(defun daily-planner--clear-search-highlights ()
  "Remove all search highlight overlays."
  (mapc #'delete-overlay daily-planner--search-overlays)
  (setq daily-planner--search-overlays nil)
  (setq daily-planner--search-matches nil)
  (setq daily-planner--search-index 0))

(defun daily-planner--highlight-matches (regex)
  "Highlight all matches for REGEX in the buffer.
Returns list of match positions."
  (daily-planner--clear-search-highlights)
  (let ((matches nil)
        (case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov 'face 'daily-planner-search-highlight)
          (overlay-put ov 'daily-planner-search t)
          (push ov daily-planner--search-overlays)
          (push (match-beginning 0) matches))))
    (setq daily-planner--search-matches (nreverse matches))
    daily-planner--search-matches))

(defun daily-planner--jump-to-match (index)
  "Jump to match at INDEX."
  (when (and daily-planner--search-matches
             (>= index 0)
             (< index (length daily-planner--search-matches)))
    (setq daily-planner--search-index index)
    (goto-char (nth index daily-planner--search-matches))
    (message "Match %d of %d"
             (1+ index)
             (length daily-planner--search-matches))))

(defun daily-planner-search-next ()
  "Jump to next search match."
  (interactive)
  (when daily-planner--search-matches
    (let ((next-index (mod (1+ daily-planner--search-index)
                           (length daily-planner--search-matches))))
      (daily-planner--jump-to-match next-index))))

(defun daily-planner-search-prev ()
  "Jump to previous search match."
  (interactive)
  (when daily-planner--search-matches
    (let ((prev-index (mod (1- daily-planner--search-index)
                           (length daily-planner--search-matches))))
      (daily-planner--jump-to-match prev-index))))

(defun daily-planner-search-clear ()
  "Clear search highlights."
  (interactive)
  (daily-planner--clear-search-highlights)
  (message "Search cleared"))

(defvar daily-planner-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'daily-planner-search-clear)
    (define-key map (kbd "<escape>") #'daily-planner-search-clear)
    (define-key map (kbd "n") #'daily-planner-search-next)
    (define-key map (kbd "N") #'daily-planner-search-prev)
    map)
  "Keymap active after a search.")

(defun daily-planner-search ()
  "Start fuzzy search in the daily planner buffer.
Type search query, then press RET to jump to first match.
After searching: n for next match, N for previous, C-g to clear."
  (interactive)
  (let* ((query (read-string "Search: "))
         (regex (daily-planner--fuzzy-regex query))
         (matches (daily-planner--highlight-matches regex)))
    (if matches
        (progn
          (daily-planner--jump-to-match 0)
          (set-transient-map daily-planner-search-map t
                             #'daily-planner--clear-search-highlights)
          (message "Match 1 of %d (n/N to navigate, C-g to clear)"
                   (length matches)))
      (message "No matches found for '%s'" query))))

;;;###autoload
(defun daily-planner-open ()
  "Open the daily planner dashboard."
  (interactive)
  (let ((buf (get-buffer-create daily-planner-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'daily-planner-mode)
        (daily-planner-mode))
      (daily-planner-refresh))
    (switch-to-buffer buf)))

;;; Auto-refresh

(defun daily-planner--start-auto-refresh ()
  "Start auto-refresh timer."
  (when daily-planner--refresh-timer
    (cancel-timer daily-planner--refresh-timer))
  (when daily-planner-auto-refresh-interval
    (setq daily-planner--refresh-timer
          (run-with-timer daily-planner-auto-refresh-interval
                          daily-planner-auto-refresh-interval
                          #'daily-planner-refresh))))

(defun daily-planner--stop-auto-refresh ()
  "Stop auto-refresh timer."
  (when daily-planner--refresh-timer
    (cancel-timer daily-planner--refresh-timer)
    (setq daily-planner--refresh-timer nil)))

;;; Evil mode support

(defun daily-planner--setup-evil ()
  "Setup Evil mode keybindings for daily-planner."
  (when (bound-and-true-p evil-mode)
    (evil-set-initial-state 'daily-planner-mode 'emacs)))

(add-hook 'daily-planner-mode-hook #'daily-planner--setup-evil)

(provide 'daily-planner)
;;; daily-planner.el ends here
