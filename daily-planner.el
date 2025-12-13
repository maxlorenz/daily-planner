;;; daily-planner.el --- Daily planning dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Your Name <your@email.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/USERNAME/daily-planner

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A dashboard for daily planning that aggregates:
;; - GitHub PRs and review requests
;; - Linear assigned tickets and notifications
;; - AI/Aviation news from Hacker News

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

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

;;; Faces

(defface daily-planner-section-header
  '((t :inherit font-lock-keyword-face :height 1.3 :weight bold))
  "Face for section headers."
  :group 'daily-planner)

(defface daily-planner-item-title
  '((t :inherit font-lock-function-name-face))
  "Face for item titles."
  :group 'daily-planner)

(defface daily-planner-item-meta
  '((t :inherit font-lock-comment-face))
  "Face for item metadata."
  :group 'daily-planner)

(defface daily-planner-urgent
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for urgent items."
  :group 'daily-planner)

;;; Internal Variables

(defvar daily-planner--refresh-timer nil
  "Timer for auto-refresh.")

(defvar daily-planner--data nil
  "Cached dashboard data.")

;;; Mode Definition

(defvar daily-planner-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'daily-planner-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'daily-planner-open-item-at-point)
    (define-key map (kbd "n") #'daily-planner-next-item)
    (define-key map (kbd "p") #'daily-planner-prev-item)
    map)
  "Keymap for `daily-planner-mode'.")

(define-derived-mode daily-planner-mode special-mode "DailyPlanner"
  "Major mode for the daily planning dashboard.

\\{daily-planner-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
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

;;; GitHub Module

(defun daily-planner-github--fetch-with-cli ()
  "Fetch GitHub data using gh CLI.
Uses `gh search prs` to find PRs across all repositories."
  (let ((prs-json (shell-command-to-string
                   "gh search prs --author @me --state open --json title,url,repository,createdAt,state --limit 20 2>/dev/null"))
        (reviews-json (shell-command-to-string
                       "gh search prs --review-requested @me --state open --json title,url,repository,createdAt,state --limit 20 2>/dev/null")))
    (list
     (cons 'prs (condition-case nil (json-read-from-string prs-json) (error nil)))
     (cons 'reviews (condition-case nil (json-read-from-string reviews-json) (error nil))))))

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
         (cons 'prs (alist-get 'nodes (alist-get 'prs data)))
         (cons 'reviews (alist-get 'nodes (alist-get 'reviews data))))))))

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
         (time-str (if created (daily-planner--format-relative-time created) "")))
    (concat
     (propertize (format "  \u2022 %s" title) 'face 'daily-planner-item-title 'daily-planner-url url)
     "\n"
     (propertize (format "    %s \u00b7 %s" repo time-str) 'face 'daily-planner-item-meta)
     "\n")))

(defun daily-planner-github--format-section (data)
  "Format GitHub section from DATA."
  (let ((prs (alist-get 'prs data))
        (reviews (alist-get 'reviews data)))
    (concat
     (propertize "GitHub" 'face 'daily-planner-section-header)
     "\n\n"
     (if (or (and prs (> (length prs) 0))
             (and reviews (> (length reviews) 0)))
         (concat
          (when (and prs (> (length prs) 0))
            (concat
             (propertize " My Open PRs" 'face 'bold) "\n"
             (mapconcat #'daily-planner-github--format-item
                        (seq-into (seq-take prs daily-planner-max-items-per-section) 'list) "")
             "\n"))
          (when (and reviews (> (length reviews) 0))
            (concat
             (propertize " Review Requested" 'face 'bold) "\n"
             (mapconcat #'daily-planner-github--format-item
                        (seq-into (seq-take reviews daily-planner-max-items-per-section) 'list) ""))))
       "  No open PRs or review requests\n")
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
         (urgent (and priority (= priority 1))))
    (concat
     (propertize (format "  \u2022 [%s] %s" identifier title)
                 'face (if urgent 'daily-planner-urgent 'daily-planner-item-title)
                 'daily-planner-url url)
     "\n"
     (propertize (format "    %s \u00b7 %s%s"
                         (or state "Unknown")
                         (daily-planner-linear--priority-label priority)
                         (if project (concat " \u00b7 " project) ""))
                 'face 'daily-planner-item-meta)
     "\n")))

(defun daily-planner-linear--format-notification (notification)
  "Format a Linear NOTIFICATION for display."
  (let* ((type (alist-get 'type notification))
         (issue (alist-get 'issue notification))
         (comment (alist-get 'comment notification))
         (title (alist-get 'title issue))
         (url (alist-get 'url issue))
         (identifier (alist-get 'identifier issue))
         (commenter (alist-get 'name (alist-get 'user comment))))
    (when (and issue (string-match-p "comment\\|mention" (downcase (or type ""))))
      (concat
       (propertize (format "  \u2022 [%s] %s" identifier title)
                   'face 'daily-planner-item-title
                   'daily-planner-url url)
       "\n"
       (propertize (format "    %s commented" (or commenter "Someone"))
                   'face 'daily-planner-item-meta)
       "\n"))))

(defun daily-planner-linear--format-section (data)
  "Format Linear section from DATA."
  (let ((issues (alist-get 'issues data))
        (notifications (alist-get 'notifications data)))
    (concat
     (propertize "Linear" 'face 'daily-planner-section-header)
     "\n\n"
     (if (or (and issues (> (length issues) 0))
             (and notifications (> (length notifications) 0)))
         (concat
          (when (and issues (> (length issues) 0))
            (concat
             (propertize " Assigned Issues" 'face 'bold) "\n"
             (mapconcat #'daily-planner-linear--format-issue
                        (seq-into (seq-take issues daily-planner-max-items-per-section) 'list) "")
             "\n"))
          (let ((filtered-notifs (seq-filter #'identity
                                             (mapcar #'daily-planner-linear--format-notification
                                                     (seq-into (seq-take notifications daily-planner-max-items-per-section) 'list)))))
            (when filtered-notifs
              (concat
               (propertize " Recent Mentions" 'face 'bold) "\n"
               (apply #'concat filtered-notifs)))))
       "  No assigned issues or mentions\n")
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
         (comments (alist-get 'descendants story)))
    (concat
     (propertize (format "  \u2022 %s" title)
                 'face 'daily-planner-item-title
                 'daily-planner-url url)
     "\n"
     (propertize (format "    %s points \u00b7 %s comments"
                         (or score 0)
                         (or comments 0))
                 'face 'daily-planner-item-meta)
     "\n")))

(defun daily-planner-hn--format-section (data)
  "Format Hacker News section from DATA."
  (let ((stories (alist-get 'stories data)))
    (concat
     (propertize "Hacker News (AI/Aviation)" 'face 'daily-planner-section-header)
     "\n\n"
     (if (and stories (> (length stories) 0))
         (mapconcat #'daily-planner-hn--format-story
                    (seq-into stories 'list) "")
       "  No matching stories today\n")
     "\n")))

;;; Dashboard Rendering

(defun daily-planner--render ()
  "Render the dashboard buffer with current data."
  (let ((inhibit-read-only t)
        (github-data (alist-get 'github daily-planner--data))
        (linear-data (alist-get 'linear daily-planner--data))
        (hn-data (alist-get 'hn daily-planner--data)))
    (erase-buffer)
    (insert (propertize "Daily Planner" 'face '(:height 1.5 :weight bold)))
    (insert "\n")
    (insert (propertize (format-time-string "%A, %B %d, %Y") 'face 'daily-planner-item-meta))
    (insert "\n\n")
    (insert (make-string 50 ?\u2500))
    (insert "\n\n")
    (when github-data
      (insert (daily-planner-github--format-section github-data)))
    (insert (make-string 50 ?\u2500))
    (insert "\n\n")
    (when linear-data
      (insert (daily-planner-linear--format-section linear-data)))
    (insert (make-string 50 ?\u2500))
    (insert "\n\n")
    (when hn-data
      (insert (daily-planner-hn--format-section hn-data)))
    (insert "\n")
    (insert (propertize "Press 'g' to refresh, 'q' to quit, RET to open item"
                        'face 'daily-planner-item-meta))
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

(provide 'daily-planner)
;;; daily-planner.el ends here
