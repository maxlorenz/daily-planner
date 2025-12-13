;;; test-helper.el --- Test utilities for daily-planner -*- lexical-binding: t; -*-

;;; Commentary:
;; Mock data and utilities for testing daily-planner.

;;; Code:

(defvar daily-planner-test--mock-github-data
  '((prs . [((title . "Add feature X")
             (url . "https://github.com/org/repo/pull/1")
             (repository . ((nameWithOwner . "org/repo")))
             (createdAt . "2024-01-15T10:00:00Z")
             (state . "OPEN"))
            ((title . "Fix bug Y")
             (url . "https://github.com/org/repo/pull/2")
             (repository . ((nameWithOwner . "org/repo")))
             (createdAt . "2024-01-14T10:00:00Z")
             (state . "OPEN"))])
    (reviews . [((title . "Review: Update docs")
                 (url . "https://github.com/org/repo/pull/3")
                 (repository . ((nameWithOwner . "org/other")))
                 (createdAt . "2024-01-15T12:00:00Z")
                 (state . "OPEN"))]))
  "Mock GitHub API response.")

(defvar daily-planner-test--mock-linear-data
  '((issues . [((title . "Implement feature")
                (url . "https://linear.app/team/issue/TEAM-123")
                (identifier . "TEAM-123")
                (priority . 2)
                (state . ((name . "In Progress")))
                (project . ((name . "Q1 Roadmap")))
                (createdAt . "2024-01-10T10:00:00Z"))
               ((title . "Urgent bug")
                (url . "https://linear.app/team/issue/TEAM-124")
                (identifier . "TEAM-124")
                (priority . 1)
                (state . ((name . "Todo")))
                (project . nil)
                (createdAt . "2024-01-15T08:00:00Z"))])
    (notifications . [((type . "issueCommentMention")
                       (createdAt . "2024-01-15T14:00:00Z")
                       (issue . ((title . "Design review")
                                 (url . "https://linear.app/team/issue/TEAM-100")
                                 (identifier . "TEAM-100")))
                       (comment . ((body . "Hey, what do you think?")
                                   (user . ((name . "Alice"))))))]))
  "Mock Linear API response.")

(defvar daily-planner-test--mock-hn-stories
  '((stories . [((id . 12345)
                 (title . "OpenAI releases new model")
                 (url . "https://example.com/openai")
                 (score . 500)
                 (descendants . 200)
                 (time . 1705320000))
                ((id . 12346)
                 (title . "Anthropic Claude update")
                 (url . "https://example.com/claude")
                 (score . 300)
                 (descendants . 100)
                 (time . 1705320000))]))
  "Mock HN stories.")

(defmacro daily-planner-test--with-mock-data (&rest body)
  "Execute BODY with mocked fetch functions."
  `(cl-letf (((symbol-function 'daily-planner-github--fetch)
              (lambda () daily-planner-test--mock-github-data))
             ((symbol-function 'daily-planner-linear--fetch)
              (lambda () daily-planner-test--mock-linear-data))
             ((symbol-function 'daily-planner-hn--fetch)
              (lambda () daily-planner-test--mock-hn-stories)))
     ,@body))

(defun daily-planner-test--buffer-contains-p (string)
  "Check if current buffer contains STRING."
  (save-excursion
    (goto-char (point-min))
    (search-forward string nil t)))

(provide 'test-helper)
;;; test-helper.el ends here
