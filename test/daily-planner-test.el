;;; daily-planner-test.el --- Tests for daily-planner -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the daily-planner package.

;;; Code:

(require 'ert)

(let ((project-root (file-name-directory (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path project-root)
  (add-to-list 'load-path (expand-file-name "test" project-root)))

(require 'daily-planner)
(require 'test-helper)

;;; Core Tests

(ert-deftest daily-planner-test-mode-defined ()
  "Test that daily-planner-mode is properly defined."
  (should (fboundp 'daily-planner-mode))
  (should (keymapp daily-planner-mode-map)))

(ert-deftest daily-planner-test-customization-groups ()
  "Test that customization variables are defined."
  (should (boundp 'daily-planner-github-token))
  (should (boundp 'daily-planner-linear-api-key))
  (should (boundp 'daily-planner-hn-keywords))
  (should (boundp 'daily-planner-max-items-per-section)))

(ert-deftest daily-planner-test-default-keywords ()
  "Test that default HN keywords are set."
  (should (member "OpenAI" daily-planner-hn-keywords))
  (should (member "Anthropic" daily-planner-hn-keywords))
  (should (member "Aviation" daily-planner-hn-keywords)))

;;; Utility Tests

(ert-deftest daily-planner-test-relative-time-hours ()
  "Test relative time formatting for hours."
  ;; Use a fixed time 2 hours ago in UTC format
  (let* ((two-hours-ago (time-subtract (current-time) (seconds-to-time 7200)))
         (time (format-time-string "%Y-%m-%dT%H:%M:%SZ" two-hours-ago t)))
    (should (string-match-p "2h ago" (daily-planner--format-relative-time time)))))

(ert-deftest daily-planner-test-relative-time-days ()
  "Test relative time formatting for days."
  ;; Use a fixed time 2 days ago in UTC format
  (let* ((two-days-ago (time-subtract (current-time) (seconds-to-time 172800)))
         (time (format-time-string "%Y-%m-%dT%H:%M:%SZ" two-days-ago t)))
    (should (string-match-p "2d ago" (daily-planner--format-relative-time time)))))

;;; GitHub Tests

(ert-deftest daily-planner-test-github-format-section ()
  "Test GitHub section formatting."
  (let ((output (daily-planner-github--format-section daily-planner-test--mock-github-data)))
    (should (string-match-p "GitHub" output))
    (should (string-match-p "My Open PRs" output))
    (should (string-match-p "Add feature X" output))
    (should (string-match-p "Review Requested" output))))

(ert-deftest daily-planner-test-github-format-empty ()
  "Test GitHub section with no data."
  (let ((output (daily-planner-github--format-section '((prs . []) (reviews . [])))))
    (should (string-match-p "No open PRs" output))))

;;; Linear Tests

(ert-deftest daily-planner-test-linear-priority-label ()
  "Test Linear priority conversion."
  (should (string= "Urgent" (daily-planner-linear--priority-label 1)))
  (should (string= "High" (daily-planner-linear--priority-label 2)))
  (should (string= "Medium" (daily-planner-linear--priority-label 3)))
  (should (string= "Low" (daily-planner-linear--priority-label 4))))

(ert-deftest daily-planner-test-linear-format-section ()
  "Test Linear section formatting."
  (let ((output (daily-planner-linear--format-section daily-planner-test--mock-linear-data)))
    (should (string-match-p "Linear" output))
    (should (string-match-p "Assigned Issues" output))
    (should (string-match-p "TEAM-123" output))
    (should (string-match-p "TEAM-124" output))))

;;; Hacker News Tests

(ert-deftest daily-planner-test-hn-keyword-match ()
  "Test HN keyword matching."
  (let ((daily-planner-hn-keywords '("OpenAI" "Claude")))
    (should (daily-planner-hn--matches-keywords-p "OpenAI releases GPT-5"))
    (should (daily-planner-hn--matches-keywords-p "Anthropic Claude update"))
    (should (daily-planner-hn--matches-keywords-p "openai is amazing"))  ; case insensitive
    (should-not (daily-planner-hn--matches-keywords-p "Random tech news"))))

(ert-deftest daily-planner-test-hn-format-section ()
  "Test HN section formatting."
  (let ((output (daily-planner-hn--format-section daily-planner-test--mock-hn-stories)))
    (should (string-match-p "Hacker News" output))
    (should (string-match-p "OpenAI releases" output))
    (should (string-match-p "500 points" output))))

;;; Integration Tests

(ert-deftest daily-planner-test-full-render ()
  "Test full dashboard rendering with mock data."
  (daily-planner-test--with-mock-data
   (let ((daily-planner-buffer-name "*daily-planner-test*"))
     (let ((buf (get-buffer-create daily-planner-buffer-name)))
       (with-current-buffer buf
         (daily-planner-mode)
         (daily-planner-refresh)
         (should (daily-planner-test--buffer-contains-p "Daily Planner"))
         (should (daily-planner-test--buffer-contains-p "GitHub"))
         (should (daily-planner-test--buffer-contains-p "Linear"))
         (should (daily-planner-test--buffer-contains-p "Hacker News")))
       (kill-buffer buf)))))

(ert-deftest daily-planner-test-open-creates-buffer ()
  "Test that daily-planner-open creates the buffer."
  (daily-planner-test--with-mock-data
   (let ((daily-planner-buffer-name "*daily-planner-test-open*"))
     (daily-planner-open)
     (should (get-buffer daily-planner-buffer-name))
     (kill-buffer daily-planner-buffer-name))))

(provide 'daily-planner-test)
;;; daily-planner-test.el ends here
