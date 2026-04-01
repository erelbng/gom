;;; gom.el --- Git-Org-Manager: Syncing Org-mode with Git forges -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric Elbing
;; Author: Eric Plaß
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: tools, git, org

;;; Commentary:
;; GOM (Git-Org-Manager)
;; Simple scripts that lets you manage issues in org.


(require 'json)
(require 'org)
(require 'seq)
(require 'org-clock)

;;; Customization

(defgroup gom nil
  "Git-Org-Manager: Syncing Org-mode with Git forges."
  :group 'external
  :prefix "gom-")

(defcustom gom-repos-directory "~/git-repos"
  "Directory where your git repositories are stored."
  :type 'directory
  :group 'gom)

(defcustom gom-issues-file "~/Documents/issues.org"
  "The central Org file where all issues are synced."
  :type 'file
  :group 'gom)

(defcustom gom-git-platform-mapping
  '(("github.com" . github)
    ("gitlab.de" . gitlab)
    ("gitea.de" . gitea))
  "Alist mapping repository domains to their platform."
  :type '(alist :key-type string :value-type symbol)
  :group 'gom)

;;; Core Helpers

(defun gom--check-requirements ()
  "Ensure necessary CLI tools are installed."
  (dolist (bin '("gh" "glab" "tea"))
    (unless (executable-find bin)
      (message "GOM Warning: CLI tool '%s' not found. Some features may fail." bin))))

(defun gom--ensure-issues-buffer ()
  "Ensure the command is only executed inside the issues.org file."
  (unless (and (buffer-file-name)
               (string-equal (expand-file-name (buffer-file-name))
                             (expand-file-name gom-issues-file)))
    (user-error "Command aborted: This can only be run inside %s" gom-issues-file)))

(defun gom--detect-platform (dir)
  "Determine the platform based on the git remote origin URL."
  (let* ((default-directory dir)
         (remote-url (shell-command-to-string "git config --get remote.origin.url")))
    (catch 'found
      (dolist (mapping gom-git-platform-mapping)
        (when (string-match-p (regexp-quote (car mapping)) remote-url)
          (throw 'found (cdr mapping))))
      nil)))

(defun gom--get-repositories ()
  "Return a list of subdirectories in the repos directory."
  (seq-filter #'file-directory-p (directory-files gom-repos-directory t "^[^.]")))

(defun gom--format-minutes-for-git (total-minutes)
  "Convert TOTAL-MINUTES into a time string like '1h 30m'."
  (let ((hours (/ total-minutes 60))
        (mins (% total-minutes 60)))
    (cond
     ((and (> hours 0) (> mins 0)) (format "%dh %dm" hours mins))
     ((> hours 0) (format "%dh" hours))
     (t (format "%dm" mins)))))

(defun gom--format-date (date-str)
  "Extract YYYY-MM-DD HH:MM from various date string formats safely."
  (cond
   ((or (null date-str) (string-empty-p date-str))
    "Unknown Date")

   ((string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[ T]*\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)" date-str)
    (format "%s %s" (match-string 1 date-str) (match-string 2 date-str)))

   (t
    (substring date-str 0 (min (length date-str) 16)))))

(defun gom--clear-local-logbook ()
  "Remove the :LOGBOOK: drawer to prevent double-pushing time."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point))))
      (save-restriction
        (narrow-to-region (point) end)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*:LOGBOOK:.*\\(?:\n.*\\)*?\n[ \t]*:END:[ \t]*\n?" nil t)
          (replace-match ""))))))

(defun gom--sanitize-tag (str)
  "Convert a string into a valid Org tag (alphanumeric, _, @, #, %)."
  (replace-regexp-in-string "[^a-zA-Z0-9_@#%]" "_" str))

;;; Fetching Logic

(defun gom--fetch-issues-unified (dir)
  "Fetch ALL open issues, assignees, and time stats using the correct CLI."
  (let* ((default-directory dir)
         (platform (gom--detect-platform dir)))
    (condition-case nil
        (cond
         ((eq platform 'github)
          (let* ((out (shell-command-to-string "gh issue list --state open --json number,title,url,assignees 2>/dev/null"))
                 (data (json-parse-string out :object-type 'alist :array-type 'list)))
            (mapcar (lambda (iss)
                      (let ((assignees (mapcar (lambda (a) (alist-get 'login a)) (alist-get 'assignees iss))))
                        (list (alist-get 'number iss) (alist-get 'title iss) (alist-get 'url iss) 'github assignees nil)))
                    data)))

         ((eq platform 'gitlab)
          (let* ((out (shell-command-to-string "glab issue list --output json 2>/dev/null"))
                 (data (json-parse-string out :object-type 'alist :array-type 'list)))
            (mapcar (lambda (iss)
                      (let* ((assignees (mapcar (lambda (a) (alist-get 'username a)) (alist-get 'assignees iss)))
                             (time-stats (alist-get 'time_stats iss))
                             (time-spent (when time-stats (alist-get 'human_total_time_spent time-stats))))
                        (list (alist-get 'iid iss) (alist-get 'title iss) (alist-get 'web_url iss) 'gitlab assignees time-spent)))
                    data)))

         ((eq platform 'gitea)
          (let* ((out (shell-command-to-string "tea issues --state open --output json 2>/dev/null"))
                 (data (json-parse-string out :object-type 'alist :array-type 'list)))
            (mapcar (lambda (iss)
                      (let ((assignee (alist-get 'username (alist-get 'assignee iss))))
                        (list (alist-get 'index iss) (alist-get 'title iss) (alist-get 'html_url iss) 'gitea (if assignee (list assignee) nil) nil)))
                    data)))
         (t nil))
      (error nil))))

(defun gom--fetch-issue-conversation (dir platform id)
  "Fetch the body and comments of a specific issue."
  (let ((default-directory dir))
    (cond
     ((eq platform 'github)
      (condition-case nil
          (json-parse-string (shell-command-to-string (format "gh issue view %s --json body,comments 2>/dev/null" id)) :object-type 'alist :array-type 'list)
        (error nil)))
     ((eq platform 'gitlab) `((raw-conversation . ,(shell-command-to-string (format "glab issue view %s -c 2>/dev/null" id)))))
     ((eq platform 'gitea) `((raw-conversation . ,(shell-command-to-string (format "tea issue view %s 2>/dev/null" id)))))
     (t nil))))

(defun gom--insert-conversation (details platform)
  "Parse and insert the conversation with proper Org Property drawers for metadata."
  (let ((body (alist-get 'body details))
        (comments (alist-get 'comments details))
        (raw-conv (alist-get 'raw-conversation details)))
    (cond
     ;; 1. GITHUB (Clean JSON)
     ((or body comments)
      (when (and body (not (string-empty-p body)))
        (insert "*** Original Description\n")
        (insert (format "    %s\n\n" body)))
      (when comments
        (seq-doseq (comment comments)
          (let ((author (alist-get 'login (alist-get 'author comment)))
                (date (alist-get 'createdAt comment))
                (comment-body (alist-get 'body comment)))
            (insert "*** Comment\n")
            (insert (format ":PROPERTIES:\n:AUTHOR: %s\n:DATE: %s\n:END:\n" author date))
            (insert (format "%s\n\n" comment-body))))))

     ;; 2. GITLAB / GITEA (Regex Parsing to Property Drawers)
     ((and raw-conv (not (string-empty-p raw-conv)))
      (let ((start 0)
            (regex "^\\([^ \n]+\\) commented \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.*\\)$")
            (first-comment-pos (string-match "^[^ \n]+ commented [0-9]\\{4\\}" raw-conv)))
        (when first-comment-pos
          (setq start first-comment-pos)
          (while (string-match regex raw-conv start)
            (let* ((author (match-string 1 raw-conv))
                   (date (match-string 2 raw-conv))
                   ;; maybe format the date later but right now it just hangs the script
                   ;; (date (gom--format-date (match-string 2 raw-conv)))
                   (end-of-header (match-end 0))
                   (next-match (save-excursion (string-match regex raw-conv end-of-header)))
                   (comment-body (substring raw-conv end-of-header (or next-match (length raw-conv)))))
              ;; (insert "*** Comment\n")
              ;; (insert (format ":PROPERTIES:\n:AUTHOR: %s\n:DATE: %s\n:END:\n" author date))
              (insert (format "*** Comment on %s by :%s:\n" date author))
              ;; (insert "#+BEGIN_QUOTE\n" (string-trim comment-body) "\n#+END_QUOTE\n\n")
              (insert (format "%s\n\n" (string-trim comment-body)))
              (setq start (or next-match (length raw-conv)))))))))))

;;; Interactive Commands

;;;###autoload
(defun gom-sync ()
  "Fetch all issues, assignees, and conversations from all mapped repos."
  (interactive)
  (gom--check-requirements)
  (with-temp-file gom-issues-file
    (insert "#+TITLE: GOM: Unified Issues\n#+STARTUP: fold\n\n")
    (dolist (dir (gom--get-repositories))
      (when (file-exists-p (expand-file-name ".git" dir))
        (let ((issues (gom--fetch-issues-unified dir))
              (repo-name (file-name-nondirectory (directory-file-name dir))))
          (when issues
            (insert (format "* %s\n" repo-name))
            (dolist (issue issues)
              (let* ((id (nth 0 issue))
                     (title (nth 1 issue))
                     (url (nth 2 issue))
                     (platform (nth 3 issue))
                     (assignees (nth 4 issue))
                     (time-spent (nth 5 issue))
                     (details (gom--fetch-issue-conversation dir platform id))
                     ;; Format tags: :tag1:tag2:
                     (tag-string (if assignees
                                     (concat ":" (mapconcat #'gom--sanitize-tag assignees ":") ":")
                                   "")))

                (insert (format "** TODO %s %s\n" title tag-string))
                (insert ":PROPERTIES:\n")
                (insert (format ":ISSUE_ID: %s\n:REPO_DIR: %s\n:PLATFORM: %s\n" id dir platform))
                (when time-spent (insert (format ":REMOTE_TIME: %s\n" time-spent)))
                (insert ":END:\n")
                (insert (format "   %s\n\n" url))

                (gom--insert-conversation details platform)

                (insert "*** Draft Reply\n")
                (insert "    Type your reply under this heading and run `M-x gom-push`.\n\n")
                )))))))
  (message "GOM: Successfully synced to %s!" gom-issues-file))

;;;###autoload
(defun gom-push ()
  "Push drafted note, local clocked time, and close issue if status is DONE."
  (interactive)
  (gom--ensure-issues-buffer)
  (save-buffer)
  (save-excursion
    ;; FIX 1: Anchor point to the main Issue Heading
    ;; Traverse up until we hit the heading that actually contains the :ISSUE_ID: property.
    (org-back-to-heading t)
    (while (and (not (org-entry-get (point) "ISSUE_ID" nil))
                (org-up-heading-safe)))

    (let* ((issue-id (org-entry-get nil "ISSUE_ID" t))
           (repo-dir (org-entry-get nil "REPO_DIR" t))
           (platform-str (org-entry-get nil "PLATFORM" t))
           (platform (when platform-str (intern platform-str)))
           ;; FIX 2: Check against org-done-keywords (more robust than exact string matching)
           (todo-state (org-get-todo-state))
           (is-done (member todo-state org-done-keywords))

           ;; Sum clocks
           (total-minutes (save-restriction
                            (org-narrow-to-subtree)
                            (org-clock-sum)
                            org-clock-file-total-minutes))
           (time-str (when (and total-minutes (> total-minutes 0))
                       (gom--format-minutes-for-git total-minutes)))

           ;; Get Comment Content
           (beg (save-excursion
                  (goto-char (point))
                  (if (re-search-forward "^\\*\\*\\* Draft Reply" nil t)
                      (progn (forward-line 1) (point))
                    (progn (org-end-of-meta-data t) (point)))))
           ;; FIX 3: Start looking for the `end` heading starting from `beg`
           (end (save-excursion
                  (goto-char beg)
                  (if (outline-next-heading) (point) (point-max))))

           (raw-content (buffer-substring-no-properties beg end))
           (clean-comment ""))

      (if (not (and issue-id repo-dir platform))
          (error "GOM: Missing metadata for #%s! Are you inside an issue subtree?" issue-id)

        (setq clean-comment
              (with-temp-buffer
                (insert raw-content)
                (goto-char (point-min))
                ;; Remove placeholder text
                (while (re-search-forward "Type your reply under this heading.*" nil t)
                  (replace-match ""))
                (goto-char (point-min))
                ;; FIX: Strip out the LOGBOOK drawer so it doesn't push to Git
                (while (re-search-forward "^[ \t]*:LOGBOOK:.*\\(?:\n.*\\)*?\n[ \t]*:END:[ \t]*\n?" nil t)
                  (replace-match ""))
                ;; Clean up surrounding whitespace and newlines
                (string-trim (buffer-string))))

        (let ((default-directory (file-name-as-directory repo-dir)))

          ;; --- STEP 1: PUSH COMMENT ---
          (unless (string-empty-p clean-comment)
            (message "GOM: Pushing comment to #%s..." issue-id)
            (let ((process-environment (cons (format "MSG=%s" clean-comment) process-environment)))
              (cond
               ((eq platform 'github) (shell-command (format "echo \"$MSG\" | gh issue comment %s --body-file -" (shell-quote-argument issue-id))))
               ((eq platform 'gitlab) (shell-command (format "glab issue note %s -m \"$MSG\"" (shell-quote-argument issue-id))))
               ((eq platform 'gitea)  (shell-command (format "echo \"$MSG\" | tea issue comment %s" (shell-quote-argument issue-id))))))
            (when (or time-str is-done) (sleep-for 1)))

          ;; --- STEP 2: PUSH TIME ---
          (when time-str
            (message "GOM: Logging %s to #%s..." time-str issue-id)
            (cond
             ((eq platform 'gitlab)
              (shell-command (format "glab issue note %s -m %s"
                                     (shell-quote-argument issue-id)
                                     (shell-quote-argument (format "/spend %s" time-str)))))
             ((eq platform 'gitea)
              (shell-command (format "echo %s | tea issue comment %s"
                                     (shell-quote-argument (format "/spend %s" time-str))
                                     (shell-quote-argument issue-id))))
             ((eq platform 'github)
              (shell-command (format "gh issue comment %s -b %s"
                                     (shell-quote-argument issue-id)
                                     (shell-quote-argument (format "⏱️ Logged %s" time-str))))))
            (gom--clear-local-logbook)
            (when is-done (sleep-for 1)))

          ;; --- STEP 3: CLOSE ISSUE (If DONE) ---
          (when is-done
            (message "GOM: Closing issue #%s..." issue-id)
            (cond
             ((eq platform 'github) (shell-command (format "gh issue close %s" (shell-quote-argument issue-id))))
             ((eq platform 'gitlab) (shell-command (format "glab issue close %s" (shell-quote-argument issue-id))))
             ((eq platform 'gitea)  (shell-command (format "tea issue close %s" (shell-quote-argument issue-id))))))

          (message "GOM: Update complete for #%s%s."
                   issue-id (if is-done " (Closed)" "")))))))

;;;###autoload
(defun gom-dashboard ()
  "Display an interactive overview of tracked GOM repositories."
  (interactive)
  (let ((buf (get-buffer-create "*GOM Dashboard*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "GOM: Git-Org-Manager Dashboard\n" 'face '(:height 2.0 :weight bold)))
        (insert "======================================\n\n")

        ;; Clickable Directory Link
        (insert "Tracked Directory: ")
        (insert-text-button gom-repos-directory
                            'action (lambda (_) (dired gom-repos-directory))
                            'follow-link t
                            'help-echo "Open directory in Dired")

        ;; Clickable Issues File Link
        (insert "\nIssues File:       ")
        (insert-text-button gom-issues-file
                            'action (lambda (_) (find-file gom-issues-file))
                            'follow-link t
                            'help-echo "Open issues file")
        (insert "\n\n")

        (insert (propertize "Tracked Repositories:\n" 'face 'bold))
        (dolist (dir (gom--get-repositories))
          (when (file-exists-p (expand-file-name ".git" dir))
            (let ((platform (gom--detect-platform dir))
                  (repo-name (file-name-nondirectory (directory-file-name dir))))
              (insert "  - ")
              ;; Clickable Repo Name
              (insert-text-button repo-name
                                  'action `(lambda (_) (dired ,dir))
                                  'follow-link t
                                  'help-echo (format "Open %s in Dired" repo-name))
              (insert (format " [%s]\n" (or platform "Unknown/Unmapped"))))))

        (read-only-mode 1)
        ;; Silently keep the keybindings for power users, but remove the text
        (local-set-key (kbd "q") 'kill-current-buffer)
        (local-set-key (kbd "s") 'gom-sync)))
    (switch-to-buffer buf)))

(provide 'gom)
;;; gom.el ends here
