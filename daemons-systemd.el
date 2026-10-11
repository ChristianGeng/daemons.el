;;; daemons-systemd.el --- UI for managing init system daemons (services) -*- lexical-binding: t -*-

;; Copyright (c) 2018 Chris Bowdon
;;
;; Author: Chris Bowdon
;; URL: https://github.com/cbowdon/daemons.el
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: February 13, 2018
;; Modified: December 15, 2018
;; Version: 2.1.0
;; Keywords: unix convenience
;; Package-Requires: ((emacs "25.1"))
;;
;;; Commentary:
;; This file provides systemd support for daemons.el.

;;; Code:
(require 'seq)
(require 'daemons)

(defcustom daemons-systemd-is-user nil
  "Determines if we want to run commands as `--user'"
  :type 'boolean
  :group 'daemons)

(defcustom daemons-systemctl-command-fn #'daemons-systemctl-cmd
  "Function used to return string systemctl commands suitable for `shell-command'.

It should take two string input arguments, denoting the desired systemctl
command, and the service to run the command with.

The default value for this user option will respect the value of
`daemons-systemd-is-user'.  In order to also do this for any custom value of
this option, see the implementation of `daemons-systemd--cmd'."
  :type 'function
  :group 'daemons)

(defcustom daemons-systemd-color nil
  "If non-nil, colorize services in systemd buffers according to their statuses."
  :type 'boolean
  :group 'daemons)

(defface daemons-systemd-enabled
  '((((class color)) (:foreground "green")))
  "Face used in systemd buffers for enabled services.")

(defface daemons-systemd-disabled
  '((((class color)) (:foreground "DimGrey")))
  "Face used in systemd buffers for disabled services.")

(defface daemons-systemd-alias
  '((((class color)) (:foreground "cyan")))
  "Face used in systemd buffers for alias services.")

(defface daemons-systemd-linked
  '((((class color)) (:foreground "cyan")))
  "Face used in systemd buffers for linked services.")

(defface daemons-systemd-masked
  '((((class color)) (:foreground "LightCoral")))
  "Face used in systemd buffers for masked services.")

(defface daemons-systemd-static
  '((((class color)) (:foreground "DimGrey" :italic t)))
  "Face used in systemd buffers for static services.")

(defface daemons-systemd-generated
  '((((class color)) (:foreground "silver")))
  "Face used in systemd buffers for generated services.")

(defface daemons-systemd-indirect
  '((((class color)) (:foreground "DimGrey")))
  "Face used in systemd buffers for indirect services.")

(defface daemons-systemd-transient
  '((((class color)) (:foreground "DimGrey")))
  "Face used in systemd buffers for transient services.")

(defface daemons-systemd-bad
  '((((class color)) (:foreground "red" :bold t)))
  "Face used in systemd buffers for bad services.")

(defface daemons-systemd-changing
  '((((class color)) (:foreground "yellow")))
  "Face used in systemd status output for units that are changing state."
  :group 'daemons)

(defface daemons-systemd-label
  '((t (:inherit bold)))
  "Face used in systemd status output for the Loaded: and Active: labels."
  :group 'daemons)

(defun daemons-systemd--color (status string)
  "Colorize STRING according to STATUS."
  (cond ((null daemons-systemd-color) string)
        ((or (string= status "enabled") (string= status "enabled-runtime"))
         (propertize string 'font-lock-face 'daemons-systemd-enabled))
        ((string= status "disabled") (propertize string 'font-lock-face 'daemons-systemd-disabled))
        ((string= status "alias") (propertize string 'font-lock-face 'daemons-systemd-alias))
        ((or (string= status "linked") (string= status "linked-runtime"))
         (propertize string 'font-lock-face 'daemons-systemd-alias))
        ((or (string= status "masked") (string= status "masked-runtime"))
         (propertize string 'font-lock-face 'daemons-systemd-masked))
        ((string= status "static") (propertize string 'font-lock-face 'daemons-systemd-static))
        ((string= status "generated") (propertize string 'font-lock-face 'daemons-systemd-generated))
        ((string= status "indirect") (propertize string 'font-lock-face 'daemons-systemd-indirect))
        ((string= status "transient") (propertize string 'font-lock-face 'daemons-systemd-transient))
        ((string= status "bad") (propertize string 'font-lock-face 'daemons-systemd-bad))
        (t string)))

(defun daemons-systemd--cmd ()
  "Appends `--user' to the `systemctl' call if `daemons-systemd-is-user' is set"
  (if daemons-systemd-is-user
      "systemctl --user"
    "systemctl"))

(defun daemons--systemd-documentation-for (service)
  "Return documentation for SERVICE."
  (let* ((output (shell-command-to-string
                  (format "systemctl show %s --no-pager" service)))
         (lines (split-string output "\n"))
         (prefix "Description=")
         (desc (seq-find (lambda (s) (string-prefix-p prefix s)) lines)))
    (when desc
      (substring desc (length prefix)))))

(defun daemons--systemd-eldoc-function (callback &rest _ignored)
  "Document service at point by calling CALLBACK."
  (when-let* ((maybe-service (thing-at-point 'symbol))
              (line (thing-at-point 'line))
              ((string= maybe-service (car (split-string (string-trim line)))))
              (result (daemons--systemd-documentation-for maybe-service)))
    (funcall callback result
             :thing maybe-service
             :face 'font-lock-variable-name-face)))

(defun daemons-systemctl-cmd (command service)
  "Return a string suitable for `shell-command' for COMMAND run with SERVICE.

COMMAND should be a valid systemctl command, and SERVICE an existing systemd
service.  Both should be strings.

\"--user\" will be appended to the systemctl call if `daemons-systemd-is-user' is set."
  (format "%s %s %s" (daemons-systemd--cmd) command service))

(defconst daemons-systemd--colour-vars "SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0"
  "Environment that makes systemctl colour its output.")

(defun daemons-systemd--status-cmd (service)
  "Return the shell command that shows the status of SERVICE.
With `daemons-systemd-color', systemctl is asked for colours so that
`daemons-systemd--mark-journal-errors' can find journal lines of priority
err and above.  The variables are set by `env' right before the systemctl
word, so they survive a wrapper such as sudo, which drops the caller's
environment.  A command without that word gets them exported first, which
keeps shell builtins and compound commands working."
  (let ((cmd (funcall daemons-systemctl-command-fn "status" service)))
    (cond ((not daemons-systemd-color) cmd)
          ((string-match "\\(?:^\\|[ \t]\\)\\([^ \t]*/\\)?systemctl\\_>" cmd)
           (let ((start (or (match-beginning 1)
                            (- (match-end 0) (length "systemctl")))))
             (concat (substring cmd 0 start) "env " daemons-systemd--colour-vars
                     " " (substring cmd start))))
          (t (concat "export " daemons-systemd--colour-vars "; " cmd)))))

(defconst daemons-systemd--status-font-lock-keywords
  '(("^ *\\(Loaded\\|Active\\):" 1 'daemons-systemd-label)
    ("^ *Active: \\(active ([^)\n]*)\\)" 1 'daemons-systemd-enabled)
    ("^ *Active: \\(failed\\|inactive (dead)\\)" 1 'daemons-systemd-bad)
    ("^ *Active: \\(\\(?:de\\)?activating\\|reloading\\)" 1 'daemons-systemd-changing))
  "Font-lock keywords for systemctl status output.")

(defconst daemons-systemd--escape-regexp
  "\e\\[[0-9;:]*m\\|\e]8;;[^\a\e\n]*\\(?:\a\\|\e\\\\\\)"
  "Regexp for the SGR and hyperlink escapes in coloured systemctl output.")

(defun daemons-systemd--mark-journal-errors ()
  "Give journal lines of priority err and above `daemons-systemd-bad'.
systemd starts the message of such a line with bold red; the journal
follows the first empty line after the Active: line."
  (save-excursion
    (goto-char (point-min))
    (when (and (re-search-forward "^ *Active:" nil t)
               (re-search-forward "^$" nil t))
      (while (re-search-forward "^[^\e\n]*: \\(\e\\[0;1;31m\\)" nil t)
        (put-text-property (match-beginning 1) (line-end-position)
                           'font-lock-face 'daemons-systemd-bad)))))

(defun daemons-systemd--strip-escapes ()
  "Delete the escapes coloured systemctl output contains."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward daemons-systemd--escape-regexp nil t)
      (replace-match ""))))

(defun daemons-systemd--setup-output ()
  "Colour systemd output in the current `daemons-output-mode' buffer.
Does nothing unless `daemons-systemd-color' is set."
  (when (and daemons-systemd-color
             (eq (daemons-init-system-submodule) 'daemons-systemd))
    (let ((inhibit-read-only t))
      (daemons-systemd--mark-journal-errors)
      (daemons-systemd--strip-escapes))
    (setq-local font-lock-defaults
                '(daemons-systemd--status-font-lock-keywords t))))

(add-hook 'daemons-output-mode-hook #'daemons-systemd--setup-output)

(daemons-define-submodule daemons-systemd
  "Daemons submodule for systemd."

  :test (and (eq system-type 'gnu/linux)
             (executable-find "systemctl"))
  :commands
  '((status . daemons-systemd--status-cmd)
    (start . (lambda (name) (funcall daemons-systemctl-command-fn "start" name)))
    (stop . (lambda (name) (funcall daemons-systemctl-command-fn "stop" name)))
    (restart . (lambda (name) (funcall daemons-systemctl-command-fn "restart" name)))
    (reload . (lambda (name) (funcall daemons-systemctl-command-fn "reload" name)))
    (enable . (lambda (name) (funcall daemons-systemctl-command-fn "enable" name)))
    (disable . (lambda (name) (funcall daemons-systemctl-command-fn "disable" name))))

  :list (daemons-systemd--list)

  :headers [("Daemon (service)" 60 t) ("Enabled" 20 t) ("Active" 15 t)]

  :eldoc-documentation-function #'daemons--systemd-eldoc-function)

(defun daemons-systemd--parse-list-item (raw-systemctl-output)
  "Parse a single line from RAW-SYSTEMCTL-OUTPUT into a tabulated list item."
  (let* ((parts (split-string raw-systemctl-output))
         (name (replace-regexp-in-string "\.service" "" (car parts)))
         (status (cadr parts)))
    (list name (vector (daemons-systemd--color status name) (daemons-systemd--color status status)))))

(defun daemons-systemd--item-is-simple-service-p (item)
  "Return non-nil if ITEM is not a template service.
ITEM should correspond to the output type of `daemons-systemd--parse-list-item'."
  (not (string-match-p "@$" (car item))))

(defun daemons-systemd--active-state (active sub)
  "Return the short state of a unit whose ACTIVE and SUB states are given.
This is SUB, such as \"running\" or \"dead\", except while the unit
changes state: then SUB is a step such as \"start-pre\", and ACTIVE,
such as \"activating\", says more."
  (if (member active '("activating" "deactivating" "reloading"))
      active
    sub))

(defun daemons-systemd--active-color (state)
  "Return STATE, colorized if `daemons-systemd-color' is set.
A dead unit is dimmed, not marked bad: for a timer-driven oneshot unit
that is the normal state between runs."
  (let ((face (pcase state
                ((or "running" "exited") 'daemons-systemd-enabled)
                ("failed" 'daemons-systemd-bad)
                ((or "activating" "deactivating" "reloading") 'daemons-systemd-changing)
                ((or "dead" "inactive") 'daemons-systemd-disabled))))
    (if (and daemons-systemd-color face)
        (propertize state 'font-lock-face face)
      state)))

(defun daemons-systemd--active-states ()
  "Return a hash table of the short active state of each loaded service.
The keys are unit names such as \"foo.service\"."
  (let ((states (make-hash-table :test 'equal)))
    (dolist (line (daemons--split-lines
                   (daemons--shell-command-to-string
                    (format "%s list-units --type=service --all --no-legend"
                            (daemons-systemd--cmd)))))
      (let ((parts (split-string line)))
        ;; Failed and not-found units are marked with a bullet.
        (when (member (car parts) '("\u25cf" "*"))
          (setq parts (cdr parts)))
        (pcase-let ((`(,unit ,_load ,active ,sub . ,_) parts))
          (when sub
            (puthash unit (daemons-systemd--active-state active sub) states)))))
    states))

(defun daemons-systemd--list ()
  "Return a list of daemons on a systemd system.
Each item is (NAME [NAME ENABLED ACTIVE]).  Units that `systemctl
list-units' does not report have never been loaded; they are inactive."
  (let ((states (daemons-systemd--active-states)))
    (thread-last  (format "%s list-unit-files --type=service --no-legend" (daemons-systemd--cmd))
      (daemons--shell-command-to-string)
      (daemons--split-lines)
      (seq-map (lambda (line)
                 (let ((item (daemons-systemd--parse-list-item line))
                       (state (gethash (car (split-string line)) states "inactive")))
                   (list (car item)
                         (vconcat (cadr item)
                                  (vector (daemons-systemd--active-color state)))))))
      (seq-filter 'daemons-systemd--item-is-simple-service-p))))

(defun daemons-systemd-toggle-user ()
  "Toggle showing of user services"
  (interactive)
  (setq daemons-systemd-is-user (not daemons-systemd-is-user))
  (revert-buffer))

(provide 'daemons-systemd)
;;; daemons-systemd.el ends here
