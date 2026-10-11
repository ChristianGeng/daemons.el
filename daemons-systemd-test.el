(load-file "./daemons-systemd.el")

(ert-deftest systemd-parse-list-item-test ()
  (let ((input "accounts-daemon.service                      disabled")
        (expected '("accounts-daemon" ["accounts-daemon" "disabled"])))
    (should (equal expected
                   (daemons-systemd--parse-list-item input)))))

(ert-deftest systemd-is-simple-service-test ()
  (let ((input '("autofs" ["autofs" "disabled"])))
    (should (daemons-systemd--item-is-simple-service-p input)))
  (let ((input '("autovt@" ["autovt@" "disabled"])))
    (should (not (daemons-systemd--item-is-simple-service-p input)))))

(defconst daemons-systemd-test--unit-files "
accounts-daemon.service                      disabled
alsa-state.service                           static  
auditd.service                               enabled 
autovt@.service                              enabled "
  "systemctl list-unit-files output.")

(defconst daemons-systemd-test--units "
  accounts-daemon.service   loaded    active   running Accounts Service
\u25cf auditd.service            loaded    failed   failed  Security Auditing Service
  getty@tty1.service        loaded    active   running Getty on tty1
  transient-only.service    loaded    active   running Not in unit files"
  "systemctl list-units --all output, with a bullet on the failed unit.")

(defun daemons-systemd-test--list-command-fun (unit-files units)
  "Return a shell stub that answers list-unit-files with UNIT-FILES, list-units with UNITS."
  `(lambda (cmd)
     (cond ((string-match-p "list-unit-files" cmd) ,unit-files)
           ((string-match-p "list-units" cmd) ,units)
           (t (error "Unexpected command %s" cmd)))))

(ert-deftest systemd-list-test ()
  (let* ((daemons-systemd-color nil)
         (daemons--shell-command-to-string-fun
          (daemons-systemd-test--list-command-fun
           daemons-systemd-test--unit-files daemons-systemd-test--units))
         (expected '(("accounts-daemon" ["accounts-daemon" "disabled" "running"])
                     ("alsa-state" ["alsa-state" "static" "inactive"])
                     ("auditd" ["auditd" "enabled" "failed"]))))
    (should (equal expected
                   (daemons-systemd--list)))))

(ert-deftest systemd-list-commands-test ()
  (dolist (case '((nil . ("systemctl list-units --type=service --all --no-legend"
                          "systemctl list-unit-files --type=service --no-legend"))
                  (t . ("systemctl --user list-units --type=service --all --no-legend"
                        "systemctl --user list-unit-files --type=service --no-legend"))))
    (let* ((daemons-systemd-is-user (car case))
           (commands nil)
           (daemons--shell-command-to-string-fun
            (lambda (cmd) (push cmd commands) "")))
      (daemons-systemd--list)
      (should (equal (cdr case) (nreverse commands))))))

(ert-deftest systemd-active-state-test ()
  (dolist (case '((("active" "running") . "running")
                  (("active" "exited") . "exited")
                  (("inactive" "dead") . "dead")
                  (("failed" "failed") . "failed")
                  (("activating" "start-pre") . "activating")
                  (("activating" "auto-restart") . "activating")
                  (("deactivating" "stop-sigterm") . "deactivating")
                  (("reloading" "reload") . "reloading")))
    (should (equal (cdr case) (apply #'daemons-systemd--active-state (car case))))))

(ert-deftest systemd-active-states-test ()
  (let* ((daemons--shell-command-to-string-fun
          (lambda (_) (concat daemons-systemd-test--units "
  nope.service              not-found inactive dead    nope.service
  helper.service            loaded    activating start-pre Helper")))
         (states (daemons-systemd--active-states)))
    (should (equal "running" (gethash "accounts-daemon.service" states)))
    (should (equal "failed" (gethash "auditd.service" states)))
    (should (equal "dead" (gethash "nope.service" states)))
    (should (equal "activating" (gethash "helper.service" states)))
    (should-not (gethash "\u25cf" states))))

(ert-deftest systemd-active-states-query-failed-test ()
  (let ((daemons--shell-command-to-string-fun
         (lambda (_) "Failed to connect to bus: No such file or directory\n")))
    (should-not (daemons-systemd--active-states)))
  (let ((daemons--shell-command-to-string-fun
         (lambda (_) (concat "Warning: something odd\n" daemons-systemd-test--units))))
    (let ((states (daemons-systemd--active-states)))
      (should (equal "running" (gethash "accounts-daemon.service" states)))
      (should-not (gethash "Warning:" states))))
  (let ((daemons--shell-command-to-string-fun (lambda (_) "")))
    (should (hash-table-p (daemons-systemd--active-states)))))

(ert-deftest systemd-list-query-failed-test ()
  (let* ((daemons-systemd-color t)
         (daemons--shell-command-to-string-fun
          (daemons-systemd-test--list-command-fun
           daemons-systemd-test--unit-files
           "Failed to connect to bus: No such file or directory\n"))
         (rows (daemons-systemd--list)))
    (should (equal '("unknown" "unknown" "unknown")
                   (mapcar (lambda (row) (aref (cadr row) 2)) rows)))
    (should-not (get-text-property 0 'font-lock-face (aref (cadr (car rows)) 2)))))

(ert-deftest systemd-active-colours-test ()
  (let ((daemons-systemd-color t))
    (dolist (case '(("running" . daemons-systemd-enabled)
                    ("exited" . daemons-systemd-enabled)
                    ("failed" . daemons-systemd-bad)
                    ("activating" . daemons-systemd-changing)
                    ("deactivating" . daemons-systemd-changing)
                    ("reloading" . daemons-systemd-changing)
                    ("dead" . daemons-systemd-disabled)
                    ("inactive" . daemons-systemd-disabled)))
      (let ((s (daemons-systemd--active-color (car case))))
        (should (equal (car case) s))
        (should (eq (cdr case) (get-text-property 0 'font-lock-face s))))))
  (let ((daemons-systemd-color nil))
    (should-not (get-text-property 0 'font-lock-face
                                   (daemons-systemd--active-color "failed")))))

(ert-deftest systemd-list-colours-both-columns-test ()
  (let* ((daemons-systemd-color t)
         (daemons--shell-command-to-string-fun
          (daemons-systemd-test--list-command-fun
           daemons-systemd-test--unit-files daemons-systemd-test--units))
         (rows (daemons-systemd--list))
         (face (lambda (name column)
                 (get-text-property 0 'font-lock-face
                                    (aref (cadr (assoc name rows)) column)))))
    (should (eq 'daemons-systemd-enabled (funcall face "auditd" 1)))
    (should (eq 'daemons-systemd-bad (funcall face "auditd" 2)))
    (should (eq 'daemons-systemd-disabled (funcall face "accounts-daemon" 1)))
    (should (eq 'daemons-systemd-enabled (funcall face "accounts-daemon" 2)))
    (should (eq 'daemons-systemd-static (funcall face "alsa-state" 1)))
    (should (eq 'daemons-systemd-disabled (funcall face "alsa-state" 2)))))

(ert-deftest systemd-list-revert-picks-up-state-change-test ()
  (let* ((daemons-systemd-color nil)
         (units "  auditd.service loaded active running Auditing")
         (daemons--shell-command-to-string-fun
          (lambda (cmd)
            (funcall (daemons-systemd-test--list-command-fun
                      daemons-systemd-test--unit-files units)
                     cmd)))
         (active (lambda (name)
                   (goto-char (point-min))
                   (while (not (equal name (tabulated-list-get-id)))
                     (forward-line))
                   (aref (tabulated-list-get-entry) 2))))
    (cl-letf (((symbol-function 'daemons-init-system-submodule) (lambda () 'daemons-systemd)))
      (with-temp-buffer
        (daemons-mode)
        (setq-local tabulated-list-entries
                    (lambda () (daemons--list 'daemons-systemd)))
        (tabulated-list-print)
        (should (equal ["Daemon (service)" "Enabled" "Active"]
                       (vconcat (mapcar #'car tabulated-list-format))))
        (should (equal "running" (funcall active "auditd")))
        (setq units "\u25cf auditd.service loaded failed failed Auditing")
        (revert-buffer)
        (should (equal "failed" (funcall active "auditd")))))))

(ert-deftest systemd-list-sorts-by-active-test ()
  (let ((daemons-systemd-color t)
        (daemons--shell-command-to-string-fun
         (daemons-systemd-test--list-command-fun
          daemons-systemd-test--unit-files daemons-systemd-test--units)))
    (cl-letf (((symbol-function 'daemons-init-system-submodule) (lambda () 'daemons-systemd)))
      (with-temp-buffer
        (daemons-mode)
        (setq-local tabulated-list-entries
                    (lambda () (daemons--list 'daemons-systemd)))
        (should (nth 2 (aref tabulated-list-format 2)))
        (setq tabulated-list-sort-key (cons "Active" nil))
        (tabulated-list-print)
        (goto-char (point-min))
        (let (ids)
          (while (tabulated-list-get-id)
            (push (tabulated-list-get-id) ids)
            (forward-line))
          (should (equal '("auditd" "alsa-state" "accounts-daemon") (nreverse ids))))))))

(defconst daemons-systemd-test--failed-status
  (concat
   "\e[0;1;31m×\e[0m dtest-fail.service - failing unit\n"
   "     Loaded: loaded (\e]8;;file://box/u/dtest-fail.service\a/u/dtest-fail.service\e]8;;\a; static)\n"
   "     Active: \e[0;1;31mfailed\e[0m (Result: exit-code) since Sun 2026-10-11 13:17:19 UTC; 1s ago\n"
   "    Process: 4124 ExecStart=/bin/sh -c exit 3 \e[0;1;31m(code=exited, status=3)\e[0m\n"
   "\n"
   "Oct 11 13:17:19 box sh[4124]: \e[0;1;31m\e[0;1;39m\e[0;1;31mboom: something broke\e[0m\n"
   "Oct 11 13:17:19 box sh[4124]: \e[0;1;38;5;185m\e[0;1;39m\e[0;1;38;5;185ma warning\e[0m\n"
   "Oct 11 13:17:19 box sh[4124]: plain info\n"
   "Oct 11 13:17:19 box systemd[1056]: \e[0;1;39m\e[0;1;31m\e[0;1;39mdtest-fail.service: Main process exited\e[0m\n")
  "systemctl status output for a failed unit, as captured with SYSTEMD_COLORS=1.")

(defconst daemons-systemd-test--running-status
  (concat
   "● dtest-ok.service - running unit\n"
   "     Loaded: loaded (/u/dtest-ok.service; static)\n"
   "     Active: active (running) since Sun 2026-10-11 13:17:19 UTC; 1s ago\n"
   "\n"
   "Oct 11 13:17:19 box sh[4122]: started\n")
  "systemctl status output for a running unit.")

(defun daemons-systemd-test--status-buffer (output color)
  "Show OUTPUT as `daemons-status' would, with `daemons-systemd-color' COLOR.
Return the fontified buffer text and the command that was run."
  (let* ((daemons-systemd-color color)
         (daemons-systemd-is-user t)
         (daemons-show-output-in-minibuffer t)
         (daemons-always-sudo nil)
         (default-directory "/tmp/")
         (command nil)
         (daemons--shell-command-fun (lambda (cmd _) (setq command cmd) (insert output))))
    (cl-letf (((symbol-function 'daemons-init-system-submodule) (lambda () 'daemons-systemd))
              ((symbol-function 'message) #'ignore))
      (daemons-status "dtest")
      (with-current-buffer (daemons--get-output-buffer-name
                            (daemons--get-user-and-hostname default-directory))
        (font-lock-ensure)
        (prog1 (list (buffer-string) command)
          (kill-buffer))))))

(defun daemons-systemd-test--face (text string)
  "Return the face on the first character of STRING within TEXT."
  (let ((pos (string-match (regexp-quote string) text)))
    (should pos)
    (or (get-text-property pos 'face text)
        (get-text-property pos 'font-lock-face text))))

(ert-deftest systemd-status-cmd-asks-for-colours-test ()
  (let ((daemons-systemd-is-user t)
        (daemons-systemctl-command-fn #'daemons-systemctl-cmd))
    (let ((daemons-systemd-color t))
      (should (equal "env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 systemctl --user status foo"
                     (daemons-systemd--status-cmd "foo"))))
    (let ((daemons-systemd-color nil))
      (should (equal "systemctl --user status foo"
                     (daemons-systemd--status-cmd "foo"))))))

(ert-deftest systemd-status-cmd-colours-reach-systemctl-test ()
  "The colour variables go right before systemctl, inside any sudo wrapper."
  (let ((daemons-systemd-color t))
    (dolist (case '(("sudo systemctl status foo"
                     . "sudo env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 systemctl status foo")
                    ("sudo -u bob /usr/bin/systemctl --user status foo"
                     . "sudo -u bob env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 /usr/bin/systemctl --user status foo")
                    ("cd /tmp && systemctl status foo"
                     . "cd /tmp && env SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0 systemctl status foo")
                    ("my-status-wrapper foo"
                     . "export SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0; my-status-wrapper foo")
                    ("cd /tmp && my-status-wrapper foo"
                     . "export SYSTEMD_COLORS=1 SYSTEMD_URLIFY=0; cd /tmp && my-status-wrapper foo")))
      (let ((daemons-systemctl-command-fn (lambda (_command _service) (car case))))
        (should (equal (cdr case) (daemons-systemd--status-cmd "foo")))))))

(ert-deftest systemd-status-colours-failed-unit-test ()
  (pcase-let ((`(,text ,command)
               (daemons-systemd-test--status-buffer daemons-systemd-test--failed-status t)))
    (should (string-prefix-p "env SYSTEMD_COLORS=1 " command))
    (should-not (string-match-p "\e" text))
    (should (string-match-p "Loaded: loaded (/u/dtest-fail.service; static)" text))
    (should (eq 'daemons-systemd-bad (daemons-systemd-test--face text "failed (Result")))
    (should (eq 'daemons-systemd-label (daemons-systemd-test--face text "Loaded:")))
    (should (eq 'daemons-systemd-label (daemons-systemd-test--face text "Active:")))
    (should (eq 'daemons-systemd-bad (daemons-systemd-test--face text "boom")))
    (should-not (daemons-systemd-test--face text "a warning"))
    (should-not (daemons-systemd-test--face text "plain info"))
    (should-not (daemons-systemd-test--face text "dtest-fail.service: Main"))))

(ert-deftest systemd-status-colours-unit-states-test ()
  (dolist (case '(("active (running)" . daemons-systemd-enabled)
                  ("active (exited)" . daemons-systemd-enabled)
                  ("inactive (dead)" . daemons-systemd-bad)
                  ("activating (start)" . daemons-systemd-changing)
                  ("deactivating (stop-sigterm)" . daemons-systemd-changing)
                  ("reloading (reload)" . daemons-systemd-changing)))
    (let* ((output (replace-regexp-in-string
                    "active (running)" (car case) daemons-systemd-test--running-status t t))
           (text (car (daemons-systemd-test--status-buffer output t))))
      (should (eq (cdr case) (daemons-systemd-test--face text (car case)))))))

(ert-deftest systemd-status-without-colour-test ()
  (pcase-let ((`(,text ,command)
               (daemons-systemd-test--status-buffer daemons-systemd-test--running-status nil)))
    (should (equal "systemctl --user status dtest" command))
    (should-not (daemons-systemd-test--face text "active (running)"))
    (should-not (daemons-systemd-test--face text "Loaded:"))))

(defun daemons-systemd-test--status-line (output &optional color name)
  "Return the status line of NAME for systemctl OUTPUT, and the command run.
NAME defaults to \"rygel\", COLOR sets `daemons-systemd-color'."
  (let* ((daemons-systemd-color color)
         (daemons-systemd-is-user t)
         (daemons-systemctl-command-fn #'daemons-systemctl-cmd)
         (command nil)
         (daemons--shell-command-to-string-fun (lambda (cmd) (setq command cmd) output)))
    (cl-letf (((symbol-function 'daemons-init-system-submodule) (lambda () 'daemons-systemd))
              ((symbol-function 'message) #'ignore))
      (list (daemons-status-line (or name "rygel")) command))))

(defun daemons-systemd-test--show (load active sub file since)
  "Return `systemctl show' output for the status line properties.
LOAD, ACTIVE, SUB, FILE and SINCE are the property values."
  (format (concat "LoadState=%s\nActiveState=%s\nSubState=%s\n"
                  "UnitFileState=%s\nActiveEnterTimestamp=%s\n")
          load active sub file since))

(defun daemons-systemd-test--today (time)
  "Return an ActiveEnterTimestamp for TIME today."
  (format-time-string (concat "%a %F " time " UTC")))

(ert-deftest systemd-status-line-running-test ()
  (pcase-let ((`(,line ,command)
               (daemons-systemd-test--status-line
                (daemons-systemd-test--show "loaded" "active" "running" "enabled"
                                            (daemons-systemd-test--today "09:14:07"))
                t)))
    (should (equal (concat "systemctl --user show -p LoadState,ActiveState,SubState,"
                           "UnitFileState,ActiveEnterTimestamp rygel.service")
                   command))
    (should (equal "rygel.service: active (running) since 09:14 \u2014 enabled" line))
    (should (eq 'daemons-systemd-enabled (daemons-systemd-test--face line "active (running)")))
    (should-not (daemons-systemd-test--face line "rygel.service"))
    (should-not (daemons-systemd-test--face line "enabled"))))

(ert-deftest systemd-status-line-any-property-order-test ()
  "The properties are read by name, whatever order systemctl prints them in."
  (should (equal "rygel.service: active (running) \u2014 enabled"
                 (car (daemons-systemd-test--status-line
                       (concat "ActiveEnterTimestamp=n/a\nUnitFileState=enabled\n"
                               "SubState=running\nActiveState=active\nLoadState=loaded\n")
                       t)))))

(ert-deftest systemd-status-line-started-before-today-test ()
  (should (equal "cron.service: active (running) since 2020-02-03 07:05 \u2014 enabled"
                 (car (daemons-systemd-test--status-line
                       (daemons-systemd-test--show "loaded" "active" "running" "enabled"
                                                   "Mon 2020-02-03 07:05:00 UTC")
                       t "cron.service")))))

(ert-deftest systemd-status-line-stopped-test ()
  (let ((line (car (daemons-systemd-test--status-line
                    (daemons-systemd-test--show "loaded" "inactive" "dead" "disabled"
                                                "Sun 2026-10-11 08:00:00 UTC")
                    t))))
    (should (equal "rygel.service: inactive (dead) \u2014 disabled" line))
    (should (eq 'daemons-systemd-bad (daemons-systemd-test--face line "inactive (dead)")))))

(ert-deftest systemd-status-line-unknown-unit-test ()
  "An unknown unit reports \"no such unit\", also after a warning on stderr."
  (let ((unknown (daemons-systemd-test--show "not-found" "inactive" "dead" "" "n/a")))
    (dolist (output (list unknown
                          (concat "Invalid unit name \"foo/bar\" escaped as \"foo-bar\".\n"
                                  unknown)))
      (let ((line (car (daemons-systemd-test--status-line output t "nope"))))
        (should (equal "nope.service: no such unit" line))
        (should (eq 'daemons-systemd-bad (daemons-systemd-test--face line "no such unit")))))))

(ert-deftest systemd-status-line-states-test ()
  (dolist (case '(("failed" "failed" daemons-systemd-bad)
                  ("activating" "start" daemons-systemd-changing)
                  ("deactivating" "stop-sigterm" daemons-systemd-changing)
                  ("reloading" "reload" daemons-systemd-changing)))
    (pcase-let* ((`(,active ,sub ,face) case)
                 (state (format "%s (%s)" active sub))
                 (line (car (daemons-systemd-test--status-line
                             (daemons-systemd-test--show "loaded" active sub "static" "n/a")
                             t))))
      (should (equal (concat "rygel.service: " state " \u2014 static") line))
      (should (eq face (daemons-systemd-test--face line state))))))

(ert-deftest systemd-status-line-without-unit-file-state-test ()
  (should (equal "run-u1.scope: active (running)"
                 (car (daemons-systemd-test--status-line
                       (daemons-systemd-test--show "loaded" "active" "running" "" "n/a")
                       t "run-u1.scope")))))

(ert-deftest systemd-status-line-without-colour-test ()
  (dolist (output (list (daemons-systemd-test--show "loaded" "active" "running" "enabled" "n/a")
                        (daemons-systemd-test--show "not-found" "inactive" "dead" "" "n/a")))
    (let ((line (car (daemons-systemd-test--status-line output nil))))
      (should-not (text-property-not-all 0 (length line) 'face nil line)))))

(ert-deftest systemd-status-line-bad-output-test ()
  "Output without the properties, as from an old systemctl or no bus, is an error."
  (dolist (output '("Failed to connect to bus: No such file or directory\n"
                    "systemctl: unrecognized option '--value'\n"
                    ""))
    (should-error (daemons-systemd-test--status-line output t))))

(ert-deftest systemd-status-line-remote-shows-date-test ()
  "A remote host's date may differ from ours, so its start date is shown.
A local /sudo:: directory, as from `daemons-always-sudo', is not remote."
  (let ((timestamp (daemons-systemd-test--today "09:14:07")))
    (dolist (dir '("/tmp/" "/sudo::/tmp/"))
      (should (equal "09:14" (let ((default-directory dir))
                               (daemons-systemd--status-line-time timestamp)))))
    (should (equal (concat (format-time-string "%F") " 09:14")
                   (let ((default-directory "/ssh:box:/tmp/"))
                     (daemons-systemd--status-line-time timestamp))))))

;; journal
(defconst daemons-systemd-test--journal-output
  (concat "Oct 11 14:42:39 host bash[4715]: \e[0;1;31merr line\e[0m\n"
          "Oct 11 14:42:39 host bash[4715]: \e[0;1;38;5;185mwarn line\e[0m\n"
          "Oct 11 14:42:39 host bash[4715]: info line\n")
  "journalctl output with SYSTEMD_COLORS=1: an err, a warning and an info line.")

(ert-deftest systemd-journal-command-test ()
  (let ((daemons-systemd-color nil))
    (let ((daemons-systemd-is-user nil))
      (should (equal '("journalctl" "--no-pager" "-f" "-u" "foo.service")
                     (daemons-systemd--journal-command "foo"))))
    (let ((daemons-systemd-is-user t))
      (should (equal '("journalctl" "--user" "--no-pager" "-f" "-u" "foo.timer")
                     (daemons-systemd--journal-command "foo.timer"))))))

(ert-deftest systemd-journal-command-asks-for-colours-test ()
  (let ((daemons-systemd-color t)
        (daemons-systemd-is-user t))
    (should (equal '("env" "SYSTEMD_COLORS=1" "SYSTEMD_URLIFY=0"
                     "journalctl" "--user" "--no-pager" "-f" "-u" "foo.service")
                   (daemons-systemd--journal-command "foo")))))

(ert-deftest systemd-journal-starts-process-test ()
  "The journal follows in an async process in `default-directory'."
  (let ((daemons-systemd-color nil)
        (daemons-systemd-is-user t)
        (args nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist) (setq args plist) 'process)))
      (with-temp-buffer
        (should (eq 'process (daemons-systemd--journal "foo")))
        (should (eq (current-buffer) (plist-get args :buffer)))
        (should (equal '("journalctl" "--user" "--no-pager" "-f" "-u" "foo.service")
                       (plist-get args :command)))
        (should (plist-get args :file-handler))
        (should (eq #'daemons-systemd--journal-filter (plist-get args :filter)))))))

(ert-deftest systemd-journal-is-a-command-test ()
  (should (eq 'daemons-systemd--journal (daemons--command 'journal 'daemons-systemd))))

(defun daemons-systemd-test--journal-faces ()
  "Return each line of the current buffer with its face."
  (mapcar (lambda (line)
            (goto-char (point-min))
            (search-forward line)
            (cons (get-text-property (match-beginning 0) 'face) line))
          (split-string (buffer-string) "\n" t)))

(ert-deftest systemd-journal-line-test ()
  (let ((daemons-systemd-color t))
    (with-temp-buffer
      (dolist (line (split-string daemons-systemd-test--journal-output "\n" t))
        (insert (daemons-systemd--journal-line line) "\n"))
      (should (equal '((daemons-systemd-bad . "Oct 11 14:42:39 host bash[4715]: err line")
                       (daemons-systemd-changing . "Oct 11 14:42:39 host bash[4715]: warn line")
                       (nil . "Oct 11 14:42:39 host bash[4715]: info line"))
                     (daemons-systemd-test--journal-faces))))))

(ert-deftest systemd-journal-line-without-colour-test ()
  (let ((daemons-systemd-color nil))
    (should-not (text-properties-at
                 0 (daemons-systemd--journal-line "x \e[0;1;31merr\e[0m")))
    (should (equal "x err" (daemons-systemd--journal-line "x \e[0;1;31merr\e[0m")))))

(defmacro daemons-systemd-test--with-journal-process (&rest body)
  "Run BODY in a buffer with a live cat process, bound to `process'."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((process (make-process :name "daemons-test-journal" :buffer (current-buffer)
                                  :command '("cat") :connection-type 'pipe :noquery t
                                  :sentinel #'ignore)))
       (unwind-protect (progn ,@body)
         (delete-process process)))))

(ert-deftest systemd-journal-filter-split-output-test ()
  "Output split anywhere, even inside an escape, is shown line by line."
  (let ((daemons-systemd-color t))
    (daemons-systemd-test--with-journal-process
      (let ((output daemons-systemd-test--journal-output))
        (daemons-systemd--journal-filter process (substring output 0 37))
        (should (equal "" (buffer-string)))
        (daemons-systemd--journal-filter process (substring output 37 100))
        (should (equal "Oct 11 14:42:39 host bash[4715]: err line\n" (buffer-string)))
        (daemons-systemd--journal-filter process (substring output 100)))
      (should (equal '(daemons-systemd-bad daemons-systemd-changing nil)
                     (mapcar #'car (daemons-systemd-test--journal-faces))))
      (should-not (string-match-p "\e" (buffer-string))))))

(ert-deftest systemd-journal-sentinel-flushes-last-line-test ()
  "A last line without newline is shown when journalctl ends."
  (let ((daemons-systemd-color t))
    (daemons-systemd-test--with-journal-process
      (daemons-systemd--journal-filter process "one\ntwo")
      (delete-process process)
      (daemons-systemd--journal-sentinel process "killed\n")
      (should (equal "one\ntwo\n-- daemons-test-journal killed --\n" (buffer-string))))))

(ert-deftest systemd-journal-follows-output-test ()
  "`daemons-journal' shows journalctl's output as it comes, highlighted.
A printf stands in for journalctl."
  (let ((daemons-systemd-color t)
        (daemons-systemd-is-user t)
        (daemons-journal-function nil)
        (daemons-always-sudo nil)
        (make-process (symbol-function 'make-process))
        (commands nil))
    (cl-letf (((symbol-function 'daemons-init-system-submodule) (lambda () 'daemons-systemd))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (push (plist-get plist :command) commands)
                 (apply make-process
                        (plist-put plist :command
                                   (list "printf" "%s"
                                         daemons-systemd-test--journal-output))))))
      (let* ((buffer (save-current-buffer (daemons-journal "foo")))
             (process (get-buffer-process buffer)))
        (unwind-protect
            (progn
              (while (process-live-p process) (accept-process-output process 0.1))
              (accept-process-output nil 0.1)
              (should (equal '(("env" "SYSTEMD_COLORS=1" "SYSTEMD_URLIFY=0" "journalctl"
                                "--user" "--no-pager" "-f" "-u" "foo.service"))
                             commands))
              (with-current-buffer buffer
                (should (equal '(daemons-systemd-bad daemons-systemd-changing nil nil)
                               (mapcar #'car (daemons-systemd-test--journal-faces))))
                (should (string-suffix-p "-- journalctl finished --\n" (buffer-string)))))
          (kill-buffer buffer))))))
