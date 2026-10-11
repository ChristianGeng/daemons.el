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
