(require 'ert)
(load-file "./daemons.el")

(ert-deftest daemons-run-test ()
  (let* (;; mock the shell command function to just echo into buffer
         (daemons--shell-command-fun (lambda (cmd ins) (insert cmd)))
         (expected "COMMAND=foo"))
    ;; mock up a submodule
    (daemons-define-submodule daemons-run-test
      "Test: run submodule"
      :test t
      :commands '((status . (lambda (name) (format "COMMAND=%s" name))))
      :headers [("Daemon name" 100 t)]
      :list '("foo" "bar"))
    ;; add the submodule to the list
    (add-to-list 'daemons-init-system-submodules 'daemons-run-test)
    (should (equal expected
                   (with-temp-buffer
                     (daemons--run 'status "foo")
                     (buffer-string))))))

(ert-deftest daemons-define-submodule-test ()
  "A correct definition should populate `daemons--init-system-submodules-alist'."
  (let ((expected (list :docstring "Test: happy path submodule"
                        :test (lambda () t)
                        :commands nil
                        :list (lambda () nil)
                        :headers (lambda () [])
                        :eldoc-documentation-function nil)))
    (daemons-define-submodule daemons-happy
      "Test: happy path submodule"
      :test t
      :commands nil
      :headers []
      :list nil)
    (should (equal expected
                   (alist-get 'daemons-happy
                              daemons--init-system-submodules-alist)))))

(ert-deftest daemons--test-submodule-test ()
  (daemons-define-submodule daemons-positive-test
    "Test: positive test submodule"
    :test t
    :commands nil
    :headers []
    :list nil)
  (daemons-define-submodule daemons-negative-test
    "Test: negative test submodule"
    :test nil
    :commands nil
    :headers []
    :list nil)
  (should (daemons--test-submodule 'daemons-positive-test))
  (should (not (daemons--test-submodule 'daemons-negative-test))))

(ert-deftest daemons--commands-alist-test ()
  (let ((expected '((stop . (lambda () "plz stop"))
                    (start . (lambda () "let's go already")))))
    (daemons-define-submodule daemons-commands-test
      "Test: commands submodule"
      :test t
      :commands expected
      :headers []
      :list nil)
    (should (equal expected (daemons--commands-alist 'daemons-commands-test)))))


(ert-deftest daemons--command-test ()
  (let ((expected  (lambda () "let's go already")))
    (daemons-define-submodule daemons-command-test
      "Test: commands submodule"
      :test t
      :commands '((stop . (lambda () "plz stop"))
                    (start . (lambda () "let's go already")))
      :headers []
      :list nil)
    (should (equal expected (daemons--command 'start 'daemons-command-test)))))

(ert-deftest daemons--list-headers-test ()
  (let ((expected [("Daemon name" 80 t)
                   ("Status" 20 t)]))
    (daemons-define-submodule daemons-list-headers-test
      "Test: list headers submodule"
      :test t
      :commands nil
      :headers expected
      :list nil)
    (should (equal expected (daemons--list-headers 'daemons-list-headers-test)))))

(ert-deftest daemons--list-test ()
  (let ((expected '("foo" "bar" "baz")))
    (daemons-define-submodule daemons-list-test
      "Test: list submodule"
      :test t
      :commands nil
      :headers []
      :list expected)
    (should (equal expected (daemons--list 'daemons-list-test)))))

;; status line tests
(daemons-define-submodule daemons-status-line-test
  "Test: submodule with a status line"
  :test t
  :commands '((status-line . (lambda (name) (format "%s: up" name))))
  :headers [("Daemon name" 100 t)]
  :list '(("foo" ["foo"]) ("bar" ["bar"])))

(daemons-define-submodule daemons-no-status-line-test
  "Test: submodule without a status line"
  :test t
  :commands '((status . (lambda (name) (format "status %s" name))))
  :headers [("Daemon name" 100 t)]
  :list nil)

(defmacro daemons-test--with-submodule (submodule &rest body)
  "Run BODY with SUBMODULE as the init system and messages collected.
The messages are bound to `messages' in BODY."
  (declare (indent 1))
  `(let ((messages nil))
     (cl-letf (((symbol-function 'daemons-init-system-submodule) (lambda () ,submodule))
               ((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
       ,@body)))

(ert-deftest daemons-status-line-test ()
  "The status line is echoed and returned."
  (daemons-test--with-submodule 'daemons-status-line-test
    (should (equal "foo: up" (daemons-status-line "foo")))
    (should (equal '("foo: up") messages))))

(ert-deftest daemons-status-line-completes-daemon-names-test ()
  (daemons-test--with-submodule 'daemons-status-line-test
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (equal '("foo" "bar") (mapcar #'car collection)))
                 "bar")))
      (should (equal "bar: up" (call-interactively #'daemons-status-line)))
      (should (equal '("bar: up") messages)))))

(ert-deftest daemons-status-line-at-point-test ()
  (daemons-test--with-submodule 'daemons-status-line-test
    (with-temp-buffer
      (daemons-mode)
      (setq tabulated-list-entries (daemons--list 'daemons-status-line-test))
      (tabulated-list-print)
      (goto-char (point-min))
      (forward-line 1)
      (should (equal "bar: up" (call-interactively #'daemons-status-line-at-point)))
      (should (equal '("bar: up") messages)))))

(daemons-define-submodule daemons-status-line-dir-test
  "Test: submodule whose status line shows where it ran"
  :test t
  :commands '((status-line . (lambda (name)
                               (if (equal name "fail")
                                   (error "Status line failed")
                                 (concat name ": " default-directory)))))
  :headers [("Daemon name" 100 t)]
  :list nil)

(ert-deftest daemons-status-line-always-sudo-test ()
  "The status line runs as root with `daemons-always-sudo', like other commands.
The directory `daemons--sudo' makes is removed afterwards, also on error."
  (dolist (sudo '(t nil))
    (dolist (name '("foo" "fail"))
      (daemons-test--with-submodule 'daemons-status-line-dir-test
        (let* ((daemons-always-sudo sudo)
               (default-directory "/tmp/")
               (tempdir (file-name-as-directory (make-temp-file "daemons-test" t)))
               (sudo-dir (concat "/sudo::" tempdir)))
          (unwind-protect
              (cl-letf (((symbol-function 'daemons--sudo)
                         (lambda () (setq default-directory sudo-dir))))
                (if (equal name "fail")
                    (should-error (daemons-status-line name))
                  (should (equal (concat "foo: " (if sudo sudo-dir "/tmp/"))
                                 (daemons-status-line name))))
                (should (equal "/tmp/" default-directory))
                (should (eq (not sudo) (file-directory-p tempdir))))
            (when (file-directory-p tempdir) (delete-directory tempdir))))))))

(ert-deftest daemons-status-line-unsupported-test ()
  "An init system without a status line says so."
  (daemons-test--with-submodule 'daemons-no-status-line-test
    (let ((err (should-error (daemons-status-line "foo") :type 'user-error)))
      (should (equal "daemons-no-status-line-test does not support a one-line status"
                     (cadr err))))
    (should-not messages)))

(ert-deftest daemons-status-line-unsupported-before-prompt-test ()
  "M-x on an init system without a status line says so before prompting."
  (daemons-test--with-submodule 'daemons-no-status-line-test
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "prompted for a daemon name"))))
      (should-error (call-interactively #'daemons-status-line) :type 'user-error))))

;; journal tests
(defun daemons-test--journal (name)
  "Follow a stand-in journal of NAME: a cat process in the current buffer."
  (setq daemons-test--journal-calls (cons (cons name default-directory)
                                          daemons-test--journal-calls))
  (make-process :name "daemons-test-journal"
                :buffer (current-buffer)
                :command '("cat")
                :connection-type 'pipe
                :noquery t
                :filter #'daemons--journal-insert
                :sentinel #'daemons--journal-sentinel))

(defvar daemons-test--journal-calls nil
  "The (NAME . DIRECTORY) of each call of `daemons-test--journal', latest first.")

(defun daemons-test--wait-for-exit (process)
  "Wait for PROCESS to exit; fail the test if it still runs after 5 seconds."
  (with-timeout (5 (ert-fail (format "%s still running" process)))
    (while (process-live-p process) (accept-process-output process 0.1)))
  (accept-process-output nil 0.05))

(daemons-define-submodule daemons-journal-test
  "Test: submodule with a journal"
  :test t
  :commands '((journal . daemons-test--journal))
  :headers [("Daemon name" 100 t)]
  :list '(("foo" ["foo"]) ("bar" ["bar"])))

(defmacro daemons-test--with-journal (&rest body)
  "Run BODY with the journal test submodule; kill the journal buffers after."
  (declare (indent 0))
  `(let ((daemons-test--journal-calls nil)
         (daemons-journal-function nil)
         (daemons-always-sudo nil))
     (daemons-test--with-submodule 'daemons-journal-test
       (unwind-protect (progn ,@body)
         (dolist (buffer (buffer-list))
           (when (string-prefix-p "*daemons-journal " (buffer-name buffer))
             (kill-buffer buffer)))))))

(ert-deftest daemons-journal-test ()
  "The journal is followed in a buffer of its own for each daemon."
  (daemons-test--with-journal
    (let ((buffer (daemons-journal "foo")))
      (should (equal (daemons--get-journal-buffer-name "foo") (buffer-name buffer)))
      (should (string-prefix-p "*daemons-journal foo for " (buffer-name buffer)))
      (should (eq 'daemons-journal-mode (buffer-local-value 'major-mode buffer)))
      (should (process-live-p (get-buffer-process buffer)))
      (should (equal '("foo") (mapcar #'car daemons-test--journal-calls)))
      (should-not (eq buffer (daemons-journal "bar"))))))

(ert-deftest daemons-journal-buffer-remote-identity-test ()
  "Journal buffers differ by the method, user, host and port of a remote directory."
  (daemons-test--with-journal
    (let ((names (mapcar (lambda (dir)
                           (let ((default-directory dir))
                             (daemons--get-journal-buffer-name "foo")))
                         '("/ssh:me@example.com:/" "/scp:me@example.com:/"
                           "/ssh:you@example.com:/" "/ssh:me@example.org:/"
                           "/ssh:me@example.com#2222:/"))))
      (should (equal "*daemons-journal foo for /ssh:me@example.com#2222:*"
                     (car (last names))))
      (should (= 5 (length (delete-dups (copy-sequence names)))))
      (should (equal (car names)
                     (let ((default-directory "/ssh:me@example.com:/etc/"))
                       (daemons--get-journal-buffer-name "foo")))))))

(ert-deftest daemons-journal-buffer-per-sudo-test ()
  "With `daemons-always-sudo' a local journal has a buffer of its own, as root."
  (daemons-test--with-journal
    (let* ((default-directory "/tmp/")
           (user (let ((daemons-always-sudo nil)) (daemons--get-journal-buffer-name "foo")))
           (root (let ((daemons-always-sudo t)) (daemons--get-journal-buffer-name "foo"))))
      (should-not (equal user root))
      (should (equal (format "*daemons-journal foo for root@%s*" (system-name)) root))
      (let ((default-directory "/ssh:me@example.com:/"))
        (should (equal (let ((daemons-always-sudo nil)) (daemons--get-journal-buffer-name "foo"))
                       (let ((daemons-always-sudo t)) (daemons--get-journal-buffer-name "foo"))))))))

(ert-deftest daemons-journal-reuses-running-process-test ()
  "Asking again for a journal that is being followed shows the same buffer."
  (daemons-test--with-journal
    (let* ((buffer (daemons-journal "foo"))
           (process (get-buffer-process buffer)))
      (should (eq buffer (daemons-journal "foo")))
      (should (eq process (get-buffer-process buffer)))
      (should (= 1 (length daemons-test--journal-calls))))))

(ert-deftest daemons-journal-restarts-ended-process-test ()
  "A journal whose process ended is started afresh in an emptied buffer."
  (daemons-test--with-journal
    (let* ((buffer (daemons-journal "foo"))
           (process (get-buffer-process buffer)))
      (process-send-eof process)
      (daemons-test--wait-for-exit process)
      (should (string-match-p "-- daemons-test-journal finished --"
                              (with-current-buffer buffer (buffer-string))))
      (daemons-journal "foo")
      (should (process-live-p (get-buffer-process buffer)))
      (should (equal "" (with-current-buffer buffer (buffer-string))))
      (should (= 2 (length daemons-test--journal-calls))))))

(ert-deftest daemons-journal-kill-buffer-kills-process-test ()
  "No process is left over after `kill-buffer'."
  (daemons-test--with-journal
    (let* ((buffer (daemons-journal "foo"))
           (process (get-buffer-process buffer)))
      (should (process-live-p process))
      (kill-buffer buffer)
      (should-not (process-live-p process))
      (should-not (memq process (process-list))))))

(ert-deftest daemons-journal-scrolls-only-at-end-test ()
  "New output moves point and window point only if they were at the end."
  (daemons-test--with-journal
    (let* ((buffer (daemons-journal "foo"))
           (process (get-buffer-process buffer))
           (window (selected-window)))
      (set-window-buffer window buffer)
      (with-current-buffer buffer
        (daemons--journal-insert process "one\n")
        (should (= (point-max) (point)))
        (should (= (point-max) (window-point window)))
        (goto-char (point-min))
        (set-window-point window (point-min))
        (daemons--journal-insert process "two\n")
        (should (equal "one\ntwo\n" (buffer-string)))
        (should (= (point-min) (point)))
        (should (= (point-min) (window-point window)))
        (goto-char (point-max))
        (set-window-point window (point-max))
        (daemons--journal-insert process "three\n")
        (should (= (point-max) (point)))
        (should (= (point-max) (window-point window)))))))

(ert-deftest daemons-journal-failed-start-test ()
  "A journal that fails to start leaves no buffer or sudo directory behind."
  (daemons-test--with-journal
    (let* ((daemons-always-sudo t)
           (default-directory "/tmp/")
           (tempdir (file-name-as-directory (make-temp-file "daemons-test" t))))
      (unwind-protect
          (cl-letf (((symbol-function 'daemons--sudo)
                     (lambda () (setq default-directory (concat "/sudo::" tempdir))))
                    ((symbol-function 'daemons-test--journal)
                     (lambda (_name) (error "No journalctl"))))
            (should-error (daemons-journal "foo"))
            (should-not (get-buffer (daemons--get-journal-buffer-name "foo")))
            (should-not (file-directory-p tempdir)))
        (when (file-directory-p tempdir) (delete-directory tempdir))))))

(ert-deftest daemons-journal-function-test ()
  "`daemons-journal-function' is called with the daemon name instead."
  (daemons-test--with-journal
    (let* ((names nil)
           (daemons-journal-function (lambda (name) (push name names) 'shown)))
      (should (eq 'shown (daemons-journal "foo")))
      (should (equal '("foo") names))
      (should-not daemons-test--journal-calls)
      (should-not (seq-find (lambda (b) (string-prefix-p "*daemons-journal " (buffer-name b)))
                            (buffer-list))))))

(ert-deftest daemons-journal-at-point-test ()
  (daemons-test--with-journal
    (with-temp-buffer
      (daemons-mode)
      (setq tabulated-list-entries (daemons--list 'daemons-journal-test))
      (tabulated-list-print)
      (goto-char (point-min))
      (forward-line 1)
      (call-interactively #'daemons-journal-at-point)
      (should (equal '("bar") (mapcar #'car daemons-test--journal-calls))))))

(ert-deftest daemons-journal-completes-daemon-names-test ()
  (daemons-test--with-journal
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (equal '("foo" "bar") (mapcar #'car collection)))
                 "bar")))
      (call-interactively #'daemons-journal)
      (should (equal '("bar") (mapcar #'car daemons-test--journal-calls))))))

(ert-deftest daemons-journal-always-sudo-test ()
  "With `daemons-always-sudo' the journal follows as root, like other commands.
The directory `daemons--sudo' makes is reused for the buffer and removed with it."
  (dolist (sudo '(t nil))
    (daemons-test--with-journal
      (let* ((daemons-always-sudo sudo)
             (default-directory "/tmp/")
             (tempdir (file-name-as-directory (make-temp-file "daemons-test" t)))
             (sudo-dir (concat "/sudo::" tempdir))
             (sudo-calls 0))
        (unwind-protect
            (cl-letf (((symbol-function 'daemons--sudo)
                       (lambda ()
                         (setq sudo-calls (1+ sudo-calls))
                         (unless (string-prefix-p "/sudo::" default-directory)
                           (setq default-directory sudo-dir)))))
              (let ((buffer (save-current-buffer (daemons-journal "foo"))))
                (should (equal (if sudo sudo-dir "/tmp/") (cdar daemons-test--journal-calls)))
                (should (equal "/tmp/" default-directory))
                (let ((process (get-buffer-process buffer)))
                  (process-send-eof process)
                  (daemons-test--wait-for-exit process))
                (save-current-buffer (daemons-journal "foo"))
                (should (equal (if sudo sudo-dir "/tmp/") (cdar daemons-test--journal-calls)))
                (should (= (if sudo 1 0) sudo-calls))
                (should (file-directory-p tempdir))
                (kill-buffer buffer)
                (should (eq (not sudo) (file-directory-p tempdir)))))
          (when (file-directory-p tempdir) (delete-directory tempdir)))))))

(ert-deftest daemons-journal-function-without-sudo-test ()
  "`daemons-journal-function' runs in the caller's directory, even with `daemons-always-sudo'."
  (dolist (sudo '(t nil))
    (daemons-test--with-journal
      (let* ((daemons-always-sudo sudo)
             (default-directory "/tmp/")
             (dirs nil)
             (daemons-journal-function (lambda (_name) (push default-directory dirs))))
        (cl-letf (((symbol-function 'daemons--sudo)
                   (lambda () (ert-fail "daemons--sudo was called"))))
          (daemons-journal "foo")
          (should (equal '("/tmp/") dirs)))))))

(ert-deftest daemons-journal-function-without-journal-command-test ()
  "`daemons-journal-function' is called on an init system with no journal command."
  (daemons-test--with-submodule 'daemons-no-status-line-test
    (let* ((names nil)
           (daemons-journal-function (lambda (name) (push name names))))
      (daemons-journal "foo")
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "bar")))
        (call-interactively #'daemons-journal))
      (should (equal '("bar" "foo") names)))))

(ert-deftest daemons-journal-unsupported-test ()
  "An init system without a journal says so."
  (daemons-test--with-submodule 'daemons-no-status-line-test
    (let ((err (should-error (daemons-journal "foo") :type 'user-error)))
      (should (equal "daemons-no-status-line-test does not support a journal"
                     (cadr err))))))

(ert-deftest daemons-journal-unsupported-before-prompt-test ()
  "M-x on an init system without a journal says so before prompting."
  (daemons-test--with-submodule 'daemons-no-status-line-test
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "prompted for a daemon name"))))
      (should-error (call-interactively #'daemons-journal) :type 'user-error))))

;; system-specific tests
(dolist (test-suite (directory-files "." t "daemons-.*-test\.el$"))
  (load-file test-suite))

;; helper function tests
(ert-deftest daemons--split-lines-test ()
  (should (equal '("this" "is" "a" "test") (daemons--split-lines "this
is

a


test
"))))

(ert-deftest daemons--get-user-and-hostname-test-local ()
  (should (equal (format "%s@%s" (user-login-name) (system-name))
                 (daemons--get-user-and-hostname "/tmp/file"))))

(ert-deftest daemons--get-user-and-hostname-test-sudo ()
  "Case where user is using tramp to access local system as root."
  (should (equal (format "root@%s" (system-name))
                 (daemons--get-user-and-hostname "/sudo::"))))

(ert-deftest daemons--get-user-and-hostname-test-remote ()
  "Case where user is using tramp to access a remote system."
  (should (equal "me@example.com"
                 (daemons--get-user-and-hostname "/ssh:me@example.com:/etc/issue"))))

(ert-deftest daemons--get-user-and-hostname-test-remote-no-user ()
  "Case where user can omit username because it's in their ssh config is same as current."
  (should (equal "example.com"
                 (daemons--get-user-and-hostname "/ssh:example.com:/etc/issue"))))

(ert-deftest daemons--get-user-and-hostname-test-remote-no-host ()
  "Case where user is ssh-ing into own system as different user."
  (should (equal (format "me@%s" (system-name))
                 (daemons--get-user-and-hostname "/ssh:me@:/etc/issue"))))

(ert-deftest daemons--get-user-and-hostname-test-remote-no-user-or-host ()
  "Case where user is ssh-ing into own system."
  (should (equal (system-name)
                 (daemons--get-user-and-hostname "/ssh::/etc/issue"))))

(ert t)

;; Test teardown - we added a dummy submodule
(setq daemons-init-system-submodules
      (remove 'daemons-run-test daemons-init-system-submodules))
