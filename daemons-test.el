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
