(load-file "./daemons-dispatch.el")

(defvar daemons-dispatch-test--calls nil
  "The daemon commands the dispatch tests saw, newest first.")

(daemons-define-submodule daemons-dispatch-test
  "Test: submodule for the dispatch menu"
  :test t
  :commands '((start . (lambda (name) (format "start %s" name)))
              (stop . (lambda (name) (format "stop %s" name)))
              (status . (lambda (name) (format "status %s" name)))
              (status-line . (lambda (name) (format "%s: up" name))))
  :headers [("Daemon name" 20 t)]
  :list '(("foo" ["foo"])))

(defmacro daemons-dispatch-test--with-submodule (&rest body)
  "Run BODY with the dispatch test submodule and daemon commands recorded."
  (declare (indent 0))
  `(let ((daemons-dispatch-test--calls nil)
         (daemons--dispatch-submodule nil)
         (daemons-journal-function nil))
     (cl-letf (((symbol-function 'daemons-init-system-submodule)
                (lambda () 'daemons-dispatch-test))
               ((symbol-function 'daemons-start)
                (lambda (name) (push (list 'start name) daemons-dispatch-test--calls)))
               ((symbol-function 'daemons-status)
                (lambda (name) (push (list 'status name) daemons-dispatch-test--calls))))
       ,@body)))

(ert-deftest daemons-dispatch-opens-menu-for-daemon-test ()
  "Both entry points open the same menu for the daemon."
  (daemons-dispatch-test--with-submodule
    (let ((opened 0))
      (cl-letf (((symbol-function 'daemons-dispatch-menu) (lambda () (cl-incf opened)))
                ((symbol-function 'completing-read) (lambda (&rest _) "foo")))
        (call-interactively #'daemons-dispatch)
        (should (equal "foo" daemons--dispatch-name))
        (with-temp-buffer
          (setq-local daemons--current-id "bar")
          (call-interactively #'daemons-dispatch-at-point))
        (should (equal "bar" daemons--dispatch-name))
        (should (= 2 opened))))))

(ert-deftest daemons-dispatch-header-test ()
  "The header is the one-line status, or the name without one."
  (daemons-dispatch-test--with-submodule
    (let ((daemons--dispatch-name "foo"))
      (should (equal "foo: up" (substring-no-properties (daemons--dispatch-header))))
      (cl-letf (((symbol-function 'daemons-init-system-submodule)
                 (lambda () 'daemons-no-status-line-test)))
        (should (equal "foo" (substring-no-properties (daemons--dispatch-header))))))))

(ert-deftest daemons-dispatch-header-error-test ()
  "A failing status line shows in the header instead of breaking the menu."
  (cl-letf (((symbol-function 'daemons-init-system-submodule) (lambda () 'daemons-dispatch-test))
            ((symbol-function 'daemons--command)
             (lambda (_command _submodule) (lambda (_name) (error "No bus")))))
    (let ((daemons--dispatch-name "foo"))
      (should (equal "foo: No bus" (substring-no-properties (daemons--dispatch-header)))))))

(ert-deftest daemons-dispatch-suffixes-call-commands-test ()
  "Each menu entry calls its daemon command on the menu's daemon."
  (daemons-dispatch-test--with-submodule
    (let ((daemons--dispatch-name "foo"))
      (call-interactively #'daemons-dispatch--start)
      (call-interactively #'daemons-dispatch--status)
      (should (equal '((status "foo") (start "foo")) daemons-dispatch-test--calls)))))

(ert-deftest daemons-dispatch-offers-only-supported-commands-test ()
  "Entries the init system lacks are hidden; control entries keep the menu open."
  (daemons-dispatch-test--with-submodule
    (dolist (case '((daemons-dispatch--start . t) (daemons-dispatch--stop . t)
                    (daemons-dispatch--status . t) (daemons-dispatch--restart . nil)
                    (daemons-dispatch--journal . nil) (daemons-dispatch--edit . nil)))
      (let ((suffix (get (car case) 'transient--suffix)))
        (should suffix)
        (should (eq (cdr case) (and (funcall (oref suffix if)) t)))))
    (should (eq t (oref (get 'daemons-dispatch--start 'transient--suffix) transient)))
    (should-not (slot-boundp (get 'daemons-dispatch--status 'transient--suffix) 'transient))))

(ert-deftest daemons-dispatch-menu-has-every-action-test ()
  "The menu lists every action, with the keys of the daemons list."
  (let ((keys nil))
    ;; Find every plist with a :key, wherever this transient version keeps it.
    (cl-labels ((walk (x) (cond ((and (consp x) (keywordp (car x)) (plist-get x :key))
                                 (push (cons (plist-get x :key) (plist-get x :command))
                                       keys))
                                ((vectorp x) (mapc #'walk x))
                                ((consp x) (mapc #'walk x)))))
      (walk (get 'daemons-dispatch-menu 'transient--layout)))
    (dolist (expected '(("s" . daemons-dispatch--start) ("S" . daemons-dispatch--stop)
                        ("R" . daemons-dispatch--restart) ("r" . daemons-dispatch--reload)
                        ("e" . daemons-dispatch--enable) ("d" . daemons-dispatch--disable)
                        ("i" . daemons-dispatch--status) ("j" . daemons-dispatch--journal)
                        ("E" . daemons-dispatch--edit)))
      (should (member expected keys)))))

(ert-deftest daemons-ret-opens-dispatch-or-status-test ()
  "RET opens the menu, or the status with `daemons-dispatch-on-ret' off."
  (let ((calls nil))
    (cl-letf (((symbol-function 'daemons-dispatch) (lambda (name) (push (list 'menu name) calls)))
              ((symbol-function 'daemons-status-at-point) (lambda (name) (push (list 'status name) calls))))
      (should (eq 'daemons-dispatch-or-status-at-point (lookup-key daemons-mode-map (kbd "RET"))))
      (daemons-dispatch-or-status-at-point "foo")
      (let ((daemons-dispatch-on-ret nil))
        (daemons-dispatch-or-status-at-point "foo"))
      (should (equal '((status "foo") (menu "foo")) calls)))))

(ert-deftest daemons-dispatch-journal-with-custom-viewer-test ()
  "j is offered with `daemons-journal-function' even without a journal command."
  (daemons-dispatch-test--with-submodule
    (let ((suffix (get 'daemons-dispatch--journal 'transient--suffix)))
      (should-not (funcall (oref suffix if)))
      (let ((daemons-journal-function #'ignore))
        (should (funcall (oref suffix if)))))))

(ert-deftest daemons-dispatch-header-keeps-status-colours-test ()
  (cl-letf (((symbol-function 'daemons-init-system-submodule) (lambda () 'daemons-dispatch-test))
            ((symbol-function 'daemons--command)
             (lambda (_command _submodule) (lambda (name) (propertize (concat name ": failed") 'face 'error)))))
    (let ((daemons--dispatch-name "foo"))
      (should (eq 'error (get-text-property 0 'face (daemons--dispatch-header)))))))

(ert-deftest daemons-dispatch-finds-init-system-once-test ()
  "Opening the menu finds the init system once, not once per entry."
  (let ((found 0))
    (cl-letf (((symbol-function 'daemons-init-system-submodule)
               (lambda () (cl-incf found) 'daemons-dispatch-test))
              ((symbol-function 'daemons-dispatch-menu)
               (lambda ()
                 (dolist (command '(start stop status journal edit))
                   (daemons--dispatch-supported-p command)))))
      (daemons-dispatch "foo")
      (should (= 1 found)))))

(ert-deftest daemons-ret-without-transient-shows-status-test ()
  "RET falls back to the status when daemons-dispatch cannot load."
  (let ((calls nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _)
                 (if (eq feature 'daemons-dispatch) (error "Cannot open load file: transient")
                   feature)))
              ((symbol-function 'daemons-status-at-point) (lambda (name) (push name calls))))
      (daemons-dispatch-or-status-at-point "foo")
      (should (equal '("foo") calls)))))

(ert-deftest daemons-dispatch-header-runs-like-status-line-test ()
  "The header comes from `daemons-status-line', so it runs as root with sudo."
  (daemons-dispatch-test--with-submodule
    (let ((daemons--dispatch-name "foo")
          (called nil))
      (cl-letf (((symbol-function 'daemons-status-line)
                 (lambda (name) (setq called name) "foo: as root")))
        (should (equal "foo: as root" (daemons--dispatch-header)))
        (should (equal "foo" called))))))

(ert-deftest daemons-output-ret-shows-status-test ()
  "RET in the output buffer shows the status again, not the menu."
  (should (eq 'daemons-status-at-point (lookup-key daemons-output-mode-map (kbd "RET"))))
  (should (eq 'daemons-dispatch-or-status-at-point (lookup-key daemons-mode-map (kbd "RET")))))

(ert-deftest daemons-dispatch-header-does-not-probe-again-test ()
  "Redrawing the header reuses the init system found when the menu opened."
  (let ((found 0))
    (cl-letf (((symbol-function 'daemons-init-system-submodule)
               (lambda () (cl-incf found) 'daemons-dispatch-test))
              ((symbol-function 'message) #'ignore))
      (let ((daemons--dispatch-name "foo")
            (daemons--dispatch-submodule 'daemons-dispatch-test)
            (daemons-always-sudo nil))
        (should (equal "foo: up" (substring-no-properties (daemons--dispatch-header))))
        (daemons--dispatch-header)
        (should (= 0 found))))))
