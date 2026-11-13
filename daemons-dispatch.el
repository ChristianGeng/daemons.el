;;; daemons-dispatch.el --- One menu per daemon for daemons.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Package-Requires: ((emacs "28.1"))
;;
;;; Commentary:
;; `daemons-dispatch' opens a transient menu for one daemon: its one-line
;; status as the header, then every action the init system supports.
;; `RET' in the daemons list opens it when transient is available (see
;; `daemons-dispatch-on-ret').  transient is built into Emacs 28 and later;
;; without it this file is not loaded and the rest of daemons works as
;; before.

;;; Code:
(require 'daemons)
(require 'transient)

(defvar daemons--dispatch-name nil
  "The daemon the menu of `daemons-dispatch' is for.")

(defvar daemons--dispatch-submodule nil
  "The init system submodule of the menu, found once when it opens.
Finding it runs the submodules' tests, which on a remote host each
cost a round trip.")

(defun daemons--dispatch-supported-p (command)
  "Return non-nil if the init system has COMMAND, e.g. \\='journal."
  (daemons--command command (or daemons--dispatch-submodule
                                (daemons-init-system-submodule))))

(defun daemons--dispatch-header ()
  "Return the header of the menu: the daemon's one-line status, if any.
It is computed each time the menu is drawn, so after a start or stop the
menu shows the new state."
  (let ((status-line (daemons--dispatch-supported-p 'status-line))
        (name daemons--dispatch-name))
    ;; Not propertized: that would hide the status line's own colours,
    ;; and transient gives a plain description its heading face.
    (if status-line
        (condition-case err
            ;; As `daemons-status-line' does it, as root with
            ;; `daemons-always-sudo', but without its message.
            (let ((inhibit-message t)
                  (message-log-max nil))
              (daemons-status-line name))
          (error (format "%s: %s" name (error-message-string err))))
      name)))

(defmacro daemons--define-dispatch-suffix (command function &optional stay)
  "Define the menu command for COMMAND that calls FUNCTION on the daemon.
With STAY the menu stays open, and its header is redrawn."
  (let ((name (intern (format "daemons-dispatch--%s" command))))
    `(transient-define-suffix ,name ()
       ,(format "Call `%s' on the daemon of the menu." function)
       ,@(when stay '(:transient t))
       :if (lambda () (daemons--dispatch-supported-p ',command))
       (interactive)
       (,function daemons--dispatch-name))))

(daemons--define-dispatch-suffix start daemons-start t)
(daemons--define-dispatch-suffix stop daemons-stop t)
(daemons--define-dispatch-suffix restart daemons-restart t)
(daemons--define-dispatch-suffix reload daemons-reload t)
(daemons--define-dispatch-suffix enable daemons-enable t)
(daemons--define-dispatch-suffix disable daemons-disable t)
(daemons--define-dispatch-suffix status daemons-status)
(transient-define-suffix daemons-dispatch--journal ()
  "Call `daemons-journal' on the daemon of the menu."
  ;; `daemons-journal-function' works also without a journal command.
  :if (lambda () (or daemons-journal-function
                     (daemons--dispatch-supported-p 'journal)))
  (interactive)
  (daemons-journal daemons--dispatch-name))
(daemons--define-dispatch-suffix edit daemons-edit)

(transient-define-prefix daemons-dispatch-menu ()
  "Menu of the actions for one daemon; see `daemons-dispatch'."
  [:description daemons--dispatch-header
   ["Control"
    ("s" "start" daemons-dispatch--start)
    ("S" "stop" daemons-dispatch--stop)
    ("R" "restart" daemons-dispatch--restart)
    ("r" "reload" daemons-dispatch--reload)
    ("e" "enable" daemons-dispatch--enable)
    ("d" "disable" daemons-dispatch--disable)]
   ["Inspect"
    ("i" "status" daemons-dispatch--status)
    ("j" "journal" daemons-dispatch--journal)]
   ["Edit"
    ("E" "unit file" daemons-dispatch--edit)]])

;;;###autoload
(defun daemons-dispatch (name)
  "Open the menu of actions for the daemon NAME.
Its header is the daemon's one-line status; it offers only the actions
the init system supports, with the keys of the daemons list.  Control
actions keep the menu open and redraw the header."
  (interactive (list (daemons--completing-read)))
  (setq daemons--dispatch-name name
        daemons--dispatch-submodule (daemons-init-system-submodule))
  (daemons-dispatch-menu))

;;;###autoload
(defun daemons-dispatch-at-point (name)
  "Open the menu of actions for the daemon NAME at point."
  (interactive (list (daemons--daemon-at-point)))
  (daemons-dispatch name))

(provide 'daemons-dispatch)
;;; daemons-dispatch.el ends here
