;;; deploy-framework.el --- Framework for building and deploying your code

;; Copyright (C) 2014, Vadim Radovel <vradovel@croc.ru>

;; Author: Vadim Radovel <vradovel@croc.ru>
;; URL: https://github.com/NightBlues/deploy-framework
;; Version: 0.0.1

;;; Commentary:
;; Provides easy to write rules for building and deploying your project.
;; Example:
;; (defun df-deploy-my ()
;;   (interactive)
;;   (df-run-local
;;    (df-make-command "~/devel/" :make-args "no-lint"))
;;   (df-copy-files 
;;    "root" "test.com" "37017"
;;    (cons "~/devel/" "/root/"))
;;   (df-run-remote
;;    "root" "test.com" "37017"
;;    "killall devel.sh"
;;    "screen -d -m python /root/devel/devel.sh")
;;   (message "Deploy finished."))

;;; Code:
(eval-when-compile (require 'cl))

(defvar df-profiles '())

;; Helpers
(cl-defun df-scp-command (&key user host port src dst)
  (format "scp -P %s %s %s@%s:%s" port src user host dst))

(cl-defun df-rsync-command (&key (user "root") (host "localhost") (port "22") src dst)
  (format "rsync -avz --exclude='.git' --exclude='*.pyc' --exclude='*.pyo' -e 'ssh -p %s' %s %s@%s:%s" port src user host dst))

(cl-defun df-make-command (dir &key (make-args))
  (cons dir (format "make %s" make-args)))

(cl-defun df-killall-command (process-name &optional (kill-args ""))
  (format "killall %s %s" kill-args process-name))

(cl-defun get-ssh-prefix (user host port)
  (format "ssh %s@%s -p %s " user host port))

(defun get-hash-keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys))

;; Profile generators
(cl-defmacro df-copy-files (user host port &body files)
  "Generate code that runs commands local"
  `(list ,@(let ((res nil))
             (dolist (f files (reverse res))
               (setq res
                     (cons
                      (df-rsync-command :user user :host host :port port :src (car f) :dst (cdr f))
                      res))))))

(cl-defmacro df-run-local (&body commands)
  "Generate code that runs commands local"
  `(list ,@(let ((res nil))
             (dolist (c commands (reverse res))
               (setq res (cons c res))))))

(cl-defmacro df-run-remote (user host port &body commands)
  "Generate code that runs commands remote"
  `(list ,@(let ((res nil)
                 (ssh-prefix (get-ssh-prefix user host port)))
             (dolist (c commands (reverse res))
               (setq res (cons (concat ssh-prefix c) res))))))

;; Main functions
(cl-defun df-shell-command (command)
  "Run shell command"
  (message (format "\nRunning command: %s in dir %s" command default-directory))
  (call-process-shell-command command nil "*Messages*"))

(cl-defun df-run (commands)
  "Run given commands"
  (dolist (c commands)
    (if (consp c)
        (let ((default-directory (car c)))
          (df-shell-command (cdr c)))
      (df-shell-command c))))

(cl-defun df-profile (name &rest commands)
  "Add profile"
  (add-to-list 'df-profiles (cons name (apply #'append commands))))

(defun deploy (profile)
  "Run deploy process"
  (interactive (list (completing-read "Deploy profile: " (mapcar #'car df-profiles))))
  (df-run (assoc profile df-profiles))
  (message "Deploy finished."))

(provide 'deploy-framework)
;;; deploy-framework.el ends here
