;;; deploy-framework.el --- Framework for building and deploying your code

;; Copyright (C) 2014, Vadim Radovel <vradovel@croc.ru>

;; Author: Vadim Radovel <vradovel@croc.ru>
;; URL: https://github.com/NightBlues/deploy-framework
;; Version: 0.0.2

;;; Commentary:
;; Provides easy to write rules for building and deploying your project.
;; Example:
;; Declare profile in your .emacs or init.el:
;;
;; (df-profile 
;;  "my1"
;;  (df-run-local
;;    (df-make-command "~/devel/" :make-args "no-lint"))
;;  (df-copy-files 
;;    "root" "test.com" "22"
;;    ("~/devel/ "/root/devel"))
;;  (df-run-remote
;;    "root" "test.com" "22"
;;    "fuser -n tcp 8000 -k"
;;    "service httpd restart"))
;; 
;; Then M-x deploy and type my1 RET

;;; Code:
(eval-when-compile (require 'cl))

(defvar df-profiles '() "Collection of all available profiles.")
(defconst df-log-buffer "*deployment*" "Name of the buffer that uses for store commands output.")

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

(defmacro df-message (msg)
  `(progn
     (message ,msg)
     (with-current-buffer (get-buffer-create df-log-buffer)
       (insert (concat "\n" ,msg "\n")))))

;; Profile generators
(cl-defmacro df-copy-files (user host port &body files)
  "Generate code that runs commands local"
  `(list ,@(let ((res nil))
             (dolist (f files (reverse res))
               (add-to-list 'res (df-rsync-command :user user :host host :port port :src (car f) :dst (cadr f)))))))

(cl-defmacro df-run-local (&body commands)
  "Generate code that runs commands local"
  `(list ,@(let ((res nil))
             (dolist (c commands (reverse res))
               (add-to-list 'res c)))))

(cl-defmacro df-run-remote (user host port &body commands)
  "Generate code that runs commands remote"
  `(list ,@(let ((res nil)
                 (ssh-prefix (get-ssh-prefix user host port)))
             (dolist (c commands (reverse res))
               (add-to-list 'res (concat ssh-prefix c))))))

;; Main functions
(cl-defun df-shell-command (command)
  "Run shell command"
  (df-message (format "Running command: %s in dir %s" command default-directory))
  (unless (= 0 (call-process-shell-command command nil
                                           (get-buffer-create df-log-buffer)))
    (throw 'command-failed nil)))

(cl-defun df-run (commands)
  "Run given commands"
  (catch 'command-failed
    (dolist (c commands)
      (if (consp c)
          (let ((default-directory (car c)))
            (df-shell-command (cdr c)))
        (df-shell-command c)))
    t))

(cl-defun df-profile (name &rest commands)
  "Add profile"
  (add-to-list 'df-profiles (cons name (apply #'append commands))))

(defun deploy (profile)
  "Run deploy process"
  (interactive (list (completing-read "Deploy profile: " (mapcar #'car df-profiles))))
  (df-message (format "Deploying profile: %s ..." profile))
  (if (df-run (cdr (assoc profile df-profiles)))
      (df-message "Deployment successfull.")
    (progn
      (df-message "Deployment failed.")
      (switch-to-buffer df-log-buffer))))

(provide 'deploy-framework)
;;; deploy-framework.el ends here
