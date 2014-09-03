;;; deploy-mode.el --- Framework for building and deploying your code

;; Copyright (C) 2014, Vadim Radovel <vradovel@croc.ru>

;; Author: Vadim Radovel <vradovel@croc.ru>
;; URL: https://github.com/NightBlues/deploy-mode
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

(cl-defmacro df-shell-command (command)
  `(progn (message (format "Running command: %s in dir %s" ,command default-directory))
          ;; (shell-command ,command "*Messages*")
          (call-process-shell-command ,command nil "*Messages*")))
          ;; (with-temp-buffer
          ;;   (shell-command-on-region (point-min) (point-max) ,command t)
          ;;   (append-to-buffer *Messages* (point-min) (point-max)))))

(cl-defmacro df-copy-files (user host port &body files)
  "Generates code that deploys files"
  (let ((f (gensym)))
    `(dolist (,f (list ,@files))
         (df-shell-command (df-rsync-command :user ,user :host ,host :port ,port :src (car ,f) :dst (cdr ,f))))))

(cl-defmacro df-run-local (&body commands)
  (let ((c (gensym)))
    `(dolist (,c (list ,@commands))
       (if (consp ,c)
           (let ((default-directory (car ,c)))
             (df-shell-command (cdr ,c)))
           (df-shell-command ,c)))))

(cl-defmacro df-run-remote (user host port &body commands)
  (let ((c (gensym))
        (ssh-prefix (get-ssh-prefix user host port)))
    `(dolist (,c (list ,@commands))
       (if (consp ,c)
           (let ((default-directory (car ,c)))
             (df-shell-command (concat ,ssh-prefix (cdr ,c))))
           (df-shell-command (concat ,ssh-prefix ,c))))))


(provide 'df-scp-command)
(provide 'df-rsync-command)
(provide 'df-make-command)
(provide 'df-run-local)
(provide 'df-copy-files)
(provide 'df-run-remote)
;;; deploy-mode.el ends here
