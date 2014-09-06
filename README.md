deploy-framework
===========

Emacs lisp framework that helps you deploy your applications onto test stand.

Provides easy to write rules for building and deploying your project.
Example:

Declare profile in your .emacs or init.el:

    (df-profile 
     "my1"
     (df-run-local
       (df-make-command "~/devel/" :make-args "no-lint"))
     (df-copy-files 
       "root" "test.com" "22"
       ("~/devel/ "/root/devel"))
     (df-run-remote
       "root" "test.com" "22"
       "fuser -n tcp 8000 -k"
       "service httpd restart"))
 
Then **M-x** deploy and type *my1* **RET**
