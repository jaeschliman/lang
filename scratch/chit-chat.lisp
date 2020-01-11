(set '*package* (symbol-package 'define))

(define %message-package (%make-package "messages"))
(package-add-subpackage %root-package %message-package "st")

(define (as-message-send charlist) (intern (charlist-to-string charlist) %message-package))

(define (rulename rule) (intern (symbol-name rule) %meta-package))

(define (compose-msg-send rcvr parts)
  (let ((msg (as-message-send (reduce-list (lambda (acc pair)
                                             (append acc (car pair)))
                                           '() parts)))
        (args (mapcar cdr parts)))
    `(send ',msg ,rcvr ,@args)))

(define (compose-hdr parts)
  (let ((msg (as-message-send (reduce-list (lambda (acc pair)
                                     (append acc (car pair)))
                                   '() parts)))
        (args (mapcar cdr parts)))
    (list :name msg :args args)))

(import-file! "./scratch/chit-chat.grammar.lisp")
(import-file! "./scratch/chit-chat.compiler.lisp")

