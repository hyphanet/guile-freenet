#!/usr/bin/env bash
# -*- scheme -*-
# A Freenet Client Protocol library for Guile Scheme.

exec -a "${0}" guile -L $(dirname $(realpath "$0")) -e '(fcp-example)' -c '' "${@}"
; !#

;; for emacs (defun test-this-file () (interactive) (save-current-buffer) (async-shell-command (concat (buffer-file-name (current-buffer)) " --test")))


(define-module (fcp-example)
    #:export (main))

(import
    (only (fcp) message-create message-task message-type message-data message-fields
             message-client-get  message-client-get-realtime message-client-get-bulk 
             message-client-put message-client-put-realtime message-client-put-bulk 
             message-remove-request
             send-message processor-put! processor-delete!
             printing-passthrough-processor printing-discarding-processor 
             discarding-processor processor-nodehello-printer
             processor-datafound-getdata
             node-ip-set! node-port-set!
             task-id
             call-with-fcp-connection with-fcp-connection)
    (only (ice-9 pretty-print) pretty-print)
    (only (srfi srfi-1) first second third assoc)
    (only (srfi srfi-26) cut)
    (srfi srfi-37 );; commandline handling
    (only (rnrs bytevectors) string->utf8 utf8->string))


(define (request-successful-upload message)
    "When the put succeeds, download the data."
    (if (equal? 'PutSuccessful (message-type message))
         (let ((fields (message-fields message)))
           (when (and=> (assoc 'URI fields) (λ (uri-cel) (equal? key (cdr uri-cel))))
                  (send-message
                       (message-client-get-realtime get-task key)))
           #f)
         message))

(define (record-successful-download message)
    "When the download succeeds, display the result"
    (if (equal? 'AllData (message-type message))
         (let ((task (message-task message)))
           (when (equal? task get-task)
                  (format #t "Received Message: ~a\n" (utf8->string (message-data message)))
                  (set! successful #t))
           #f)
         message))

(define (remove-successful-tasks-from-queue message)
  "Cleanup the task because we use the global queue for easier debugging"
  (when (member (message-type message) '(AllData PutSuccessful))
         (send-message (message-remove-request (message-task message))))
  message)

(define put-task (task-id))
(define get-task (task-id))
(define key (string-append "KSK@" put-task))
(define successful #f)

(define (setup-handlers)
  ;; standard processors
  (processor-put! printing-discarding-processor)
  (processor-put! processor-nodehello-printer)
  ;; immediately request data from successfull get requests
  (processor-put! processor-datafound-getdata)
  ;; custom processors
  (processor-put! request-successful-upload)
  (processor-put! record-successful-download)
  (processor-put! remove-successful-tasks-from-queue))


;; commandline handling via srfi-37
(define options
    (list
        (option '(#\V "version") #f #f
            (λ (opt name args loads)
                (display "Guile FCP version 0.2\n")
                (quit)))
        (option '(#\h "help") #f #f
            (λ (opt name args loads)
                (format #t "Usage: ~a [options] 
                
Options: 
    -h --help                                 show this dialog
    -V --version                              show the version
     -H IP_OR_HOSTNAME --host=IP_OR_HOSTNAME  set the node address
    -P PORT --port=PORT                       set the FCP port
" (car (program-arguments)))
                (quit)))
        (option '(#\P "port") #t #f
            (λ (opt name arg loads)
                (node-port-set! arg)
                loads))
        (option '(#\H "host") #t #f
            (λ (opt name arg loads)
                (node-ip-set! arg)
                loads))))


(define (main args)
  (define arguments
    (args-fold (cdr args)
              options
              (lambda (opt name arg loads)
                (error "Unrecognized option `~A'" name))
              (lambda (op loads) (cons op loads))
              '()))
  (setup-handlers)
  ;; open the FCP connection. Anything inside this scope can
  ;; communicate directly with Freenet via FCP, other interaction
  ;; must be done through processing procedures as setup above.
  (with-fcp-connection
      ;; get the ball rolling
      (send-message
          (message-client-put-realtime put-task key
              (string->utf8 (string-append "Hello " key))))
      (while (not successful)
          (display ".")
          (sleep 1))))


