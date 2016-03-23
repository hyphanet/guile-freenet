#lang racket/base

;; Copyright (c) 2015 dinky's evil twin sone://EWtk1limedjBM2LnGE3~z98tC8bLTu9ryLIMcFgg8PI
;; License: LGPL

(require
  racket/pretty
  racket/tcp  
  racket/string)

;; note: data/skip-list takes hella long to load, so use alists
;; for better performance ordered dicts!

(displayln 'OK)
(define (startup-time)  
  (displayln (exact->inexact (current-process-milliseconds)))
  (flush-output)
  (exit))
;; (startup-time)

;; racket/port takes 400ms to load!
(define (copy-port in out)
  (let ((buf (make-bytes #x1000)))
    (let loop ()
      (let ((amt (read-bytes! buf in)))
        (when (not (eof-object? amt))
          (write-bytes buf out)
          (loop))))))

(define (find-identifier name opts)
  (let ((identifier (assq 'Identifier opts)))
    (if identifier
        (string->symbol (car identifier))
        name)))

(define (fcp-loop app)    
  (define waiters (make-immutable-hash))
  (define aliases (make-immutable-hash))
  (define in (current-input-port))
  (define out (current-output-port))
  
  (define data-buf (make-bytes #x1000))   
  
  (define (write-line s)
    (write-string s out)
    (newline out))
  
  (define (send name opts (data #f) (data-length 0))
    (write-line (symbol->string name))
    (for-each    
     (λ (pair)
       (let ((name (car pair))
             (value (cdr pair)))
         (write-line (string-append (symbol->string name)
                                    "=" (cond
                                          ((symbol? value) (symbol->string value))
                                          ((string? value) value)
                                          ((bytes? value) (bytes->string/utf-8 value))
                                          ((number? value) (number->string value))
                                          ((eq? value #f) "false")
                                          ((eq? value #t) "true")
                                          (else
                                           (error "wat is ~s" value)))))))
     opts)
    (if data
        (begin
          (write-line (string-append "Data-Length=" (number->string data-length)))
          (write-line "Data")
          (cond 
            ((procedure? data)
             (data (λ (chunk) (write-bytes chunk out))))
            ((input-port? data)
             (copy-port data out))
            ((bytes? data)
             (write-bytes data out))
            ((string? data)
             (write-bytes (string->bytes/utf-8 data) out))
            (else
             (error "How to write this data?" data))))
        (begin
          (write-line "EndMessage")
          (newline out))))
  
  (define expect
    (case-lambda
      ((identifier newaliases waiter)
       (set! aliases
             (apply hash-set* aliases                         
                    (let loop ((result '()) (newaliases newaliases))                     
                      (if (null? newaliases)
                          (reverse result)
                          (let ((alias (car newaliases)))
                            (when (hash-ref aliases alias #f)
                              (error "Already waiting on alias" alias identifier))                      
                            (loop (cons identifier (cons (car newaliases) result)) (cdr newaliases)))))))
       (expect identifier waiter))
      ((identifier waiter)
       (if (list? identifier)
           (expect (car identifier) (cdr identifier) waiter)
           (begin
             (set! waiters (hash-set waiters identifier waiter)))))))
  
  (define (doit shutdown)
    (app send expect shutdown)
    
    (let read-a-message ()
      (define name (string->symbol                  
                    (let ((line (read-line in 'linefeed)))
                      (when (eof-object? line)
                        (error "Fffail"))
                      line)))
      (let properties ((opts '()))
        (define line (read-line in 'linefeed))
        (case line
          (("Data" "EndMessage")
           (define identifier (find-identifier
                               (hash-ref aliases name name)
                               opts))
           (define waiter (hash-ref waiters identifier))
           (if (equal? line "Data")                                
               (let-values (((feed finished) (waiter name identifier opts))
                            ((total) (string->number (cdr (assoc "DataLength" opts)))))
                 (let reading-data ((left total))
                   (if (<= left 0)
                       (finished total)
                       (let* ((max-to-read (min left (bytes-length data-buf)))
                              (amount (read-bytes! data-buf in 0 max-to-read)))
                         (when (eof-object? amount)
                           (error "FCP server closed connection"))
                         (cond
                           ((procedure? feed)
                            (feed data-buf amount left total))
                           ((output-port? feed)
                            (write-bytes data-buf amount feed))
                           (else
                            (error "How the heay ~s" feed)))
                         (reading-data (- left amount))))))
               (waiter name identifier opts))
           (read-a-message))
          (else
           
           (define-values (name value) (apply values
                                              (string-split
                                               line
                                               "="
                                               #:repeat? #f)))
           (properties (cons (cons name value) opts)))))
      (read-a-message)))
  
  (dynamic-wind
   (λ ()
     (set!-values (in out) (tcp-connect/enable-break "127.0.0.1" 9481))
     (file-stream-buffer-mode out 'none))
   (λ ()
     (call/cc doit))
   (λ ()
     (close-input-port in)
     (close-output-port out)
     (set! in #f))))

(define make-identifier (let ((counter 0))
                          
                          (λ (sym)
                            (begin0
                              (string-append (symbol->string sym) "-" (number->string counter))
                              (set! counter (+ counter 1))))))

(define uri (let ((uri (getenv "URI")))
              (if (or (not uri) (= 0 (string-length uri)))
                  "KSK@gpl.txt"
                  uri)))

(fcp-loop
 (λ (send expect shutdown)
   (expect 'NodeHello
           (λ (name identifier opts)
             (pretty-print (list 'got name opts))
             (expect '(SimpleProgress ProtocolError)
                     (λ (name identifier opts)
                       (pretty-print (list 'progress name opts))))
             (expect '(DataFound)
                     (λ (name identifier opts)
                       (displayln "Found it!")));
             (expect 'AllData
                     (λ (name identifier opts)
                       (pretty-print (list 'receiving-data name opts))
                       (values
                        (λ (buf amount left total)
                          (println (list 'got-data amount left total)))
                        (λ (total)
                          (println 'all-done)
                          (shutdown)))))
             (expect 'GetFailed
                     (λ (name identifier opts)
                       (pretty-print (list "Aww! It didn't come" uri opts))
                       (shutdown)))
             (send 'ClientGet `((Identifier . ,(make-identifier 'get))
                                (URI . ,uri)
                                (Verbosity . 1)
                                (ReturnType . direct)))))         
   (send 'ClientHello '((Name . "Racket FCP")
                        (ExpectedVersion . 2.0)))))
