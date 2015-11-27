;; Copyright (c) 2015 dinky's evil twin sone://EWtk1limedjBM2LnGE3~z98tC8bLTu9ryLIMcFgg8PI
;; License: LGPL

(use-modules
 (rnrs bytevectors)
 (rnrs io ports) ;; get/put-bytevector bytevector->string
 (ice-9 rw) ;; write-string
 (ice-9 rdelim)
 (ice-9 pretty-print)
 (ice-9 vlist)
 (srfi srfi-11)) ; let-values

(define (println s)
  (pretty-print s))

(define (copy-port in out)
  (let ((buf (make-bytevector #x1000)))
    (let loop ()
      (let ((amt (get-bytevector-n! in buf 0 #x1000)))
        (when amt
          (put-bytevector out buf 0 amt)
          (loop))))))

(define (find-identifier name opts)
  (let ((ret
  (let ((identifier (assq 'Identifier opts)))
    (if identifier
        (string->symbol (cdr identifier))
        name))))
    (println (list 'find-id name opts ret))
    ret))

(define (string-splitonce s delim)
  (let ((where (string-index s delim)))
    (if where
        (values (substring/shared s 0 where) (substring/shared s (+ where 1) (string-length s)))
        (values s ""))))

(define (vhash-keys v)
  (vhash-fold (lambda (name value l) (cons name l)) '() v))

(define (fcp-loop app)
  (define waiters vlist-null)
  (define aliases vlist-null)
  (define sock #f)

  (define data-buf (make-bytevector #x1000))

  (define (write-line s)
    (write-string/partial s sock)
    (newline sock))

  (letrec* ((send
         (lambda* (name opts #:optional (data #f) (data-length 0))
                  (println (symbol->string name))
                  (write-line (symbol->string name))
                  (for-each
                   (λ (pair)
                     (let ((name (car pair))
                           (value (cdr pair)))
                       (let ((line (string-append (symbol->string name)
                                                  "=" (cond
                                                       ((symbol? value) (symbol->string value))
                                                       ((string? value) value)
                                                       ((bytevector? value) (bytevector->string
                                                                             value "utf-8"))
                                                       ((number? value) (number->string value))
                                                       ((eq? value #f) "false")
                                                       ((eq? value #t) "true")
                                                       (else
                                                        (error "wat is ~s" value))))))
                         (println line)
                         (write-line line))))
                   opts)
                  (if data
                      (begin
                        (write-line (string-append "Data-Length=" (number->string data-length)))
                        (write-line "Data")
                        (cond
                         ((procedure? data)
                          (data (λ (chunk) (put-bytevector sock chunk))))
                         ((input-port? data)
                          (copy-port data sock))
                         ((bytevector? data)
                          (put-bytevector sock data))
                         ((string? data)
                          (put-bytevector sock (string->bytevector data "utf-8")))
                         (else
                          (error "How to write this data?" data))))
                      (begin
                        (println "EndMessage")
                        (write-line "EndMessage")
                        (newline sock)))))
         (expect
         (case-lambda
          ((identifier newaliases waiter)
           (set! aliases
                 (let loop ((result aliases) (newaliases newaliases))
                   (if (null? newaliases)
                       result
                       (let ((alias (car newaliases)))
                         (when (vhash-assq alias aliases)
                               (error "Already waiting on alias" alias identifier))
                         (loop (vhash-consq identifier (car newaliases) result) (cdr newaliases))))))
           (expect identifier waiter))
          ((identifier waiter)
           (if (list? identifier)
               (expect (car identifier) (cdr identifier) waiter)
               (begin
                 (println (list 'consq identifier waiter (vhash-keys waiters)))
                 (set! waiters (vhash-consq identifier waiter waiters))
                 (println (list 'consq identifier waiter (vhash-keys waiters))))))))

        (doit (lambda (shutdown)
                (app send expect shutdown)
                (let read-a-message ()
                  (define name (string->symbol
                                (let ((line (read-line sock 'trim)))
                                  (println (list 'line line))
                                  (when (eof-object? line)
                                        (error "Fffail"))
                                  line)))
                  (let properties ((opts '()))
                    (define line (read-line sock 'trim))
                    (println (list 'line line))
                    (if (or (equal? line "Data")
                            (equal? line "EndMessage"))
                        (begin
                          (println 'woo)
                          (let* ((name
                                  (let ((derp (vhash-assq name aliases)))
                                    (if derp
                                        (cdr derp)
                                        name)))
                                 (identifier (find-identifier name opts))
                                 (waiter (let ((waiter
                                                (or
                                                 (vhash-assq identifier waiters)
                                                 (vhash-assq name waiters))))
                                           (when (not waiter)
                                                 (println (list identifier name 'not-iny (vhash-keys waiters)))
                                                 (error "waugh"))
                                           (cdr waiter))))
                            (println (list 'waiteruh waiter))
                         (if (equal? line "Data")
                             (let-values (((feed finished) (waiter name identifier opts))
                                          ((total) (string->number (cdr (assoc 'DataLength opts)))))
                               (let reading-data ((left total))
                                 (if (<= left 0)
                                     (finished total)
                                     (let* ((max-to-read (min left (bytevector-length data-buf)))
                                            (amount (get-bytevector-n! sock data-buf 0 max-to-read)))
                                       (when (eof-object? amount)
                                             (error "FCP server closed connection"))
                                       (cond
                                        ((procedure? feed)
                                         (feed data-buf amount left total))
                                        ((output-port? feed)
                                         (put-bytevector feed data-buf amount))
                                        (else
                                         (error "How the heay ~s" feed)))
                                       (reading-data (- left amount))))))
                             (waiter name identifier opts)))
                       (read-a-message))
                        (call-with-values
                            (lambda ()
                              (string-splitonce line #\=))
                          (lambda (name value)
                            (println (list 'pair name value))
                            (properties (cons (cons (string->symbol name) value) opts))))))
                  (read-a-message)))))
    (dynamic-wind
        (λ ()
          (set! sock (let* ((addrs (getaddrinfo "127.0.0.1" "9481"))
                            (addr (car addrs))
                            (s (socket (addrinfo:fam addr)
                                       (addrinfo:socktype addr)
                                       (addrinfo:protocol addr))))
                       (connect s (addrinfo:addr addr))
                       s)))
        (λ ()
          (call/cc doit))
        (λ ()
          (close-port sock)
          (set! sock #f)))))

(define make-identifier (let ((counter 0))

                          (λ (sym)
                            (let ((result
                                   (string-append (symbol->string sym)
                                                  "-"
                                                  (number->string counter))))
                              (set! counter (+ counter 1))
                              result))))

(define uri (let ((uri (getenv "URI")))
              (if (or (not uri) (= 0 (string-length uri)))
                  "KSK@gpl.txt"
                  uri)))

(fcp-loop
 (λ (send expect shutdown)
   (expect 'NodeHello
           (λ (name identifier opts)
             (pretty-print (list 'got name opts))
             (expect '(SimpleProgress) ; ProtocolError)
                     (λ (name identifier opts)
                       (pretty-print (list 'progress name opts))))
             (expect '(DataFound)
                     (λ (name identifier opts)
                       (println "Found it!")));
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
