#!/bin/sh
# -*- scheme -*-
exec guile -e main -s "$0" "$@"
!#

;; Simple WoT crawler

(use-modules (web request)
             (web client)
             (web response)
             (web uri)
             (web http)
             (ice-9 threads)
             (ice-9 vlist)
             (ice-9 rdelim)
             (rnrs io ports)
             (ice-9 match)
             (srfi srfi-42)
             (srfi srfi-1)
             (rnrs bytevectors)
             (sxml simple)
             (sxml match))

(define base-url "http://127.0.0.1:8888")
(define seed-id "USK@QeTBVWTwBldfI-lrF~xf0nqFVDdQoSUghT~PvhyJ1NE,OjEywGD063La2H-IihD7iYtZm3rC0BP6UTvvwyF5Zh4,AQACAAE/WebOfTrust/1502")

(define (furl uri)
  (string-append base-url uri "?forcedownload=true"))

(define (furl-uri uri)
  (string-append base-url "/" uri "?forcedownload=true"))


(define (get uri)
  (let* ((u (string->uri uri))
         (r (build-request u))
         (p (open-socket-for-uri u))
         (rr (write-request r p))
         (rp (request-port rr)))
    (force-output p)
    (declare-opaque-header! "Location")
      ;(while (write (read-line p))
      ;       (newline))
    (let ((resp (read-response rp)))
      (let ((c (response-code resp))
            (h (response-headers resp))
            (b (read-response-body resp)))
        (if (= c 301)
            (get (furl (assoc-ref h 'location)))
            (cond
             ((equal? '(text/html (charset . "utf-8")) (assoc-ref h 'content-type))
              (utf8->string b))
             ((equal? '(application/force-download) (assoc-ref h 'content-type))
              (utf8->string b))
             (else
              (assoc-ref h 'content-type))))))))


(define (non-breaking-sxml-reader xml-port)
  (catch #t
         (lambda () (xml->sxml xml-port))
         (lambda (key . args) (format #t "~A: ~A" key args)(newline) '())))

(define (snarf-wot-ids xml-port)
  (let ((sxml (non-breaking-sxml-reader xml-port)))
    (let ((uris '()))
      (let grab-uris ((sxml sxml))
        (match sxml
               (('Identity uri) (set! uris (cons uri uris)))
               ((a b ...)
                (map grab-uris sxml))
               (else sxml)))
      uris)))

(define (wot-uri-key uri)
  (string-take uri (string-index uri #\/)))

(define (wot-uri-filename uri)
  (string-join (string-split uri #\/) "-"))

(define (dump-wot-id uri filename)
  (if (string-prefix? "USK@" uri)
      (let ((port (open-output-file filename)))
        (put-string port (get (furl-uri uri))))
      (error (format #t "tried to save in file ~A" uri))))

(define (crawl-wot seed-id)
  (let ((known '()))
    (let crawl ((seed seed-id))
      ;; save the data
      (dump-wot-id seed-id (wot-uri-filename seed))
      ;; snarf all uris
      (let* ((uris (call-with-input-file (wot-uri-filename seed) snarf-wot-ids))
             (new (list-ec (: u uris) (if (not (member (wot-uri-key u) known))) u)))
        (when (not (null? new))
              (display 'new:)
              (write (car new))(newline))
        (when (not (null? known))
              (display 'known:)
              (write (car known))(newline)(write (length known))(newline))
        (set! known (lset-union equal?
                     (list-ec (: u new) (wot-uri-key u))
                     known))
        (map crawl new)))))

(define (main args)
  (dump-wot-id seed-id (wot-uri-filename seed-id))
  (crawl-wot seed-id)
  (newline))
