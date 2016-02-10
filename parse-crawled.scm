#!/bin/sh
# -*- scheme -*-
exec guile -e main -s "$0" "$@"
!#

;; Parser to turn a set of downloaded WoT files into a standard graph format.

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
             (sxml match)
             (ice-9 ftw))


(define (non-breaking-sxml-reader xml-port)
  (catch #t
         (lambda () (xml->sxml xml-port))
         (lambda (key . args) (format #t "~A: ~A" key args)(newline) '())))


(define (parse-trust-values filename)
  (let* ((port (open-input-file filename))
         (sxml (non-breaking-sxml-reader port))
         (closed (close-port port)))
    #f))
    

(define (main args)
  (let ((dir (if (null? (cdr args))
                 "."
                 (car (cdr args)))))
    (let ((select? (lambda (x) (or (equal? x ".") (string-prefix? "USK@--PQYgLrwxB~4Q~vok8EVMCGoPzxSsVem6TwsN9CBKE,UEcmWOEG24NhwoMlP8IkhySUUyRkPwZnIsNoP2nZy4U,AQACAAE-WebOfTrust-0" x)))))
      (write (map parse-trust-values (cdr (scandir dir select?))))
      (newline))))
