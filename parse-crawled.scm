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


(define (main args)
  (let ((dir (if (null? (cdr args))
                 "."
                 (car (cdr args)))))
    (let ((select? (lambda (x) (or (equal? x ".") (string-prefix? "USK@" x)))))
      (write (length (scandir dir select?)))
      (newline))))
