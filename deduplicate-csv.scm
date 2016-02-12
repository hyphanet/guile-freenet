#!/bin/sh
# -*- scheme -*-
exec guile -e main -s "$0" "$@"
!#

;; Remove duplicate entries from the csv file (these are due to
;; downloading multiple versions of the same ID).

(use-modules (ice-9 rdelim) ; for read-line
             (ice-9 i18n)
             (srfi srfi-1) ; first, second, third
             )

(define (deduplicate infile outfile)
  (let ((known (make-hash-table))
        (inport (open-input-file infile))
        (outport (open-output-file outfile)))
    ;; first copy the header
    (display (read-line inport) outport)
    (newline outport)
    (let copy-dedup ((line (read-line inport)))
      (cond
       ((eof-object? line)
        #t)
       (else
        (let* ((columns (string-split line #\;))
               (source (first columns))
               (target (second columns))
               (key (string-append source target)))
          (when (not (hash-ref known key))
            (hash-set! known key #t)
            (display line outport)
            (newline outport))
          (copy-dedup (read-line inport))))))))
            

(define (main args)
  (let ((infile (if (null? (cdr args))
                    "trust.csv"
                    (second args)))
        (outfile (if (or (null? (cdr args)) (null? (cdr (cdr args))))
                     "trust-deduplicated.csv"
                     (third args))))
    (deduplicate infile outfile)))
