#!/bin/sh
# -*- scheme -*-
exec guile -e main -s "$0" "$@"
!#

;; Double-Anonymize the trust.csv by replacing keys with
;; index-numbers.  This prevents results from evaluations of the trust
;; graph from being applied directly to correlation attacks on Freenet
;; users.

;; TODO: use vhashes instead of regular hash tables.

(use-modules (ice-9 rdelim)
             (ice-9 i18n)
             (srfi srfi-69) ; hash tables
             (srfi srfi-1) ; first, second, third
             )


(define (set-add table . elements)
  (let add ((elements elements))
    (cond
     ((null? elements)
      table)
     (else
      (hash-table-set! table (car elements) #t)
      (add (cdr elements))))))

(define (set-keys table)
  (hash-table-keys table))

(define (set-size table)
  (hash-table-size table))

(define (set->list-sorted table)
  (sort-list (set-keys table) string<?))

(define (make-set)
  (make-hash-table))

(define (get-ids port)
  (let collect-ids ((ids (make-set)))
    (let ((line (read-line port)))
      (cond
       ((eof-object? line)
        (set->list-sorted ids))
       (else
          (let* ((columns (string-split line #\;))
                 (source (first columns))
                 (target (second columns)))
            (collect-ids (set-add ids source target))))))))


(define (index-ids-fun ids)
  (let ((id-to-index (make-hash-table)))
    (let fill-table ((ids ids)
                     (index 0))
      (cond ((null? ids)
             id-to-index)
            (else
             (hash-table-set! id-to-index (car ids) index)
             (fill-table (cdr ids)
                         (+ 1 index)))))
    (lambda (id) (hash-table-ref id-to-index id))))

(define (check-csv-header port)
  (let ((header (read-line port))
        (required-header-lowercase "source;target"))
    
    (when (not (string-prefix? required-header-lowercase (string-locale-downcase header)))
      (error (format #f "input file must have header '~A' (regardless of case) but has header '~A'" required-header-lowercase header)))))

(define (index-ids-from-file port)
  (check-csv-header port)
  (let* ((ids (get-ids port))
         (id->index (index-ids-fun ids)))
    id->index))
    

(define (anonymize-ids id->index inport outport)
  (check-csv-header inport)
  (format outport "Source;Target;Weight\n")
  (let anonymize ((line (read-line inport)))
    (cond
     ((eof-object? line) #t)
     (else
      (let* ((columns (string-split line #\;))
             (source (id->index (first columns)))
             (target (id->index (second columns)))
             (weight (third columns)))
        (format outport "~A;~A;~A\n" source target weight))
      (anonymize (read-line inport))))))


(define (main args)
  (let ((infile (if (null? (cdr args))
                    "trust-deduplicated.csv"
                    (second args)))
        (outfile (if (or (null? (cdr args)) (null? (cdr (cdr args))))
                     "trust-anonymized.csv"
                     (third args))))
    (let ((id->index (call-with-input-file infile index-ids-from-file))
          (inport (open-input-file infile))
          (outport (open-output-file outfile)))
      (anonymize-ids id->index inport outport)
      (close-port inport)
      (close-port outport))))
