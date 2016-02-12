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


(define (get url)
  (let* ((u (string->uri url))
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
        (cond
         ((= c 301)
          (get (furl (assoc-ref h 'location))))
          ((= c 200)
           (cond
            ((equal? '(text/html (charset . "utf-8")) (assoc-ref h 'content-type))
             (utf8->string b))
            ((equal? '(application/force-download) (assoc-ref h 'content-type))
             (utf8->string b))
            (else (assoc-ref h 'content-type))))
          (else c))))))



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
               (else '())))
      uris)))

(define (wot-uri-key uri)
  (let ((index (string-index uri #\/)))
    (if index 
        (string-take uri index)
        uri))) ;; no / in uri, so it is already a key.

(define (wot-uri-filename uri)
  (let ((u (if (string-prefix? "freenet:" uri)
               (substring uri 8)
               uri)))
    (string-join (string-split u #\/) "-")))

(define (dump-wot-id uri filename)
  (let ((u (if (string-prefix? "freenet:" uri)
               (substring uri 8)
               uri)))
    (format #t "Download to file ~A\n" filename)
    (if (string-prefix? "USK@" u)
        (let ((data (get (furl-uri u))))
          (if (string? data)
            (let ((port (open-output-file filename)))
              (put-string port data)
              (close-port port))
            (error (format #t "tried to save in file ~A\n" filename))))
        (error (format #t "tried to save in file ~A\n" filename)))))

(define (flatten l)
  "Flatten a nested list into a single list."
  (cond ((null? l) '())
        ((list? l) (append (flatten (car l)) (flatten (cdr l))))
        (else (list l))))

(define* (crawl-wot seed-id #:key (redownload #f))
  ;; TODO: add (flatten ...) with Guile 2.1.x (currently it gives a stack overflow)
  (let ((known '()))
    (let crawl ((seed seed-id))
      ;; save the data
      (if (catch 'misc-error
            (lambda () (let* ((filename (wot-uri-filename seed))
                              (dump (lambda () (dump-wot-id seed filename))))
                         (if (and (not redownload) (file-exists? filename))
                             (let* ((s (stat filename))
                                    (size (stat:size s)))
                               (if (= size 0)
                                   (dump)
                                   (format #t "Use local copy of file ~A (redownload ~A).\n" filename redownload)))
                             (dump))
                         #f))
            (lambda (key . args) #t))
          known
          ;; snarf all uris
          (let ((uris (call-with-input-file (wot-uri-filename seed) snarf-wot-ids)))
            ;; (write seed)(newline)
            ;; (when (not (null? uris))
            ;;  (write (car uris))(newline))
            (let ((new (list-ec (: u uris) (if (and
                                                (not (pair? u)) ; TODO: this is a hack. I do not know why u can be the full sxml. Seems to happen with IDs who do not have any trust set.
                                                (not (member (wot-uri-key u) known)))) u)))
              (when (not (null? new))
                (display 'new:)
                (write (car new))(newline))
              (when (not (null? known))
                (display 'known:)
                (write (car known))(newline)(write (length known))(newline))
              (set! known (lset-union equal?
                                      (list-ec (: u new) (wot-uri-key u))
                                      known))
              (if (null? new)
                  known
                  (append known (map crawl new)))))))))

(define (parse-datehint str)
  (let ((lines (string-split str #\newline)))
    `((version . ,(list-ref lines 1))
      (date . ,(list-ref lines 2)))))

(define* (datehint-for-key key year #:key (sitename "WebOfTrust") (week #f))
  (string-append "SSK" (substring key 3)
                 "/" sitename
                 "-" "DATEHINT"
                 "-" (number->string year)
                 (if week (string-append "-WEEK-" (number->string week)) "")))
  

(define (furl-key-name-version key name version)
  "Get a freenet URL for the key and the version"
  (furl-uri (string-append "SSK" (substring key 3) "/" name "-" version)))

(define (download-by-weekly-date-hint uri year week)
  (let* ((weekuri (datehint-for-key (wot-uri-key uri) year #:week week))
         (hint (get (furl-uri weekuri))))
    (if (not (string? hint))
        #f
        (let* ((hint-alist (parse-datehint hint))
               (version (assoc-ref hint-alist 'version))
               (date (assoc-ref hint-alist 'date))
               (url (furl-key-name-version (wot-uri-key uri) "WebOfTrust" version))
               (filename (string-append date "/" (wot-uri-key uri) "-" version)))
          (when (not (file-exists? date))
            (mkdir date))
          (let ((data (get url)))
            (when (string? data)
              (let ((port (open-output-file filename)))
                (put-string port data)
                (close-port port))))
          filename))))

(define (download-by-date-hint uri)
  "Download all versions of the ID, ordered by the week in the DATEHINT."
  ;; An uri looks like this: USK@QWW2a74OWrtN-aWJ80fjWhfFx8NlNrlU0dQfd3J7t1E,2g-wfM57Up9DV1qoEDMPcDU-KPskk0yyiYFz67ydSos,AQACAAE
  ;; A date hint for WoT looks like this: SSK@QWW2a74OWrtN-aWJ80fjWhfFx8NlNrlU0dQfd3J7t1E,2g-wfM57Up9DV1qoEDMPcDU-KPskk0yyiYFz67ydSos,AQACAAE-WebOfTrust-DATEHINT-2015
  ;; or
  ;; SSK@[key]/[sitename]-DATEHINT-[year]
  ;; SSK@[key]/[sitename]-DATEHINT-[year]-WEEK-[week]
  ;; SSK@[key]/[sitename]-DATEHINT-[year]-[month]
  ;; SSK@[key]/[sitename]-DATEHINT-[year]-[month]-[day]
  ;; see http://draketo.de/light/english/freenet/usk-and-date-hints
  ;; Approach: First check whether the ID has a date hint for each year. Then check each weak in the matching years.
  ;; download the versions into directories ordered as YEAR-month-day/SSK@...-WebOfTrust-version
  (let ((years (iota 10 2016 -1))
        (weeks (iota 52 1))) ; 52-1
    (delete #f ;; only return the filenames of successful downloads 
            (par-map (lambda (year)
                       (let* ((yearuri (datehint-for-key (wot-uri-key uri) year))
                              (hint (get (furl-uri yearuri))))
                         (if (not (string? hint))
                             #f
                             (delete #f ;; only return the filenames of successful downloads 
                                     (n-par-map 52 (lambda (week)
                                                     (download-by-weekly-date-hint uri year week))
                                                weeks)))))
                     years))))

(define (main args)
  (let ((seed-id (if (null? (cdr args))
                     seed-id
                     (car (cdr args)))))
    (let ((seed (if (string-index seed-id #\/)
                    seed-id
                    (string-append "USK" (string-drop seed-id 3) "/WebOfTrust/0"))))
      (write (download-by-date-hint seed))
      (par-map (lambda (x) (map download-by-date-hint x))
               (crawl-wot seed)))))
