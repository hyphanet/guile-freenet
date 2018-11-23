#!/usr/bin/env bash
# -*- wisp -*-
guile -L $(dirname $(realpath "$0")) -c '(import (language wisp spec))'
exec -a "$0" guile -L $(dirname $(realpath "$0")) --language=wisp -x .w -e '(fetchpull)' -c '' "$@"
; !#

;; for emacs (defun test-this-file () (interactive) (save-current-buffer) (async-shell-command (concat (buffer-file-name (current-buffer)) " --test")))

define-module : fetchpull
    . #:export : main

define version "0.0.0 just-do-it"

define design
    ' 
        keys are KSK@<prefix>--DATE-uploaded-xxx-days-before-using-MODE
        process:
            get the current date
            for realtime or bulk as MODE
                for each power of two up to 128 as iii
                    insert a random chunk (without compression) 
                        to {DATE + iii days}-uploaded-iii-days-before-using-MODE
                    request the key DATE-uploaded-iii-days-before-using-MODE
                    write the times along with the keys (without the prefix) 
                        into insert-times.csv and request-times.csv
                    format as
                        DATE-as-seconds-since-epoch iii MODE
        prefix is generated from securepassword.w and stored in the file fetchpull-prefix.txt

import
    only (srfi srfi-19) current-date date->string string->date date->time-utc time-utc->date
                      . make-time time-utc time-duration add-duration current-time
    only (securepassword) letterblocks-nice
    only (ice-9 pretty-print) pretty-print
    only (ice-9 rdelim) read-line
    only (ice-9 format) format
    only (srfi srfi-1) first second third
    doctests

define today : current-time time-utc
define : time->iso time
    date->string (time-utc->date time) "~1"
define : iso->time string
    date->time-utc : string->date string "~Y~m~d"

define : add-days time number-of-days
    let : : seconds : * 3600 24 number-of-days
        add-duration time
            make-time time-duration 0 seconds

define : string-replace-string s char replacement-string
    string-join (string-split s char) replacement-string
define : replace-KSK-escaped s
    string-replace-string : string-replace-string s #\+ "-"
        . #\= "-"
        
define prefix-filename "fetchpull-prefix.txt"
define prefix-cache #f
define : prefix
    cond
       prefix-cache 
         . prefix-cache
       : file-exists? prefix-filename
         read-line : open-input-file prefix-filename
       else
           let
            : pw : replace-KSK-escaped : letterblocks-nice 6
              port : open-output-file prefix-filename
            display pw port
            close-port port
            . pw

define : KSK-for-insert prefix today days-before mode
    format #f "KSK@~a--~a-uploaded-~3,'0d-days-before-using-~a" prefix 
            time->iso : add-days today days-before
            . days-before mode

define : KSK-for-request prefix time days-before mode
    ##
        tests
            test-equal   
              . "KSK@WwL6-UXTu-sa5n.fAk2-s7kj.5Kp6--2018-11-23-uploaded-005-days-before-using-realtime"
              KSK-for-request "WwL6-UXTu-sa5n.fAk2-s7kj.5Kp6" (iso->time "2018-11-23") 5 'realtime
               
    format #f "KSK@~a--~a-uploaded-~3,'0d-days-before-using-~a" prefix
            time->iso time
            . days-before mode



define : help args
    format : current-error-port
           . "~a [--help | --version | --test | YYYY-mm-dd]\n" : first args

define %this-module : current-module
define : test
    doctests-testmod %this-module
    pretty-print : time->iso today
    pretty-print : time->iso : add-days today 5
    pretty-print : prefix
    pretty-print : KSK-for-insert (prefix) today 5 'realtime
    pretty-print : KSK-for-request (prefix) today 5 'realtime
    pretty-print : time->iso : iso->time "2018-11-23"

define : main args
    if {(length args) > 1}
       cond 
           : equal? "--help" : second args
             help args
             exit 0
           : equal? "--version" : second args
             format : current-error-port
                    . "~a\n" version
             exit 0
           : equal? "--test" : second args
             test
             exit 0
           else
             pretty-print : second args
             set! today : iso->time : second args
