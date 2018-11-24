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
    only (srfi srfi-9) define-record-type
    only (ice-9 pretty-print) pretty-print
    only (ice-9 rdelim) read-line read-delimited
    only (ice-9 format) format
    only (srfi srfi-1) first second third
    only (rnrs bytevectors) make-bytevector bytevector-length
    only (rnrs io ports) get-bytevector-all get-bytevector-n
         . put-bytevector bytevector->string
    only (ice-9 expect) expect-strings ;; for quick experimentation. Expect needs additional functions and variables available:
        .  expect expect-regexec expect-timeout expect-select expect-timeout-proc
        .  expect-char-proc expect-eof-proc expect-strings-compile-flags
    ice-9 threads
    ice-9 atomic
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


;; the shared FCP socket
define sock #f

define : fcp-socket-create
    define addrs : getaddrinfo "127.0.0.1" "9482"
    define addr : first addrs
    define s : socket (addrinfo:fam addr) (addrinfo:socktype addr) (addrinfo:protocol addr)
    connect s : addrinfo:addr addr
    . s

define : call-with-sock thunk
    . "Calls PROC, ensuring that the variable sock is bound to an FCP socket. 

To clean up, you must close it and set it to #f by yourself"
    dynamic-wind
        λ () : when (not sock) : set! sock : fcp-socket-create
        . thunk
        λ () #f

define-record-type <message>
    message-create task type data fields 
    . message?
    task message-task
    type message-type
    data message-data
    fields message-fields

define : format-field field
    format #f "~a=~a"
        car field
        cdr field

define : join-fields fields
    ## : tests : test-equal "A=B\nX=V" : join-fields : list (cons 'A "B") (cons 'X 'V)
    string-join
        map format-field fields
        . "\n"

define field-key car
define field-value cdr
define : field-split s
  let : : where : string-index s #\=
     if where
        cons 
            string->symbol : substring/shared s 0 where
            substring/shared s (+ where 1) (string-length s)
        cons s ""


define : write-message message
         display (message-type message) sock
         newline sock
         when : message-task message
             format sock "Identifier=~a\n" 
                 message-task message
         display : join-fields : message-fields message
                 . sock
         newline sock
         cond
           : message-data message
             format sock "~a\n"
                 format-field : cons 'DataLength : bytevector-length : message-data message
             format sock "Data\n"
             put-bytevector sock : message-data message
           else
             display 'EndMessage sock
             newline sock

define message-client-hello
    message-create #f 'ClientHello #f
        list : cons 'Name "FetchpullClient" 
               cons 'ExpectedVersion "2.0"

define supported-messages
    ' NodeHello

define : log-warning message things
         format : current-error-port
             . "Warning: ~a: ~a\n" message things

define : read-message port
    let loop : : type : string->symbol : read-line port
        define DataLength #f
        define task #f
        let readlines : : lines : list : read-line port
            define line : first lines
            define field : field-split line
            when : equal? 'DataLength : field-key field
                set! DataLength
                    field-value field
            when : equal? 'Identifier : field-key field
                set! task
                    field-value field
            cond
              : string-index line #\=
                readlines : cons (read-line port) lines
              : member type supported-messages
                let : : data : if DataLength (get-bytevector-n port DataLength) #f
                    message-create task type data
                            map field-split lines
              else
                    log-warning "unsupported message type" type
                    loop : string->symbol : read-line port

define message-processors
    make-atomic-box : list

define : process message
    let loop : (processors (atomic-box-ref message-processors)) (msg message)
        cond
          : not msg
            . #f
          : null? processors
            . msg
          else
            loop (cdr processors)
                 (first processors) msg

define : processor-put! processor
    atomic-box-set! message-processors
        cons processor : atomic-box-ref message-processors

define : processor-remove! processor
    atomic-box-set! message-processors
        delete processor : atomic-box-ref message-processors

define : printing-passthrough-processor message
    pretty-print message
    . message

define : printing-discarding-processor message
    pretty-print message
    . #f

define : read-fcp-loop
    let loop :
        pretty-print : atomic-box-ref message-processors
        warn-unhandled
            process
                read-message sock
        loop

define : warn-unhandled message
    when message
        format : current-error-port
            . "Unhandled message ~a\n" message
    . #f

define : help args
    format : current-error-port
           . "~a [--help | --version | --test | YYYY-mm-dd]\n" : first args

define %this-module : current-module
define : test
    processor-put! printing-passthrough-processor
    let 
       : 
         fcp-thread
           begin-thread
               call-with-sock 
                   λ ()
                       write-message message-client-hello
                       read-fcp-loop
       
       doctests-testmod %this-module
       pretty-print : time->iso today
       pretty-print : time->iso : add-days today 5
       pretty-print : prefix
       pretty-print : KSK-for-insert (prefix) today 5 'realtime
       pretty-print : KSK-for-request (prefix) today 5 'realtime
       pretty-print : time->iso : iso->time "2018-11-23"
       join-thread fcp-thread 30
       close sock
    
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
             
