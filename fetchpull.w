#!/usr/bin/env bash
# -*- wisp -*-
guile -L $(dirname $(realpath "$0")) -c '(import (language wisp spec))'
PROG="$0"
if [[ "$1" == "-i" ]]; then
    shift
    exec -a "${PROG}" guile -L $(dirname $(realpath "$0")) --language=wisp -x .w -e '(fetchpull)' -- "${@}"
else
    exec -a "${0}" guile -L $(dirname $(realpath "$0")) --language=wisp -x .w -e '(fetchpull)' -c '' "${@}"
fi;
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
                        DATE-as-seconds-since-epoch duration iii MODE
        prefix is generated from securepassword.w and stored in the file fetchpull-prefix.txt

import
    only (srfi srfi-19) current-date date->string string->date date->time-utc time-utc->date
                      . make-time time-utc time-duration add-duration current-time
    only (securepassword) letterblocks-nice
    only (srfi srfi-9) define-record-type
    only (ice-9 pretty-print) pretty-print
    only (ice-9 rdelim) read-line read-delimited
    only (ice-9 format) format
    only (srfi srfi-1) first second third alist-cons assoc lset<= lset-intersection lset-difference
    only (rnrs bytevectors) make-bytevector bytevector-length string->utf8
    only (rnrs io ports) get-bytevector-all get-bytevector-n
         . put-bytevector bytevector->string port-eof?
    only (ice-9 expect) expect-strings ;; for quick experimentation. Expect needs additional functions and variables available:
        .  expect expect-regexec expect-timeout expect-select expect-timeout-proc
        .  expect-char-proc expect-eof-proc expect-strings-compile-flags
    only (ice-9 regex) string-match match:substring
    ice-9 threads
    ice-9 atomic
    only (ice-9 q) make-q enq! deq! q-empty?
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
        
define : task-id
    replace-KSK-escaped : letterblocks-nice 6

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
            : pw : task-id
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
    define addrs : getaddrinfo "127.0.0.1" "9481"
    define addr : first addrs
    define s : socket (addrinfo:fam addr) (addrinfo:socktype addr) (addrinfo:protocol addr)
    connect s : addrinfo:addr addr
    . s

define-record-type <message>
    message-create task type data fields 
    . message?
    task message-task
    type message-type
    data message-data
    fields message-fields ;; avoid duplicates: fred joins duplicate fields with ";" to a single value

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


define : write-message message sock
         display (message-type message) sock
         newline sock
         when : message-task message
             format sock "Identifier=~a\n" 
                 message-task message
         when : not : null? : message-fields message
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

define : message-client-hello
    message-create #f 'ClientHello #f
        list : cons 'Name "FetchpullClient" 
               cons 'ExpectedVersion "2.0"

define : message-watch-global
    message-create #f 'WatchGlobal #f
        list : cons 'Enabled "true" 
               cons 'VerbosityMask 1 ;; simple progress

define : message-disconnect
    message-create #f 'Disconnect #f
        list

define : message-client-get task URI custom-fields
    message-create task 'ClientGet #f
        append
          list : cons 'URI URI
          ' : Verbosity . 1 ;; get SimpleProgress messages for the tasks
              ReturnType . direct
              MaxRetries . 1 ;; -1 means: try indefinitely, with ULPR, essentially long polling
              Global . true
              Persistence . reboot
          . custom-fields

define : message-client-get-realtime task URI
    message-client-get task URI
        '
          PriorityClass . 2
          RealTimeFlag . true
          FilterData . false

define : message-client-put task URI data custom-fields
    message-create task 'ClientPut data
        append
          list : cons 'URI URI
          ` : Verbosity . 1 ;; get SimpleProgress messages for the tasks
              MaxRetries . 1 ;; default: 10
              Global . true
              Persistence . reboot
              UploadFrom . direct
          . custom-fields

define : message-client-put-realtime task URI data
    message-client-put task URI data
        '
          PriorityClass . 2
          RealTimeFlag . true
          DontCompress . true
          ExtraInsertsSingleBlock . 0
          ExtraInsertsSplitfileHeaderBlock . 0
          Metadata.ContentType . application/octet-stream

define : message-remove-request task
    message-create task 'RemoveRequest #f
            list : cons 'Global 'true


define supported-messages
    ' NodeHello GetFailed DataFound AllData PutSuccessful PutFailed

define : log-warning message things more
         format : current-output-port
             . "Warning: ~a: ~a\n~a\n" message things more

define : read-message port
  if : port-eof? port
    . #f
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
            ;; pretty-print : list 'line line 'type type
            cond
              : string-index line #\=
                readlines : cons (read-line port) lines
              : member type supported-messages ;; line is Data or EndMessage
                let
                    : 
                      data ;; EndMessage has no Data
                          if : and DataLength : not : equal? "EndMessage" line
                              get-bytevector-n port (string->number DataLength)
                              . #f
                    message-create task type data
                            map field-split : cdr lines
              else
                    log-warning "unsupported message type" type lines
                    if : port-eof? port
                        . #f
                        loop : string->symbol : read-line port

define next-message
    make-atomic-box #f

define : send-message message
    ;; wait until the message was retrieved. This only replaces if the previous content was #f. take-message-to-send switches takes the messages
    let try : : failed : atomic-box-compare-and-swap! next-message #f message
        when failed
            usleep 100
            try : atomic-box-compare-and-swap! next-message #f message

define : take-message-to-send
    ;; get the message and reset next-message to #f to allow taking another message
    atomic-box-swap! next-message #f

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
    let loop : : old : atomic-box-ref message-processors
        define old-now : atomic-box-compare-and-swap! message-processors old : cons processor old
        when : not : equal? old old-now
            loop : atomic-box-ref message-processors

define : processor-delete! processor
    let loop : : old : atomic-box-ref message-processors
        define old-now : atomic-box-compare-and-swap! message-processors old : delete processor old
        when : not : equal? old old-now
             loop : atomic-box-ref message-processors

define : fcp-read-loop sock
    let loop : : message : read-message sock
        when message
            warn-unhandled
                process message
            loop : read-message sock

define : fcp-write-loop sock
    let loop : : message : take-message-to-send
        if message
          begin
            write-message message sock
          usleep 100
        loop : take-message-to-send

define : warn-unhandled message
    when message
        format : current-error-port  ;; avoid writing to the error port elsewhere, that causes multithreading problems. Use current-output-port instead
            . "Unhandled message ~a\n" message
    . #f

define : printing-passthrough-processor message
    pretty-print message
    . message

define : printing-discarding-processor message
    pretty-print message
    . #f

define : discarding-processor message
    . #f


define : help args
    format : current-output-port
           . "~a [-i] [--help | --version | --test | YYYY-mm-dd]

Options:
        -i    load the script and run an interactive REPL."
           first args

;; timing information (alists)
define get-successful : list
define get-failed : list
define put-successful : list
define put-failed : list
define get-alldata : list ; the actual data, for debugging

define : processor-record-datafound-getdata message
    cond
      : equal? 'DataFound : message-type message
        send-message
            message-create : message-task message
                . 'GetRequestStatus #f
                list : cons 'Global 'true
        . #f
      else message

define : current-time-seconds
    car : gettimeofday

define : processor-record-alldata-time message
    cond
      : equal? 'AllData : message-type message
        set! get-successful
            alist-cons (message-task message) (current-time-seconds) get-successful
        . #f
      else message


define : processor-record-getfailed-time message
    cond
      : equal? 'GetFailed : message-type message
        set! get-failed
            alist-cons (message-task message) (current-time-seconds) get-failed
        . #f
      else message

define : processor-record-putfailed-time message
    cond
      : equal? 'PutFailed : message-type message
        set! put-failed
            alist-cons (message-task message) (current-time-seconds) put-failed
        . #f
      else message

define : processor-record-putsuccessful-time message
    cond
      : equal? 'PutSuccessful : message-type message
        set! put-successful
            alist-cons (message-task message) (current-time-seconds) put-successful
        . #f
      else message

define : processor-record-identifier-collision-time message failed
    cond
      : equal? 'IdentifierCollision : message-type message
        set! failed
            alist-cons (message-task message) (current-time-seconds) failed
        . #f
      else message

define : processor-record-identifier-collision-get-time message
    processor-record-identifier-collision-time message get-failed

define : processor-record-identifier-collision-put-time message
    processor-record-identifier-collision-time message put-failed


define-record-type <duration-entry>
    duration-entry key duration successful operation mode
    . timing-entry?
    key duration-entry-key
    duration duration-entry-duration
    successful duration-entry-success
    operation duration-entry-operation ;; get or put
    mode duration-entry-mode ;; realtime bulk speehacks
    

define : time-get keys
    define start-times : list
    define : finished-tasks
        append
            map car get-successful
            map car get-failed
    ;; setup a processing chain which saves the time information about the request
    processor-put! processor-record-datafound-getdata
    processor-put! processor-record-alldata-time
    processor-put! processor-record-getfailed-time
    processor-put! processor-record-identifier-collision-get-time
    ;; just use the keys as task-IDs (Identifiers)
    let loop : (keys keys)
        when : not : null? keys
            ;; first remove requests which might still be in the upload or download queue
            send-message
               message-remove-request : first keys
            set! start-times : alist-cons (first keys) (current-time-seconds) start-times
            send-message
                message-client-get-realtime (first keys) (first keys)
            loop (cdr keys)
    ;; wait for completion
    let loop : (finished (finished-tasks))
        when : not : lset<= equal? keys finished
            let : : unfinished : lset-difference equal? keys : lset-intersection equal? keys finished
                format : current-output-port
                    . "~d keys still in flight: ~a\n" (length unfinished) unfinished
            usleep 1000000
            loop (finished-tasks)
    ;; all done: cleanup and take the timing
    processor-delete! processor-record-identifier-collision-get-time
    processor-delete! processor-record-getfailed-time
    processor-delete! processor-record-alldata-time
    processor-delete! processor-record-datafound-getdata
    let loop : (keys keys) (times '())
        if : null? keys
           . times
           let :
             define key : first keys
             define (gettime L) : cdr : assoc key L
             define start-time : gettime start-times
             define finish-time : gettime : append get-successful get-failed
             define successful : and (assoc key get-successful) #t ;; forces boolean
             send-message
                 message-remove-request key
             loop : cdr keys
                 cons : duration-entry (first keys) {finish-time - start-time} successful 'GET 'realtime
                      . times

define : time-put keys
    define start-times : list
    define : finished-tasks
        append
            map car put-successful
            map car put-failed
    ;; setup a processing chain which saves the time information about the request
    processor-put! processor-record-putsuccessful-time
    processor-put! processor-record-putfailed-time
    processor-put! processor-record-identifier-collision-put-time
    ;; just use the keys as task-IDs (Identifiers)
    let loop : (keys keys)
        when : not : null? keys
            ;; first remove requests which might still be in the upload or download queue
            send-message
               message-remove-request : first keys
            set! start-times : alist-cons (first keys) (current-time-seconds) start-times
            send-message
                message-client-put-realtime (first keys) (first keys)
                    string->utf8 (first keys)
            loop (cdr keys)
    ;; wait for completion
    let loop : (finished (finished-tasks))
        when : not : lset<= equal? keys finished
            let : : unfinished : lset-difference equal? keys : lset-intersection equal? keys finished
                format : current-output-port
                    . "~d keys still in flight: ~a\n" (length unfinished) unfinished
            usleep 1000000
            loop (finished-tasks)
    ;; all done: cleanup and take the timing
    processor-delete! processor-record-identifier-collision-put-time
    processor-delete! processor-record-putfailed-time
    processor-delete! processor-record-putsuccessful-time
    let loop : (keys keys) (times '())
        if : null? keys
           . times
           let :
             define key : first keys
             define (gettime L) : cdr : assoc key L
             define start-time : gettime start-times
             define finish-time : gettime : append put-successful put-failed
             define successful : assoc key put-successful
             send-message
                 message-remove-request key
             loop : cdr keys
                 cons : duration-entry (first keys) {finish-time - start-time} successful 'PUT 'realtime
                      . times


define %this-module : current-module
define : test
    processor-put! discarding-processor
    processor-put! printing-passthrough-processor
    set! sock : fcp-socket-create
    let 
       : 
         fcp-read-thread
           begin-thread
             fcp-read-loop sock
         fcp-write-thread
           begin-thread
             fcp-write-loop sock
       send-message : message-client-hello
       send-message : message-watch-global
       send-message : message-client-get-realtime (letterblocks-nice 6) "USK@N82omidQlapADLWIym1u4rXvEQhjoIFbMa5~p1SKoOY,LE3WlYKas1AIdoVX~9wahrTlV5oZYhvJ4AcYYGsBq-w,AQACAAE/irclogs/772/2018-11-23.weechatlog"
       sleep 30
       send-message : message-disconnect
       doctests-testmod %this-module
       join-thread fcp-write-thread : + 30 : current-time-seconds
       join-thread fcp-read-thread : + 30 : current-time-seconds
       close sock

define : call-with-fcp-connection thunk
    set! sock : fcp-socket-create
    let 
       : 
         fcp-read-thread
           begin-thread
             fcp-read-loop sock
         fcp-write-thread
           begin-thread
             fcp-write-loop sock
       send-message : message-client-hello
       send-message : message-watch-global
       thunk
       send-message : message-disconnect
       join-thread fcp-write-thread : + 3 : current-time-seconds
       join-thread fcp-read-thread : + 3 : current-time-seconds
       close sock

define-syntax-rule : with-fcp-connection exp ...
    call-with-fcp-connection
        λ () exp ...

define* : stats->csv stats #:key (target-filename #f)
  . "Format the all duration-entry in stats as csv file.

example:
date;key;duration;days-before;mode;success
KSK@...;32;16;realtime;false
KSK@...;40;32;realtime;true
"
  define : days-before key
      string->number
          match:substring
              string-match "uploaded-([0-9]*)-days-before" key
              . 1
  define new : not : and target-filename : file-exists? target-filename
  define port
      cond 
        target-filename
          open-file target-filename "al"
        else
          current-output-port
  when new
      display "day;key;duration;days-before;mode;success" port
      newline port          
  let loop : : stats stats
    when : not : null? stats
      let : : s : first stats
        format port "~a;~a;~f;~d;~a;~a\n"
            time->iso today
            duration-entry-key s
            duration-entry-duration s
            days-before : duration-entry-key s
            duration-entry-mode s
            duration-entry-success s
      loop : cdr stats
  when target-filename : close-port port

    
define : main args
    when {(length args) > 1}
       cond 
           : equal? "--help" : second args
             help args
             exit 0
           : equal? "--version" : second args
             format : current-output-port
                    . "~a\n" version
             exit 0
           : equal? "--test" : second args
             test
             exit 0
           else
             pretty-print : second args
             set! today : iso->time : second args
    processor-put! printing-passthrough-processor
    let : (get-stats '()) (put-stats '())
      define : stats-get stat
          set! get-stats : append get-stats stat
          . stat
      define : stats-put stat
          set! put-stats : append put-stats stat
          . stat
      with-fcp-connection
          let loop
             : modes '(realtime)
             define days-before
                 cons 0
                     map : λ(x) : expt 2 x
                         iota 10
             define : KSK-for-get days
                 KSK-for-request (prefix) (current-time) days 'realtime
             define : KSK-for-put days
                 KSK-for-insert (prefix) (current-time) days 'realtime
             when : not : null? modes
               pretty-print
                  stats-put
                   time-put
                        map KSK-for-put days-before
               pretty-print
                  stats-get
                   time-get
                        map KSK-for-get days-before

      pretty-print get-stats
      pretty-print put-stats
      stats->csv get-stats #:target-filename "fetchpull-stats-get.csv"
      stats->csv put-stats #:target-filename "fetchpull-stats-put.csv"

