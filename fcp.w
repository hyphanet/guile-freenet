#!/usr/bin/env bash
# -*- wisp -*-
# A Freenet Client Protocol library for Guile Scheme.

guile -L $(dirname $(realpath "$0")) -c '(import (language wisp spec))'
PROG="$0"
if [[ "$1" == "-i" ]]; then
    shift
    exec -a "${PROG}" guile -L $(dirname $(realpath "$0")) --language=wisp -x .w -e '(fcp)' -- "${@}"
else
    exec -a "${0}" guile -L $(dirname $(realpath "$0")) --language=wisp -x .w -e '(fcp)' -c '' "${@}"
fi;
; !#

;; for emacs (defun test-this-file () (interactive) (save-current-buffer) (async-shell-command (concat (buffer-file-name (current-buffer)) " --test")))

define-module : fcp
    . #:export
    main 
      . message-create message-task message-type message-data message-fields
      . message-client-get  message-client-get-realtime message-client-get-bulk 
      . message-client-put message-client-put-realtime message-client-put-bulk 
      . message-remove-request
      . send-message processor-put! processor-delete!
      . printing-passthrough-processor printing-discarding-processor 
      . discarding-processor processor-nodehello-printer
      . processor-datafound-getdata 
      . task-id
      . call-with-fcp-connection with-fcp-connection

define version "0.0.0 just-do-it"

import
    only (srfi srfi-19) current-date date->string string->date date->time-utc time-utc->date
                      . make-time time-utc time-duration add-duration current-time
    only (securepassword) letterblocks-nice
    only (srfi srfi-9) define-record-type
    only (srfi srfi-9 gnu) set-record-type-printer!
    only (ice-9 pretty-print) pretty-print truncated-print
    only (ice-9 rdelim) read-line read-delimited
    only (ice-9 format) format
    only (srfi srfi-1) first second third alist-cons assoc lset<= lset-intersection lset-difference take
    only (rnrs bytevectors) make-bytevector bytevector-length string->utf8 utf8->string bytevector?
    only (rnrs io ports) get-bytevector-all get-bytevector-n
         . put-bytevector bytevector->string port-eof?
    only (ice-9 popen) open-output-pipe
    only (ice-9 regex) string-match match:substring
    ice-9 threads
    ice-9 atomic
    only (ice-9 q) make-q enq! deq! q-empty?
    sxml simple
    doctests

define : string-replace-string s char replacement-string
    string-join (string-split s char) replacement-string
define : replace-KSK-escaped s
    string-replace-string : string-replace-string s #\+ "-"
        . #\= "-"

define : task-id
    replace-KSK-escaped : letterblocks-nice 6

;; the shared FCP socket
define sock #f
define ip "127.0.0.1"
define port "9483"


define : fcp-socket-create
    define addrs : getaddrinfo ip port
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

;; use a custom printer which avoids printing the full data
set-record-type-printer! <message>
  lambda : record port
    format port "#<<message> task: ~A type: ~A data: ~a, fields: ~A"
        message-task record
        message-type record
        if : bytevector? : message-data record
             format #f "length=~a" : bytevector-length : message-data record
             message-data record
        message-fields record


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
         atomic-box-set! sending-message #f
         ;; avoid overloading the node ;; FIXME: is this actually needed? Just added because it might fix crashes.
         usleep 1000 ;; max of 1000 messages per second



define : message-client-hello
    message-create #f 'ClientHello #f
        list : cons 'Name "FetchpullClient" 
               cons 'ExpectedVersion "2.0"

define : message-watch-global
    message-create #f 'WatchGlobal #f
        list : cons 'Enabled "true" 
               cons 'VerbosityMask 0 ;; simple progress

define : message-disconnect
    message-create #f 'Disconnect #f
        list

define : message-client-get task URI custom-fields
    ;; https://github.com/freenet/wiki/wiki/FCPv2-ClientGet
    message-create task 'ClientGet #f
        append
          list : cons 'URI URI
          ' : Verbosity . 0 ;; only be informed when the download is finished
              ReturnType . direct
              Global . true
              Persistence . reboot
          . custom-fields

define : message-client-get-realtime task URI
    message-client-get task URI
        '
          PriorityClass . 2
          RealTimeFlag . true
          FilterData . false
          MaxRetries . 0

define : message-client-get-bulk task URI
    message-client-get task URI
        '
          PriorityClass . 3 ;; medium
          RealTimeFlag . false
          FilterData . false
          MaxRetries . 1 ;; -1 means: try indefinitely, with ULPR, essentially long polling

define : message-client-put task URI data custom-fields
    ;; https://github.com/freenet/wiki/wiki/FCPv2-ClientPut
    message-create task 'ClientPut data
        append
          list : cons 'URI URI
          ` : Global . true
              Persistence . reboot
              UploadFrom . direct
          . custom-fields

define : message-client-put-realtime task URI data
    message-client-put task URI data
        '
          PriorityClass . 2
          MaxRetries . 0 ;; default: 10
          RealTimeFlag . true
          DontCompress . true
          ExtraInsertsSingleBlock . 0
          ExtraInsertsSplitfileHeaderBlock . 0
          ;; for realtime do NOT send Metadata.ContentType (or set it
          ;; to "" -> Metadata.isTrivial()), else you force at least
          ;; one level redirect.

define : message-client-put-bulk task URI data
    message-client-put task URI data
        '
          PriorityClass . 3 ;; medium
          RealTimeFlag . false
          DontCompress . false

define : message-remove-request task
    message-create task 'RemoveRequest #f
            list : cons 'Global 'true


define supported-messages
    ' NodeHello GetFailed DataFound AllData PutSuccessful PutFailed

define ignored-messages ;; TODO: implement support for these messages
    ' CompatibilityMode ExpectedDataLength ExpectedHashes ExpectedMIME PersistentGet PersistentPut SendingToNetwork SimpleProgress URIGenerated PersistentRequestRemoved

define : log-warning message things
         format : current-output-port
             . "Warning: ~a: ~a\n" message things

define : read-message port
  if : or (port-closed? port) (port-eof? port)
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
              : member type supported-messages
                let
                    : 
                      data ;; EndMessage has no Data
                          if : and DataLength : not : equal? "EndMessage" line
                              get-bytevector-n port (string->number DataLength)
                              . #f
                    message-create task type data
                            map field-split : cdr lines
              else
                    when : not : member type ignored-messages
                           log-warning "unsupported message type" : list type lines
                    if : port-eof? port
                        . #f
                        loop : string->symbol : read-line port

define next-message
    make-atomic-box #f
define sending-message
    make-atomic-box #f

define : send-message message
    ;; wait until the message was retrieved. This only replaces if the previous content was #f. take-message-to-send switches takes the messages
    let try : : failed : atomic-box-compare-and-swap! next-message #f message
        when failed
            usleep 100
            try : atomic-box-compare-and-swap! next-message #f message

define : take-message-to-send
    ;; get the message and reset next-message to #f to allow taking another message
    atomic-box-set! sending-message #t ;; set to false again after successful write-message
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

define stop-fcp-threads #f

define : fcp-read-loop sock
    let loop : : message : read-message sock
        when message
            warn-unhandled
                process message
        usleep 10
        when : not stop-fcp-threads
            loop : read-message sock

define : fcp-write-loop sock
    let loop : : message : take-message-to-send
        if message
          write-message message sock
          begin
              atomic-box-set! sending-message #f
              usleep 10
        when : not stop-fcp-threads
            loop : take-message-to-send

define : warn-unhandled message
    when message
        format #t  ;; avoid writing to the error port elsewhere, that causes multithreading problems. Use current-output-port instead
            . "Unhandled message ~a: ~A\n" 
            message-type message
            . message
    . #f

define : printing-passthrough-processor message
    pretty-print message
    . message

define : printing-discarding-processor message
    pretty-print message
    . #f

define : discarding-processor message
    . #f

define : processor-nodehello-printer message
    cond
      : equal? 'NodeHello : message-type message
        pretty-print message
        . #f
      else message


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
define all-found-data-tasks : list


define : processor-datafound-getdata message
    cond
      : equal? 'DataFound : message-type message
        pretty-print message
        when : not : member (message-task message) all-found-data-tasks
            send-message
                message-create : message-task message
                    . 'GetRequestStatus #f
                    list : cons 'Global 'true
            set! all-found-data-tasks 
                cons : message-task message
                       take all-found-data-tasks : min 100 : length all-found-data-tasks
        . #f
      else message

define : processor-record-datafound-time message
    cond
      : equal? 'DataFound : message-type message
        let : : task : message-task message
          when : not : assoc task get-successful ;; only add if not yet known
            set! get-successful
              alist-cons task (current-time-seconds) get-successful
        . #f
      else message

define : current-time-seconds
    car : gettimeofday

define : processor-record-alldata-time message
    cond
      : equal? 'AllData : message-type message
        let : : task : message-task message
          when : not : assoc task get-successful ;; only add if not yet known
            set! get-successful
              alist-cons task (current-time-seconds) get-successful
        . #f
      else message


define : processor-record-getfailed-time message
    cond
      : equal? 'GetFailed : message-type message
        let : : task : message-task message
          when : not : assoc task get-failed ;; only add if not yet known
            set! get-failed
              alist-cons task (current-time-seconds) get-failed
        . #f
      else message

define : processor-record-putfailed-time message
    cond
      : equal? 'PutFailed : message-type message
        let : : task : message-task message
          when : not : assoc task put-failed ;; only add if not yet known
            set! put-failed
              alist-cons task (current-time-seconds) put-failed
        . #f
      else message

define : processor-record-putsuccessful-time message
    cond
      : equal? 'PutSuccessful : message-type message
        let : : task : message-task message
          when : not : assoc task put-successful ;; only add if not yet known
            set! put-successful
              alist-cons task (current-time-seconds) put-successful
        . #f
      else message

define : processor-record-identifier-collision-put-time message
    cond
      : equal? 'IdentifierCollision : message-type message
        let : : task : message-task message
          when : not : assoc task put-failed ;; only add if not yet known
            set! put-failed
              alist-cons task (current-time-seconds) put-failed
        . #f
      else message


define-record-type <duration-entry>
    duration-entry key duration successful operation mode
    . timing-entry?
    key duration-entry-key
    duration duration-entry-duration
    successful duration-entry-success
    operation duration-entry-operation ;; get or put
    mode duration-entry-mode ;; realtime bulk speehacks


define timeout-seconds : * 3600 3 ;; 3 hours maximum wait time

define : timeout? timeout-seconds start-times
    and : not : null? start-times
          pair? : car start-times
          > : - (current-time-seconds) timeout-seconds
              cdr : car start-times

define : remove-all-keys keys
    define : remove-key key
             send-message
                 message-remove-request key
    map remove-key keys



define %this-module : current-module
define : test
    processor-put! printing-discarding-processor
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
       processor-delete! printing-discarding-processor
       close sock

define : call-with-fcp-connection thunk
    set! sock : fcp-socket-create
    set! stop-fcp-threads #f
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
       while : or (atomic-box-ref next-message) (atomic-box-ref sending-message)
           format #t "waiting for message to be sent: next-message: ~a , sending: ~a\n" (atomic-box-ref next-message) (atomic-box-ref sending-message)
           sleep 1
       send-message : message-disconnect
       set! stop-fcp-threads #t
       sleep 3
       close sock
       join-thread fcp-write-thread : + 3 : current-time-seconds
       join-thread fcp-read-thread : + 3 : current-time-seconds

;; FIXME: using new fcp connections in sequential code-parts fails with
;;        ERROR: In procedure display: Wrong type argument in position 2: #<closed: file 7f106e118770>
;;        ERROR: In procedure fport_read: Die Verbindung wurde vom Kommunikationspartner zurückgesetzt
;;        therefore you should only use a single FCP connection for your program.
define-syntax-rule : with-fcp-connection exp ...
    call-with-fcp-connection
        λ () exp ...


define : final-action? args
   if {(length args) <= 1} #f
     cond 
       : equal? "--help" : second args
         help args
         . #t
       : equal? "--version" : second args
         format : current-output-port
                . "~a\n" version
         . #t
       : equal? "--test" : second args
         test
         . #t
       else #f
       
    
define : main args
  define put-task : task-id
  define get-task : task-id
  define key : string-append "KSK@" put-task
  define successful #f
  define : request-successful-upload message
    cond
        : equal? 'PutSuccessful : message-type message
          let : : fields : message-fields message
              when : and=> (assoc 'URI fields) : λ (uri) : equal? key : cdr uri
                  pretty-print message
                  send-message
                      message-client-get-realtime get-task key
                  send-message
                      message-remove-request : message-task message
              . #f
        else message
  define : record-successful-download message
    cond
        : equal? 'AllData : message-type message
          let : : task : message-task message
              when : equal? task get-task
                  pretty-print message
                  display "Data: "
                  truncated-print : utf8->string (message-data message)
                  newline
                  set! successful #t
                  send-message
                      message-remove-request task
              . #f
        else message
  ;; standard processorrs
  processor-put! printing-discarding-processor
  processor-put! processor-nodehello-printer
  ;; immediately request data from successfull get requests
  processor-put! processor-datafound-getdata
  ;; custom processors
  processor-put! request-successful-upload
  processor-put! record-successful-download
  when : not : final-action? args
    with-fcp-connection
        ;; get the ball rolling
        send-message
            message-client-put-realtime put-task key
                string->utf8 : string-append "Hello " key
        while : not successful
            display "."
            sleep 10
