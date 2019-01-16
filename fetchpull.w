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
    only (srfi srfi-9 gnu) set-record-type-printer!
    only (ice-9 pretty-print) pretty-print
    only (ice-9 rdelim) read-line read-delimited
    only (ice-9 format) format
    only (srfi srfi-1) first second third alist-cons assoc lset<= lset-intersection lset-difference
    only (rnrs bytevectors) make-bytevector bytevector-length string->utf8 bytevector?
    only (rnrs io ports) get-bytevector-all get-bytevector-n
         . put-bytevector bytevector->string port-eof?
    only (ice-9 popen) open-output-pipe
    only (ice-9 expect) expect-strings ;; for quick experimentation. Expect needs additional functions and variables available:
        .  expect expect-regexec expect-timeout expect-select expect-timeout-proc
        .  expect-char-proc expect-eof-proc expect-strings-compile-flags
    only (ice-9 regex) string-match match:substring
    ice-9 threads
    ice-9 atomic
    only (ice-9 q) make-q enq! deq! q-empty?
    sxml simple
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
    define addrs : getaddrinfo "127.0.0.1" "9483"
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
         ;; avoid overloading the node
         usleep 1000 ;; max of 1000 messages per second



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

define : message-client-get-bulk task URI
    message-client-get task URI
        '
          PriorityClass . 3 ;; medium
          RealTimeFlag . false
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
    ' CompatibilityMode ExpectedDataLength ExpectedHashes ExpectedMIME PersistentGet PersistentPut SendingToNetwork SimpleProgress URIGenerated

define : log-warning message things
         format : current-output-port
             . "Warning: ~a: ~a\n" message things

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
                           log-warning "unsupported message type" type
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

define : fcp-read-loop sock
    let loop : : message : read-message sock
        when message
            warn-unhandled
                process message
        loop : read-message sock

define : fcp-write-loop sock
    let loop : : message : take-message-to-send
        if message
          write-message message sock
          usleep 100
        loop : take-message-to-send

define : warn-unhandled message
    when message
        format : current-error-port  ;; avoid writing to the error port elsewhere, that causes multithreading problems. Use current-output-port instead
            . "Unhandled message ~a: ~a\n" 
            message-type message
            message-task message
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

define : processor-datafound-getdata message
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


define : generate-data seed-string size-bytes-min
    . "permutate the seed-string randomly to generate bulk data of at least size-bytes-min"
    define required-permutations {size-bytes-min / (string-length seed-string)}
    define chars : string->list seed-string
    define compare : λ (a b) : < (car a) (car b)
    define data
        let loop : : permutations : list chars
            if {(length permutations) >= required-permutations} permutations
               loop
                   cons
                       map cdr
                         sort
                           map : λ (x) : cons (random 1.0) x
                               . chars
                           . compare
                       . permutations
    string->utf8 : string-join (map list->string data) "\n"


define-record-type <duration-entry>
    duration-entry key duration successful operation mode
    . timing-entry?
    key duration-entry-key
    duration duration-entry-duration
    successful duration-entry-success
    operation duration-entry-operation ;; get or put
    mode duration-entry-mode ;; realtime bulk speehacks


define timeout-seconds : * 3600 6 ;; 6 hours maximum wait time

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

define* : time-get mode keys
    define start-times : list
    define : get-message key
        if : equal? mode 'realtime
             message-client-get-realtime key key
             message-client-get-bulk key key
    define : finished-tasks
        append
            map car get-successful
            map car get-failed
    ;; cleanup old state
    remove-all-keys keys
    set! get-successful : list
    set! get-failed : list
    ;; setup a processing chain which saves the time information about the request
    processor-put! processor-datafound-getdata
    processor-put! processor-record-alldata-time
    processor-put! processor-record-getfailed-time
    ;; just use the keys as task-IDs (Identifiers)
    let loop : (keys keys)
        when : not : null? keys
            ;; first remove requests which might still be in the upload or download queue
            send-message
               message-remove-request : first keys
            set! start-times : alist-cons (first keys) (current-time-seconds) start-times
            ;; now request the data
            send-message
                get-message (first keys)
            loop (cdr keys)
    ;; wait for completion
    let loop : (finished (finished-tasks))
        when : not : lset<= equal? keys finished
            format #t "debug: lset-intersection equal? keys finished: ~A -> ~a, keys -> ~a, get-successful -> ~a, get-failed -> ~a\n" finished (length finished) (length keys) (length get-successful) (length get-failed)
            let : : unfinished : lset-difference equal? keys : lset-intersection equal? keys finished
                format : current-output-port
                    . "~d download keys still in flight\n" (length unfinished)
                cond 
                  : timeout? timeout-seconds start-times
                    map ;; fail all unfinished
                        λ : key
                            send-message
                                message-remove-request key
                            set! get-failed
                                alist-cons key (current-time-seconds) get-failed
                        . unfinished
                  else
                    usleep 1000000
                    loop (finished-tasks)
    ;; all done: cleanup and take the timing
    processor-delete! processor-record-getfailed-time
    processor-delete! processor-record-alldata-time
    processor-delete! processor-datafound-getdata
    remove-all-keys keys
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
                 cons : duration-entry (first keys) {finish-time - start-time} successful 'GET mode
                      . times

define : time-put mode keys
    define 80Bytes 80
    define 1MiB : expt 2 20 ;; 1 MiB are about 40 blocks
    define 128kiB : expt 2 17 ;; 128 kiB are about 4 blocks
    define start-times : list
    define : put-message key
        cond
           : equal? mode 'realtime
             message-client-put-realtime key key : generate-data key 80Bytes
           : equal? mode 'small
             message-client-put-bulk key key : generate-data key 128kiB
           else
             message-client-put-bulk key key : generate-data key 1MiB
    define : finished-tasks
        append
            map car put-successful
            map car put-failed
    ;; cleanup old state
    remove-all-keys keys
    set! put-successful : list
    set! put-failed : list
    ;; setup a processing chain which saves the time information about the request
    processor-put! processor-record-putsuccessful-time
    processor-put! processor-record-putfailed-time
    processor-put! processor-record-identifier-collision-put-time
    ;; insert all files, using the keys as task-IDs (Identifiers)
    let loop : (keys keys)
        when : not : null? keys
            ;; first remove requests which might still be in the upload or download queue
            send-message
               message-remove-request : first keys
            set! start-times : alist-cons (first keys) (current-time-seconds) start-times
            ;; now insert the data
            send-message
                put-message (first keys)
            loop (cdr keys)
    ;; wait for completion
    let loop : (finished (finished-tasks))
        when : not : lset<= equal? keys finished
            let : : unfinished : lset-difference equal? keys : lset-intersection equal? keys finished
                format : current-output-port
                    . "~d upload keys still in flight\n" (length unfinished)
                cond 
                  : timeout? timeout-seconds start-times
                    map 
                        λ : key
                            send-message
                                message-remove-request key
                            set! put-failed
                                alist-cons key (current-time-seconds) put-failed
                        . unfinished
                  else
                    usleep 1000000
                    loop (finished-tasks)
    ;; all done: cleanup and take the timing
    processor-delete! processor-record-identifier-collision-put-time
    processor-delete! processor-record-putfailed-time
    processor-delete! processor-record-putsuccessful-time
    remove-all-keys keys
    let loop : (keys keys) (times '())
        if : null? keys
           . times
           let :
             define key : first keys
             define (gettime L) : cdr : assoc key L
             define start-time : gettime start-times
             define finish-time : gettime : append put-successful put-failed
             define successful : and (assoc key put-successful) #t ;; forces boolean
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
       while : or (atomic-box-ref next-message) (atomic-box-ref sending-message)
           format #t "waiting for message to be sent\n"
           usleep 100
       send-message : message-disconnect
       join-thread fcp-write-thread : + 3 : current-time-seconds
       join-thread fcp-read-thread : + 3 : current-time-seconds
       close sock

;; FIXME: using new fcp connections in sequential code-parts fails with
;;        ERROR: In procedure display: Wrong type argument in position 2: #<closed: file 7f106e118770>
;;        ERROR: In procedure fport_read: Die Verbindung wurde vom Kommunikationspartner zurückgesetzt
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


define : website-content port
  define title "Fetch-Pull-Stats re-woven"
  sxml->xml
    ` *TOP*
        html
           head : title ,title
                  meta : @ : charset "utf-8"
           body : h1 ,title
             p : img : @ (src "fetchpull.png") (alt "fetch-pull-statistics")
             p "created with " 
                 a : @ (href "https://bitbucket.org/ArneBab/freenet-guile/src/default/fetchpull.w") (title "link to project")
                   . "fetchpull.w"
             p "plotted with "
                 a : @ (href "fetchpull-plot.gnuplot")
                   . "fetchpull-plot.gnuplot"
             p
                 a : @ (href "/?newbookmark=USK@lwR9sLnZD3QHveZa1FB0dAHgeck~dFNBg368mY09wSU,0Vq~4FXSUj1-op3QdzqjZsIvrNMYWlnSdUwCl-Z1fYA,AQACAAE/fetchpullstats/8/&desc=fetchpullstats&hasAnActivelink=true")
                   . "bookmark this freesite"
    . port

define : create-plot
    define gnuplot : open-output-pipe "gnuplot"
    define input : open-input-file "fetchpull-plot.gnuplot"
    let loop :
        when : not : port-eof? input
            display (read-char input) gnuplot
            loop
    display #\newline gnuplot
    close input
    close gnuplot
    sync

define : copy-resources-to path
    ;; remove all KSK information from the stats to prevent people from tampering with them
    let loop : (files '("fetchpull-stats-get.csv" "fetchpull-stats-put.csv"))
        when : not : null? files
            when : file-exists? : first files
                let : : new-filename : string-append path file-name-separator-string : first files
                  copy-file : first files
                            . new-filename
                  close : open-output-pipe : string-append "sed -i 's/KSK@.*using-realtime/KEY/' " new-filename "\n"
            loop : cdr files
    ;; simply copy over the plot and plotting script
    ;; FIXME: the resulting fetchpull.png is empty
    let loop : (files '("fetchpull-plot.gnuplot" "fetchpull.png"))
        when : not : null? files
            when : file-exists? : first files
                copy-file : first files
                        string-append path file-name-separator-string
                            first files
            loop : cdr files
    sync

define : ensure-directory-exists path
    cond
       : not : file-exists? path
         mkdir path
       : not : file-is-directory? path
         error 'system-error "Selected path ~A is no directory" path
       else path

define : write-site-to path
    define filepath : string-append path file-name-separator-string "index.html"
    define port : open-output-file filepath
    display "<!doctype html>\n" port
    website-content port
    close port

define : create-site path
    ensure-directory-exists path
    create-plot
    copy-resources-to path
    write-site-to path

define : final-action? args
   if {(length args) > 1}
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
       : equal? "--site" : second args
         create-site : if {(length args) > 2} (third args) "site"
         . #t
       else #f
     . #f
       
    
define : main args
  when : not : final-action? args
    when {(length args) > 1}
         pretty-print : second args
         set! today : iso->time : second args
    ;; processor-put! printing-passthrough-processor
    let : (get-stats '()) (put-stats '())
      define : stats-get stat
          set! get-stats : append get-stats stat
          . stat
      define : stats-put stat
          set! put-stats : append put-stats stat
          . stat
      with-fcp-connection
         let loop
             : modes '(realtime small bulk)
             define days-before
                 cons 0
                     map : λ(x) : expt 2 x
                         iota 10
             define* : KSK-for-get days #:key (append "") (mode 'realtime)
                 KSK-for-request (string-append (prefix) append) today days mode
             define* : KSK-for-put days #:key (append "") (mode 'realtime)
                 KSK-for-insert (string-append (prefix) append) today days mode
             when : not : null? modes
              let : : mode : first modes
                  format #t "collecting ~a statistics\n" mode
                  stats-put
                   time-put mode
                      apply append
                        map : λ(x) : map (λ (y) (KSK-for-put y #:append (number->string x) #:mode mode)) days-before 
                              iota 5
                  stats-get
                   time-get mode
                      apply append
                        map : λ(x) : map (λ (y) (KSK-for-get y #:append (number->string x) #:mode mode)) days-before 
                              iota 5
              loop : cdr modes
      
      format #t "Finished collecting statistics\n"
      ;; pretty-print get-stats
      ;; pretty-print put-stats
      let : (get-statsfile "fetchpull-stats-get.csv") (put-statsfile "fetchpull-stats-put.csv")
        stats->csv get-stats #:target-filename get-statsfile
        format #t "Finished writing get statistics to ~a\n" get-statsfile
        stats->csv put-stats #:target-filename put-statsfile
        format #t "Finished writing put statistics to ~a\n" put-statsfile

