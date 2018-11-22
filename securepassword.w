#!/usr/bin/env bash
# -*- wisp -*-
guile -L $(dirname $(realpath "$0")) -c '(import (language wisp) (language wisp spec))'
exec -a "$0" guile -L $(dirname $(realpath "$0")) --language=wisp -x .w -e '(securepassword)' -c '' "$@"
; !#

;; Create secure passwords, usable on US and German keyboards without problems

define-module : securepassword
              . #:export : letterblocks-nice main

import
    only (srfi srfi-27) random-source-make-integers
      . make-random-source random-source-randomize!
    only (srfi srfi-1) first second third iota
    srfi srfi-11 ;; let-values
    srfi srfi-42
    ice-9 optargs
    ice-9 format
    only (ice-9 rdelim) read-line
    ice-9 match
    ice-9 pretty-print


;; newbase60 without yz_: 54 letters, 5.75 bits of entropy per letter.
define qwertysafeletters "0123456789ABCDEFGHJKLMNPQRTUVWXabcdefghijkmnopqrstuvwx"
;; delimiters: 2.3 bits of entropy per delimiter, in the same place on main keys or the num-pad.
define delimiters ".+-="

define random-source : make-random-source
random-source-randomize! random-source


define random-integer 
       random-source-make-integers random-source


define : randomletter letters
      string-ref letters
        random-integer
          string-length letters


define : letter-index letters letter
    string-index letters letter

define : block-value letterblock letters
    let loop
        : rest letterblock
          value 0
        if : equal? "" rest
           . value
           loop
               string-drop rest 1
               + : * (string-length letters) value
                 letter-index letters : string-ref rest 0

define : checkchar letters delimiters . letterblocks
    let*
        : value : block-value (apply string-append letterblocks) letters
          modvalue : string-length delimiters
          checksum : modulo value modvalue
        string-ref delimiters checksum


define : flatten e
    cond 
       : pair? e
         ` 
           ,@ flatten : car e 
           ,@ flatten : cdr e
       : null? e
         list
       else 
         list e



define : blocks-to-passphrase blocks
    let check
        : passphrase ""
          blocks blocks
        cond
         : null? blocks
             . passphrase
         {(length blocks) = 1}
             string-append passphrase : first blocks
         else
             check
                 string-append passphrase
                     first blocks
                     string
                         checkchar qwertysafeletters delimiters
                             first blocks 
                             second blocks
                 cdr blocks


define : single-block
    apply string-append
        map : λ (x) : string : randomletter qwertysafeletters
            iota 4


define : letterblocks blockcount
    let loop
        : remaining blockcount
          blocks '()
        if : zero? remaining
           blocks-to-passphrase : reverse blocks
           loop
               - remaining 1
               cons (single-block) blocks


define : letterblock-invalid? password
    let loop
        : rest password
          count 5
        if {(string-length rest) < 5}
           values #f #f #f
           let*
               : check : string : string-ref rest 4
                 block1 : string-take rest 4
                 block2
                     string-take (string-drop rest 5)
                         min 4 : - (string-length rest) 5
                 calck : string : checkchar qwertysafeletters delimiters block1 block2
               if : not : equal? check calck
                  values check calck count
                  loop : string-drop rest 5
                         + count 5


define : lines-from-file filepath
   let : : port : open-input-file filepath
     let reader : : lines '()
        let : : line : read-line port
           if : eof-object? line
              reverse! lines
              reader : cons line lines

define : split-corpus-line line
       . "turn LINE into '(first-letter second-letter weight)

A LINE is formatted as cost ab, with cost a number and a and b the letters. For example:
10123151.392154863 en
0.020499130776997592 q6
"
       define : log2 number
                / (log number) (log 2)
       let*
          : space-index : string-index line #\space
            weight : log2 : string->number : string-take line space-index
            first-letter : string-ref line : + space-index 1
            second-letter : string-ref line : + space-index 2
          list first-letter second-letter weight
             

define : shift-and-scale-cost line-costs upper-limit
   . "shift the COST to have cost values between zero (log-scale) and the UPPER-LIMIT."
   let* 
       : numbers : map (λ (x) (third x)) line-costs
         minimum : apply min numbers
         shifted-up : map (λ (x) (list (first x) (second x) {(third x) - minimum})) line-costs
         shifted-numbers : map (λ (x) (third x)) shifted-up
         maximum : apply max shifted-numbers
         scaling-factor {upper-limit / maximum}
       format #t "Scaling factor: ~a\n" scaling-factor
       map : λ(x) (list (first x) (second x) (inexact->exact (floor (* scaling-factor (third x)))))
           . shifted-up


define : collapse-weighting letters cost
   define : index-value char
            string-index letters char
   let : : collapsed : make-string (expt (string-length letters) 2) (string-ref letters 0)
       let update-string : : cost cost
         if : null? cost
           . collapsed
           let*
              : element : car cost
                one : first element
                two : second element
                val : third element
                idx : + (index-value two) {(index-value one) * (string-length letters)}
              string-set! collapsed idx val
              update-string : cdr cost

define weightletters : string-append qwertysafeletters delimiters " "
define scalingfactor-inverse {1 / 2.00834859456416}
define weight-collapsed "rmjjkkmkjjNTRRPPULMWRWMNFMNRUTJXTXWfWUQMUWdVUVFTeURJQapTiUsommkkmkkmpUTTVQQTTFKPTNPDHRPLRHaXXWXWaUJFUVTLWQXaRNPfUmVjXoqjjjjkijiiQTVVQQUKKRMTHPHNPLUPQWabXaXaQQJRXWUdRUcaPQQbkWgXomiiiiiihiiRPQWNUQJKMVQML8UPJMHQWXWaVWXLQFUaTLLBVWWRFQTjXiWnkhhihihhihNLMMPNNLTMVTHJGUNKKPKTaWTVUVQGURTMRVAQUURGPVjagTnmhhhhhihhhQQPLMTKQHNHVPTRQLPUQJUTXWVXNMMLQWQGQLKaQQHLQiQgNokhhhhhmhhgURNQHKQNFLKNJLCGGRKHKUTXUXTQRJTPTLFQQGVNAULQiRgQojhhhhihhhhMMJRJQPGGFFQGLJHPHFMJUUVWVWJLDFKTQQKKRUPGCJQhPeMnkhhhhhihhiKNLLJLQPKPNQFJCPKHLGGVVVWVXMMQHTRbDP8GUNBKLThPfMmkiijjjjjknPNHJQKJFFQHTEPJGLHGKMVVTUTUGNHELTGAFLRUP9NNQiQfKmLXUVVURNQMcbddbafVUadefdWffaXVXcogiagfeeUikqTiapjjqcaabPdNkPRURQPRQRQdeaWaWaRRXabgUNbUbaVJpLRbsXQUnbHQKnHRnVRoQNMfHbLhRTVUPQPKRLcVcgjVVeTbbaVaTbcVbRPmJLPfTGneHKJNnL6gXQfKMHbWcPiQUTPRRPQKMdXVegeWXWUbbfdWeXecWTrQHbrLLUrVKUUnQ6kcbkVQKbMcUiQQPRQLLMMRdadebabVMWccfcTghddfbdifgUfcgqNahoUdaqnfmfajaMeXgRWTPMTRQPLcabdabUTMUbVRWGaaaTUPoMNHnTNNoRRNTnDJpUTjQLGWFbKfHULMMNMPTQacWUfVacHRaXfgKcabUWHkUDRrRPcjNDdfkJFqWNiPRFaLaThTPVMLPNNMNdUXXdVdWMVbVTVQWfWUUJqNJPpPWJoMEaKnKLcQNkMRBXJVQeDJKFFFEGLRTNPRTNRRVRLNLWPRPQRQJpRMJjPTUbDKHGkTFTTJmFECaCRDaLPNQJMKJHGaVQdbUXTNVXWTfNTVXUXNoPPNmVManQJVhpTJnTPmQUEaQXMfRPQLUNKHFGgWXXeacQMVbUUaKUaWVTNoNRTpPRNpTWEJkL4TWUjdMQaPbQhJRRXXNNLMNeaWbeTWXQTcaRbTUXXVbVrTcRpTQQqKTLVoVQiUPmHNHaJbLgLQULNHHJPKeaceeaeVPaVUdaLXhgXXMpHKQnRRPkMNQQoNQbQXjRNFaKXViMTUTPLLJTHeQcheUeWPVgXWdPcbbTQLoMMUmiFkiUWMUnRQqfVjVNDXNbLgHLGGJLRFCJWJLJVHKDHAWNTHQWVWRWRVDNFN7PEUNJNGPP6ELXmEbAQ7RDaQVRNPKNJJHeVddgXXQMWcbbdLceVaaMnTQRqLTfkLKHQmGLMRPkFMPaKbRhPVTTQNMJMTeVcafbUeUVacXeHceacUToRdToMQqkUNTbnRRoeQhRcgXJaNgXUePNQLKKHXVXWaUaRHVbXeWCdeQUQQMVTaXbWiUMXmqRdEifaNLbNWMdEhPRQLLKPKPFXWcacRaRNQXVbXDVRRWXKjHJRqaVHmKKJJoHJUQPaVMDbEbUgFQNQHQUELEbUTWeVUUTTVcUaLVVQRVPoGKNrLMjpNPNKnMLcURiFQFXWWHfJXLPKNNKKDQLUPUQKMLKUWHUDNXLUFbXTQQWPJMW6MRFVTLQLQQTJQUFXGeUdbXbUVVWTQUTWMVPQGQQQdQQUWDMPFnuusksutrhru.gqbxxxwpkjiRmatVcbXVWVWXVLNQGMTMdKNQNLLDNJBLJXsmfh.gmisifgirfWrqorfiUhQfXpXXbVXTUVTVRNXVTMUJKVVTQbQKTQMLBrbomrec=oatidtjdomqoidbfMhanVdaccacWRWWUUVaTMTJUWRXVTQbVRRDwjgo-jjjxehjmsibrposjmenWkdxXjgedabaWWWkXWdaVbPVVXVXQXaPTWXtttvssuu+gqv prk=.vtrrrqTog-TccWTUUVeWUXVQPRUJMQXTLPQVTNPR9siifusmhscfhkthcsntodienMhavPWaRURRQTWRXMTWTNNFHPURMJQUQQHFrggg+inprdnioogXsrrqfffmTkbwNUWPNMRMPETMNQUTTQMPJTQRNRRQULFwjei+hphvdmrstfXvqwqfpTkQjdxHbbRMHNLHLLcTNNPQTGNRQMVHPQQPTLrr.t+svrheru-upeux.krfkiViVsBMPNNM6C6JQFQFABCFR56KJJNJ6RCNJnaUbqcWUdaaUXjecVaVnWWQULVNeQRVUTKNJRGQVWLTQHRQNLVMPLQUTPWLsgekviihpXmjorfWpospihdjUjWrabbbWVQQTRVMQNTNTQKQRVNQMRKPNQBvodjwjievbjtirsceppqghXjJiVwVabhRJRPVNWVVWaXaTURVTWWNWXTUbLupr+.r.ovhsnvujfmvxsnmesVnd=URbbRLQKeFXRaUWMQTPUTPLQPWbHUPXmqtsktqqnjouxqrbxsttorkjNkUucefbdUQUNRRKLQXRPTKLDJTFRLTDTNFsdfjtpgoqVgigrqbtnqpadbhNjepDRVRHM04L4UQRQQQU3JF6QQ54RQF94MWacafcXXXbRWaXchbbdpWeUcFPRgQhebbaaQRUWWbTTPQUQPPUQVFWRMRWLwrsv+rsqvgsruvndrvwtnpboRme+bcdVKTQPQNVXVVTVVUKNUWQWLRPTRVKtnxk.mprwfpokutfnw.rmofpWng+XdfVXQNNTPaXWXbUXXTRWVUWQWbUQVQumnj-mnwwdknmunbuuutiqkrang+JQUQJKXLBHGMLeLNQLJNKQHbJQTLJKEoptpsurmpdmt.iqXwwvhhhhgJgLtcaecTJWREBHGLLMUQUFDHUDWPKQLWRFpbfdvdiXrVWehucaihddhWXeGjcmRWVbP8fR98MRTVRNTTRMUKDLHTVLRUMucegvdeqvdbdmsdbmkfqXidhTgTpeibXTVWPbWQRNaVTTPTNPNQR4PQDGKPjjkcjjbekbhgdimafnofhdbfUhbmomjiihiggghfajgcccbbaddbNbbddePigjihghgffdgfgkbhjhgchbmQfHvjebXWUTMNQPLQLMMQLNHNXJLCMRFKKKQTPNNRPMPKQUQKQMQVWJNPTNdXgjkmkhheedffjjhihiihgihjfibiifhiakkkiijijihgkihkckmkhigceUqaqacaUUUUTQPNNWQTdTNMKNNfRULdQNQLhWbjXagXWRfifbaTbgeWVcfJJWkrqsronmmjkjuuquttttrtsurtkstrstg.wu+xvvvxrtwvvtmt.xwvxjjnrr="


define : weighting-from-corpusfile filename
   . "this is how the weighting above was calculated"
   let*
       : cost : map split-corpus-line : lines-from-file filename
         letters weightletters
         shifted : shift-and-scale-cost cost : - (string-length letters) 1
       collapse-weighting letters
           map : λ (x) (list (first x) (second x) (string-ref letters (third x)))
               . shifted

define : recreate-corpus-from-weighting
       . "Expand the weight string back into a full corpus by using the weightletters"
       let expander
           : weightleft weight-collapsed
             letter1 weightletters
             letter2 weightletters
           cond
             : string-null? weightleft
               . #t
             : string-null? letter1
               error "could not expand the whole corpus. Letters left: ~a" weightleft
             : string-null? letter2
               expander weightleft (string-drop letter1 1) weightletters
             else
               let : : cost : expt 2 : * scalingfactor-inverse : string-index weightletters : string-ref weightleft 0
                   format #t "~f ~a~a\n" cost (string-ref letter1 0) (string-ref letter2 0)
               expander
                  string-drop weightleft 1
                  . letter1
                  string-drop letter2 1

define : bigram->weight bigram
       . "Calculate the weight of the given bigram in a corpus"
       let*
           : letter1 : string-ref bigram 0
             letter2 : string-ref bigram 1
             idx1 : string-index weightletters letter1
             idx2 : string-index weightletters letter2
             ;; for downcased bigrams (for phonetics) we might have to get the uppercase version
             idx1 : if idx1 idx1 : string-index weightletters : char-upcase letter1
             idx2 : if idx2 idx2 : string-index weightletters : char-upcase letter2
             len : string-length weightletters
             costchar : string-ref weight-collapsed {{idx1 * len} + idx2}
           expt 2 : * scalingfactor-inverse : string-index weightletters costchar

define : word-weight word
       . "calculate the probability weight of the given word to appear in a corpus given by the weight-collapsed"
       let loop
           : s : string-append " " word " "
             cost 0
           cond
             : string-null? : string-drop s 1
               . cost
             : string-null? : string-drop s 2
               . cost
             else
               loop 
                   string-drop s 2
                   + cost : bigram->weight : string-take s 2


define* : string-replace-substring s substr replacement #:optional (start 0) (end (string-length s))
       . "Replace every instance of substring in s by replacement."
       let : : substr-length : string-length substr
          if : zero? substr-length
             error "string-replace-substring: empty substr"
             let loop
                 : start start
                   pieces : list : substring s 0 start
                 let : : idx : string-contains s substr start end
                   if idx
                     loop : + idx substr-length
                           cons* replacement
                                  substring s start idx
                                  . pieces
                     string-concatenate-reverse
                                                cons : substring s start
                                                    . pieces


define* : letterblocks-nice blockcount #:key (best-of 8)
     . "Generate BEST-OF letterblocks and return the one most likely to appear in the corpus given by weight-collapsed

best-of 8 consumes 3 bits of entropy, but creates passwords which are easier to remember. "
     define : delimiters-to-space s
            . "replace all delimiters by spaces"
            let replace
              : s s
                delim delimiters
              if : string-null? delim
                . s
                replace
                    string-replace-substring s (string-take delim 1) " "
                    string-drop delim 1
     car
         sort
             map : λ (x) (letterblocks blockcount)
                 iota best-of
             λ (a b)
                  > 
                      word-weight : delimiters-to-space : string-downcase a
                      word-weight : delimiters-to-space : string-downcase b


define : help args
       format #t "Usage: ~a [options]

Options:
  [<length> [<password-type>]]            create password
   --check <password>                     verify the checksums
   --help                                 show this message
" : first args

define : main args
 cond
   : and {(length args) > 1} : equal? "--help" : second args
     help args
   : and {(length args) > 2} : equal? "--check" : second args
     let-values : : (check calck count) : letterblock-invalid? : third args
        cond 
            count
                format #t "letterblock invalid. First failed checksum: ~a should have been ~a at position ~a\n"
                    . check calck count
                exit 1
            else
                format #t "valid letterblock password\n"
   else
     let
      :
        len
          if : <= 2 : length args
             string->number : second args
             . 12
      display : letterblocks-nice : floor {len / 4}
      newline

