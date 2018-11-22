#!/usr/bin/env bash
# -*- wisp -*-
guile -L $(dirname $(realpath "$0")) -c '(import (language wisp spec))'
exec -a "$0" guile -L $(dirname $(realpath "$0")) --language=wisp -x .w -e '(fetchpull)' -c '' "$@"
; !#

define design
    ' 
        keys are KSK@<prefix>-DATE-uploaded-xxx-days-before
        process:
            get the current date
            for realtime or bulk as MODE
                for each power of two up to 128 as iii
                    insert a random chunk (without compression) to {DATE + iii days}-uploaded-iii-days-before-using-MODE
                    request the key DATE-uploaded-iii-days-before-using-MODE
                    write the times along with the keys (without the prefix) into insert-times.csv and request-times.csv
                    format as
                        DATE-as-seconds-since-epoch iii MODE
       prefix is generated from securepassword.w and stored in the file fetchpull-prefix.txt

