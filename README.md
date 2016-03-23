Guile Freenet
=============

Tools to interact with Freenet from Guile Scheme

Usage
-----

### fcp.scm 

fcp.scm shows an example of interfacing with Freenet using [FCP][]. fcp.rkt is a version of the same written for racket.

[FCP]: https://wiki.freenetproject.org/FCPv2 "Freenet Client Protocol"

### crawl the social graph

You can get a snapshot of the web of trust graph using

- crawl-wot.scm
- parse-crawled.scm
- deduplicate-csv.scm
- anonymize-csv.scm

Simply execute them in order to get the file trust-anonymized.csv

License: LGPL
-------------

All these programs are provided under the LGPL: You can copy, share, change and sell them, as long as you attribute the authors and enable others to do the same with your creation. See LICENSE for details.
