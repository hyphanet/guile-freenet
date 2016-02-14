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


Get the code
------------

This repository is available from multiple services: 

- Git notabug: git clone [git://notabug.org/arnebab/guile-freenet.git](https://notabug.org/arnebab/guile-freenet)
- Mercurial Bitbucket: hg clone [https://bitbucket.org/ArneBab/freenet-guile](https://bitbucket.org/ArneBab/freenet-guile)
- Mercurial static site: hg clone [http://draketo.de/proj/guile-freenet](http://draketo.de/proj/guile-freenet)
- [Mercurial over Freenet](http://draketo.de/english/freenet/real-life-infocalypse): hg clone freenet://ArneBab/guile-fcp


License: LGPL
-------------

All these programs are provided under the LGPL: You can copy, share, change and sell them, as long as you attribute the authors and enable others to do the same with your creation. See LICENSE for details.





