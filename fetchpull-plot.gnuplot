set xdata time
set datafile separator ";"
set timefmt "%Y-%m-%d"
set format x "%Y-%m-%d"
# bee swarm plot disabled, since we now have enough data
# set jitter over 0.5 spread 0.5
# logarithmic seconds to show both realtime and bulk together
set logscale y
# styling
set xtics rotate by 45 right
set xlabel "retrieval date"
set ylabel "time to retrieve (seconds)"
set cbtics format ""
set cbtics add ("0" 0)
set cbtics add ("1" 1)
set cbtics add ("2" 2)
set cbtics add ("4" 3)
set cbtics add ("8" 4)
set cbtics add ("16" 5)
set cbtics add ("32" 6)
set cbtics add ("64" 7)
set cbtics add ("128" 8)
set cbtics add ("256" 9)
set cbtics add ("512" 10)
set cblabel "days since upload"
# plot requests
set title "fetchpull: requests"
set term png size 800,600
set output "fetchpull-get-realtime.png"
plot "<(grep realtime\\;\\#t fetchpull-stats-get.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "realtime succeeded", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc"
replot
set output "fetchpull-get-small.png"
plot "<(grep small\\;\\#t fetchpull-stats-get.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "small succeeded", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc"
replot
set output "fetchpull-get-bulk.png"
plot "<(grep bulk\\;\\#t fetchpull-stats-get.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "bulk succeeded", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc", 3600 title "60 min" at end lw 2 lc "#dddddd"
replot
set title "fetchpull: failed requests"
set term png size 800,600
set output "fetchpull-get-failed-realtime.png"
plot "<(grep realtime\\;\\#f fetchpull-stats-get.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 pt 4 title "realtime failed", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc"
replot
set output "fetchpull-get-failed-small.png"
plot "<(grep small\\;\\#f fetchpull-stats-get.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 title "small failed", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc"
replot
set output "fetchpull-get-failed-bulk.png"
plot "<(grep bulk\\;\\#f fetchpull-stats-get.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 title "bulk failed", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc"
replot
# plot inserts
set output "fetchpull-put.png"
set title "fetchpull: inserts"
set ylabel "time to upload (seconds)"
set xlabel "upload date"
set cblabel "days until download"
plot "<(grep realtime\\;\\#t fetchpull-stats-put.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "realtime succeeded", "<(grep small\\;\\#t fetchpull-stats-put.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "small succeeded", "<(grep bulk\\;\\#t fetchpull-stats-put.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "bulk succeeded", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc", 3600 title "60 min" at end lw 2 lc "#dddddd"
replot
# plot inserts
set output "fetchpull-put-failed.png"
set title "fetchpull: failed inserts"
set ylabel "time to upload (seconds)"
set xlabel "upload date"
set cblabel "days until download"
plot "<(grep realtime\\;\\#f fetchpull-stats-put.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 pt 4 title "realtime failed", "<(grep small\\;\\#f fetchpull-stats-put.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 title "small failed", "<(grep bulk\\;\\#f fetchpull-stats-put.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 title "bulk failed", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc", 3600 title "60 min" at end lw 2 lc "#dddddd"
replot
# plot max age of requests
unset cbtics
set yrange [1:512]
set logscale y
set logscale cb
set cbtics format ""
set cbtics add ("0" 0)
set cbtics add ("1" 1)
set cbtics add ("2" 2)
set cbtics add ("4" 4)
set cbtics add ("8" 8)
set cbtics add ("16" 16)
set cbtics add ("32" 32)
set cbtics add ("64" 64)
set cbtics add ("128" 128)
set cbtics add ("256" 256)
set cbtics add ("512" 512)
set cbtics add ("1024" 1024)
set cbtics add ("2048" 2048)
set cbtics add ("4096" 4096)
set ylabel "days since upload"
set cblabel "time to download (s)"
set title "fetchpull: lifetime download-time"
set term png size 800,600
set output "fetchpull-lifetime-realtime.png"
plot "<(grep realtime\\;\\#t fetchpull-stats-get.csv)" using 1:4:3 palette lw 1 title "realtime succeeded"
set output "fetchpull-lifetime-small.png"
plot "<(grep small\\;\\#t fetchpull-stats-get.csv)" using 1:4:3 palette lw 1 title "small succeeded"
set output "fetchpull-lifetime-bulk.png"
plot "<(grep bulk\\;\\#t fetchpull-stats-get.csv)" using 1:4:3 palette lw 1 title "bulk succeeded"


# download time heatmaps (do not work well yet)
# set view map
# set dgrid3d
# set pm3d interpolate 16,16
# set title "fetchpull: average download time"
# 
# set output "fetchpull-lifetime-bulk-download-time.png"
# splot "<(grep 'bulk;#t' fetchpull-stats-get.csv)" using 1:4:3 palette with pm3d title "bulk succeeded"
# set output "fetchpull-lifetime-small-download-time.png"
# splot "<(grep 'small;#t' fetchpull-stats-get.csv)" using 1:4:3 palette with pm3d title "bulk succeeded"

# success count plots
set title "fetchpull: lifetime: monthly success-count"
unset datafile separator
unset logscale cb
unset cbtics
set cbtics format "%g "
set cblabel "successful downloads (count)"
# simple monthly binning
set timefmt "%Y-%m"
set format x "%Y-%m"

set output "fetchpull-lifetime-realtime-success-count.png"
plot "<(grep 'realtime;#t' fetchpull-stats-get.csv | sed 's/-..;/;/;s/;[^;]+//;s/;[^;]*//;s/;[^;]*//;s/;/ /g' | sort | uniq -c)" using 2:3:1 palette lw 4 title "realtime succeeded", 14 title "2 weeks" at begin  lw 2 lc "#aaaaaa", 90 title "3 mo." at begin lw 2 lc "#cccccc"
set output "fetchpull-lifetime-small-success-count.png"
plot "<(grep 'small;#t' fetchpull-stats-get.csv | sed 's/-..;/;/;s/;[^;]+//;s/;[^;]*//;s/;[^;]*//;s/;/ /g' | sort | uniq -c)" using 2:3:1 palette lw 4 title "small succeeded", 14 title "2 weeks" at begin  lw 2 lc "#aaaaaa", 90 title "3 mo." at begin lw 2 lc "#cccccc"
set output "fetchpull-lifetime-bulk-success-count.png"
plot "<(grep 'bulk;#t' fetchpull-stats-get.csv | sed 's/-..;/;/;s/;[^;]+//;s/;[^;]*//;s/;[^;]*//;s/;/ /g' | sort | uniq -c)" using 2:3:1 palette lw 4 title "bulk succeeded", 14 title "2 weeks" at begin  lw 2 lc "#aaaaaa", 90 title "3 mo." at begin lw 2 lc "#cccccc"

replot
quit

