set xdata time
set datafile separator ";"
set timefmt "%Y-%m-%d"
set format x "%Y-%m-%d"
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
set cblabel "age / days since upload"

# add jitter for a bee swarm plot to get a better understanding of the data density without needing transparency
# overlap is vertical binning, spread is horizontal jitter, wrap limits horizontal spread
set jitter overlap 7 spread 0.02 wrap 1

# moving average, see https://gnuplot.sourceforge.net/demo/running_avg.html and https://stackoverflow.com/a/55935923/7666
# number of points in average
n = 64
array A[n]
samples(x) = $0 > (n - 1) ? n : int($0+1)
mod(x) = int(x) % n
avg_n(x) = (A[mod($0)+1]=x, (sum [i=1:samples($0)] A[i]) / samples($0))
# shorter average:
m = 16
array B[m]
samples_m(x) = $0 > (m - 1) ? m : int($0+1)
mod_m(x) = int(x) % m
avg_m(x) = (B[mod_m($0)+1]=x, (sum [i=1:samples_m($0)] B[i]) / samples_m($0))

# plot requests
set title "fetchpull: requests"
set term png size 800,600
set output "fetchpull-get-realtime.png"
set yrange [1:]
plot "<(grep realtime\\;\\#t fetchpull-stats-get.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "realtime succeeded", 3 title " 3 s" at end  lw 2 lc "#cccccc", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 300 title "5 min" at end lw 2 lc "#cccccc", \
   "<(grep \\;0\\;realtime\\;\\#t fetchpull-stats-get.csv)" using 1:(avg_n($3)) title "running mean over previous 64 age 0 points" pt 7 ps 0.5 lw 2 lc rgb "gray", \
   "<(grep \\;1\\;realtime\\;\\#t fetchpull-stats-get.csv)" using 1:(avg_n($3)) title "running mean over previous 64 age 1 points" pt 7 ps 0.5 lw 2 lc rgb "dark-cyan", \
   "<(grep \\;32\\;realtime\\;\\#t fetchpull-stats-get.csv)" using 1:(avg_n($3)) title "running mean over previous 64 age 32 points" pt 7 ps 0.5 lw 2 lc rgb "orange"
replot
set output "fetchpull-get-small.png"
plot "<(grep small\\;\\#t fetchpull-stats-get.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "small succeeded", 3 title " 3 s" at end  lw 2 lc "#cccccc", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc", \
   "<(grep \\;0\\;small\\;\\#t fetchpull-stats-get.csv)" using 1:(avg_n($3)) title "running mean over previous 64 age 0 points" pt 7 ps 0.5 lw 2 lc rgb "gray", \
   "<(grep \\;1\\;small\\;\\#t fetchpull-stats-get.csv)" using 1:(avg_n($3)) title "running mean over previous 64 age 1 points" pt 7 ps 0.5 lw 2 lc rgb "dark-cyan", \
   "<(grep \\;32\\;small\\;\\#t fetchpull-stats-get.csv)" using 1:(avg_n($3)) title "running mean over previous 64 age 32 points" pt 7 ps 0.5 lw 2 lc rgb "orange"
replot
set output "fetchpull-get-bulk.png"
plot "<(grep \\;bulk\\;\\#t fetchpull-stats-get.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "bulk succeeded", 3 title " 3 s" at end  lw 2 lc "#cccccc", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc", 3600 title "60 min" at end lw 2 lc "#dddddd", \
   "<(grep \\;0\\;bulk\\;\\#t fetchpull-stats-get.csv)" using 1:(avg_n($3)) title "running mean over previous 64 age 0 points" pt 7 ps 0.5 lw 2 lc rgb "gray", \
   "<(grep \\;1\\;bulk\\;\\#t fetchpull-stats-get.csv)" using 1:(avg_n($3)) title "running mean over previous 64 age 1 points" pt 7 ps 0.5 lw 2 lc rgb "dark-cyan", \
   "<(grep \\;32\\;bulk\\;\\#t fetchpull-stats-get.csv)" using 1:(avg_n($3)) title "running mean over previous 64 age 32 points" pt 7 ps 0.5 lw 2 lc rgb "orange"
replot

# failed requests get less jitter
set jitter overlap 0.5 spread 0.1 wrap 1
unset yrange

set title "fetchpull: failed requests"
set term png size 800,600
set output "fetchpull-get-failed-realtime.png"
plot "<(grep realtime\\;\\#f fetchpull-stats-get.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 pt 4 title "realtime failed", 3 title " 3 s" at end  lw 2 lc "#cccccc", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 300 title "5 min" at end lw 2 lc "#cccccc", \
   "<(grep \\;1\\;realtime\\;\\#f fetchpull-stats-get.csv)" using 1:(avg_m($3)) title "running mean over previous 16 age 1 points" pt 7 ps 0.5 lw 2 lc rgb "dark-cyan"
replot
set output "fetchpull-get-failed-small.png"
plot "<(grep small\\;\\#f fetchpull-stats-get.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 title "small failed", 3 title " 3 s" at end  lw 2 lc "#cccccc", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc", \
   "<(grep \\;1\\;small\\;\\#f fetchpull-stats-get.csv)" using 1:(avg_m($3)) title "running mean over previous 16 age 1 points" pt 7 ps 0.5 lw 2 lc rgb "dark-cyan"
replot
set output "fetchpull-get-failed-bulk.png"
plot "<(grep bulk\\;\\#f fetchpull-stats-get.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 title "bulk failed", 3 title " 3 s" at end  lw 2 lc "#cccccc", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc", \
   "<(grep \\;1\\;bulk\\;\\#f fetchpull-stats-get.csv)" using 1:(avg_m($3)) title "running mean over previous 16 age 1 points" pt 7 ps 0.5 lw 2 lc rgb "dark-cyan"
replot
# plot inserts
set output "fetchpull-put.png"
set title "fetchpull: inserts"
set ylabel "time to upload (seconds)"
set xlabel "upload date"
set cblabel "days until download"
plot "<(grep realtime\\;\\#t fetchpull-stats-put.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "realtime succeeded", "<(grep small\\;\\#t fetchpull-stats-put.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "small succeeded", "<(grep bulk\\;\\#t fetchpull-stats-put.csv)" using 1:3:(log((column(4)+1))/log(2)) palette lw 1 title "bulk succeeded", 3 title " 3 s" at end  lw 2 lc "#cccccc", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc", 3600 title "60 min" at end lw 2 lc "#dddddd", \
   "<(grep \\;1\\;realtime\\;\\#t fetchpull-stats-put.csv)" using 1:(avg_n($3)) title "running means, last 64, in 1 day" pt 7 ps 0.5 lw 2 lc rgb "dark-cyan", \
   "<(grep \\;1\\;small\\;\\#t fetchpull-stats-put.csv)" using 1:(avg_n($3)) title "" pt 7 ps 0.5 lw 2 lc rgb "dark-cyan", \
   "<(grep \\;1\\;bulk\\;\\#t fetchpull-stats-put.csv)" using 1:(avg_n($3)) title "" pt 7 ps 0.5 lw 2 lc rgb "dark-cyan"
replot
# plot inserts
set output "fetchpull-put-failed.png"
set title "fetchpull: failed inserts"
set ylabel "time to upload (seconds)"
set xlabel "upload date"
set cblabel "days until download"
plot "<(grep realtime\\;\\#f fetchpull-stats-put.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 pt 4 title "realtime failed", "<(grep small\\;\\#f fetchpull-stats-put.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 title "small failed", "<(grep bulk\\;\\#f fetchpull-stats-put.csv)" using 1:(column(3)<20000? column(3) : 1/0):(log((column(4)+1))/log(2)) palette lw 1 title "bulk failed", 3 title " 3 s" at end  lw 2 lc "#cccccc", 10 title "10 s" at end  lw 2 lc "#aaaaaa", 60 title "1 min" at end  lw 2 lc "#aaaaaa", 600 title "10 min" at end lw 2 lc "#cccccc", 3600 title "60 min" at end lw 2 lc "#dddddd"
replot
# plot max age of requests
unset cbtics
set yrange [1:1000]
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
set ylabel "age / days since upload"
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

