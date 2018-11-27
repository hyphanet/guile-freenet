set xdata time
set datafile separator ";"
set timefmt "%Y-%m-%d"
set format x "%Y-%m-%d"
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
set title "fetchpull: successful requests"
set term png size 800,600
set output "fetchpull.png"
plot "<(grep \\#t fetchpull-stats-get.csv)" using 1:3:(log(($4+1))/log(2)) palette lw 2
quit