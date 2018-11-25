set xdata time
set datafile separator ";"
set timefmt "%Y-%m-%d"
set format x "%Y-%m-%d"
set set xlabel "retrieval date"
set xtics rotate by 45 right
set ylabel "time to retrieve (seconds)"
set title "fetchpull: successful requests"
plot "<(grep \\#t fetchpull-stats-get.csv)" using 1:3:(log(($4+1))) palette

