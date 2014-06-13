#! /bin/sh 



cat << __EOF | gnuplot

set term png
set output  "Actor_Construction.png"
set title "Actor Construction Time"

set xlabel '10,000 Actors'
set ylabel 'Time (ms)'


plot "runtime.dat" using 1:2:3 with errorlines  title "Akka Time" lc 1 ,\
     "runtime.dat" using 1:4:5 with errorlines  title "TAkka Time" lc 2

set xrange [1:10]
#set xtics (1, 2, 3, 4, 5 ,6,7,8,9,10)
set yrange [0:3000]

set output

__EOF

