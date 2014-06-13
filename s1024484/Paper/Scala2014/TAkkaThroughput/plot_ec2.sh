#! /bin/sh 


cat << __EOF | gnuplot

set term png
set output  "throughput.png"
set title "Throughput Test"


set auto x
set yrange [0:300000]
set style data histogram
set style histogram cluster gap 1
set style fill solid border -1
set boxwidth 0.9
set xtic rotate by -45 scale 0 font ",8"
#set bmargin 10 
plot 'EC2.dat' using 6:xtic(1) ti col, '' u 12 ti col, '' u 13 ti col, '' u 14 ti col



set output

__EOF

