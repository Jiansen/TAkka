#! /bin/sh 
name=$1


cat << __EOF | gnuplot

set term png
set output  "$name\_time.png"
set title "$name: Run Time"

set xlabel 'Number of Nodes'
set ylabel 'runtime (ms)'
set xtics (1, 4, 8, 12, 16, 20, 24, 28, 32)
set yrange [0:*]

plot "$name.dat" using 1:2:3 with errorlines  title 'Akka Runtime' lc 1,\
     "$name.dat" using 1:4:5 with errorlines  title 'TAkka Runtime' lc 2

set output

__EOF

