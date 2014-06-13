#! /bin/sh 
name=$1


cat << __EOF | gnuplot

set term png
set output  "$name.png"
set title ""

set xlabel 'Number of Nodes'
set ylabel 'runtime (ms)'
set xtics (1, 4, 8, 12, 16, 20, 24, 28, 32)
set yrange [0:*]
set y2label 'speed-up'
set y2range [0:32]
set y2tics (0, 4, 8, 12, 16, 20, 24, 28, 32)

plot "$name.dat" using 1:2:3 with errorlines  title 'Akka Runtime' lc 1,\
     "$name.dat" using 1:4:5 with errorlines  title 'TAkka Runtime' lc 2,\
     "$name.dat"  using 1:6 with lines  title 'Akka Speedup' axes x1y2,\
     "$name.dat" using 1:7 with lines  title 'TAkka Speedup' axes x1y2
     
set output

__EOF

