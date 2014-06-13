#! /bin/sh 
name=$1


cat << __EOF | gnuplot
set term png
set output  "$name\_speedup.png"
set title "$name: Speedup"

set xlabel 'Number of Nodes'
set ylabel 'speed-up'
set xtics (1, 4, 8, 12, 16, 20, 24, 28, 32)
set yrange [0:*]
plot "$name.dat"  using 1:6 with lines  title 'Akka Speedup' ,\
     "$name.dat" using 1:7 with lines  title 'TAkka Speedup'

set output

__EOF

