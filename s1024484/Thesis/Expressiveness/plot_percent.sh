#! /bin/sh 



cat << __EOF | gnuplot

set term png
set output  "Expressiveness2.png"
# set title "Expressiveness Checks 2"

set xlabel 'lines of Akka Code (Log10)'
set ylabel 'lines of Code'


set xrange [0:5]
set yrange [0:120]

plot "exp.dat" using 6:2 with points  title "% of Modified Code" lc 1 ,\
     "exp.dat" using 6:3 with points  title "% of Code Size" lc 2


set output

__EOF

