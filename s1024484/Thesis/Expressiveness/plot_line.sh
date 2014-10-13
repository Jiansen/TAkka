#! /bin/sh 



cat << __EOF | gnuplot

set term png
set output  "Expressiveness1.png"
# set title "Expressiveness Check 1"

set xlabel 'lines of Akka Code (Log10)'
set ylabel 'lines of Code (Log10)'


set xrange [0:5]
set yrange [0:5]

plot "exp.dat" using 9:11 with points  title "TAkka Code Lines" lc 1 ,\
     "exp.dat" using 9:10 with points  title "Modified TAkka Lines" lc 2

set output

__EOF

