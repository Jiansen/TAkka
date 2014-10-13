#! /bin/sh 



cat << __EOF | gnuplot

set term png
set output  "Expressiveness2.png"
# set title "Expressiveness Checks 2"

set xlabel 'lines of Akka Code (Log10)'
set ylabel '% of Code'


set xrange [0:5]
set yrange [0:120]

plot "exp.dat" using 9:3 with points  title "% of Code Size" lc 2 ,\
     "exp.dat" using 9:2 with points  title "% of Modified Code" lc 1 ,\
     "exp.dat" using 9:12 with points  title "% of New Code" lc 3 ,\
     "exp.dat" using 9:13 with points  title "% of Updated Code" lc 4 ,\
     "exp.dat" using 9:14 with points  title "% of Deleted Code" lc 5


set output

__EOF

