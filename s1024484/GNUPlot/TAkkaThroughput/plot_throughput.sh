#! /bin/sh 
name=$1


cat << __EOF | gnuplot

set term png
set output  "$name\_throughput.png"
# set title "$name: Throughput"

set xlabel 'Number of EC2 Instance'
set ylabel '100 trans / second '
# set xrange [1:16]
set xtics (1, 2, 3, 4, 5 ,6,7,8,9,10,11,12,13,14,15,16)
set yrange [0:*]

plot "$name.dat" using 1:2:3 with errorlines  title "Akka $name" lc 1 ,\
     "$name.dat" using 1:4:5 with errorlines  title "TAkka $name" lc 2

set output

__EOF

