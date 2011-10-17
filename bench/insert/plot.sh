#! /bin/sh

gnuplot <<EOF
set terminal png size 1024,768;
set output 'plot.png';
set xtics rotate;
set xrange [-0.125:5.25];
set bmargin 5;  
set datafile separator ',';
set style data boxerrorbars;set style fill pattern;
plot 'plot.csv' using ($0+0.0):2:3:4:(0.125):xtic(1) title 'rb','plot.csv' using ($0+0.125):5:6:7:(0.125):xtic(1) title 'll'
