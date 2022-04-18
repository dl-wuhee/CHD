reset

datafile = "OpenMP-SpeedUp.dat"

set logscale x 2
set logscale y 2

set xrange [1:64]
set yrange [1:64]

set xlabel "Number of Threads"
set ylabel "Speed up ratio"

set key left top

set style line 1 lt 1 lc "red" lw 1.5 pt 6 ps 1
set style line 2 lt 1 lc "blue" lw 1.5 dashtype 2


set terminal pdfcairo enhanced size 5,4
set output "openmp-speedup.pdf"
plot datafile u 1:(388.744/$2) w lp ls 1 t "Actual", \
     x w l ls 2 t "Theoretical" 
   
set terminal pngcairo enhanced size 640,480
set output "openmp-speedup.png"
replot

set output
