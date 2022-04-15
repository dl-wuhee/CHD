set term epscairo enhanced crop size 5,4
set output "result-linescontour.eps"


set arrow 1 from 0.75,1.9 to 0.75, 2 nohead 
set arrow 2 from 1.2,1.9 to 1.2, 2 nohead 
set arrow 3 from 0.75,1.9 to 1.2, 1.9 nohead 
set arrow 4 from 0.8,1.5 to 0.8, 1.9 nohead 

set style textbox opaque noborder margins 0.5,0.5

set contour base
set cntrparam levels incremental 0, 2, 14
unset surface
set view map

splot "result.dat" w l notitle, \
      "result.dat" w labels boxed notitle

set term pngcairo enhanced crop
set output "result-linescontour.png"
replot

set output 
