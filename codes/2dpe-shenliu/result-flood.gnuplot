set term epscairo enhanced crop size 5,4
set output "result-floodcontour.eps"


set arrow 1 from 0.75,1.9 to 0.75, 2 nohead lc "red"
set arrow 2 from 1.2,1.9 to 1.2, 2 nohead 
set arrow 3 from 0.75,1.9 to 1.2, 1.9 nohead 
set arrow 4 from 0.8,1.5 to 0.8, 1.9 nohead 

set style textbox opaque noborder margins 0.5,0.5

set pm3d explicit
set pm3d interpolate 0,0
#set palette rgbformulae 33,13,10
set palette rgbformulae 22,13,-31

set contour
set cntrparam levels incremental 0, 2, 14

set view map
unset key
unset border

splot "result.dat" w pm3d, \
      "" w lines lc "black" nosurface, \
      "" w labels boxed textcolor "black"

set term pngcairo enhanced crop
set output "result-floodcontour.png"
replot

set output

