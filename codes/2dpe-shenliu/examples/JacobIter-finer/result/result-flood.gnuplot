set term epscairo enhanced crop size 5,4
set output "jacob-finer.eps"

set arrow 1  from 30,50 to 30,46 nohead 
set arrow 2  from 30,46 to 31,46 nohead 
set arrow 3  from 31,46 to 31,40 nohead 
set arrow 4  from 31,40 to 32,40 nohead 
set arrow 5  from 32,40 to 32,46 nohead 
set arrow 6  from 32,46 to 33,46 nohead 
set arrow 7  from 33,46 to 33,48 nohead 
set arrow 8  from 33,48 to 44,48 nohead 
set arrow 9  from 44,48 to 44,46 nohead 
set arrow 10 from 44,46 to 45,46 nohead 
set arrow 11 from 45,46 to 45,50 nohead 

set object 1 polygon  \
   from 30,50 to 30,46 to 31,46 to 31,40 to 32,40 to 32,46 to \
   33, 46 to 33,48 to 44,48 to 44,46 to 45,46 to 45,50 to 30,50 
set object 1 fillstyle solid fillcolor "black"
set object 1 front


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

splot "jacob-finer-result.dat" w pm3d#, \
      #"" w lines lc "black" nosurface, \
      #"" w labels boxed textcolor "black"

set term pngcairo enhanced crop
set output "jacob-finer.png"
replot

set output

