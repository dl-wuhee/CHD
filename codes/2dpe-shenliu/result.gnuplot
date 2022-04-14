reset
set contour
set cntrparam levels incremental 0, 2, 14
unset surface
set view map
set table "contour.txt"
splot "result.dat" u 1:2:3 with lines
unset table

reset

set term pdfcairo enhanced size 5,5
set output "result.pdf"
set lmargin at screen 0.05
set rmargin at screen 0.88
set bmargin at screen 0.06
set tmargin at screen 0.98

set arrow 1 from 0.75,1.9 to 0.75, 2 nohead 
set arrow 2 from 1.2,1.9 to 1.2, 2 nohead 
set arrow 3 from 0.75,1.9 to 1.2, 1.9 nohead 
set arrow 4 from 0.8,1.5 to 0.8, 1.9 nohead 

set style textbox opaque noborder margins 0.5,0.5
set cntrlabel format '%2.0f' font ",10"

set pm3d map
set pm3d interpolate 0,0
splot "result.dat" w pm3d, \
      'contour.txt' with lines lc rgb "black" notitle, \
      'contour.txt' with labels boxed notitle #with lines

set output 
