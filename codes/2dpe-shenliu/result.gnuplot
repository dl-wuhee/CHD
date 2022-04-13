set term pngcairo crop
set output "result.png"
set contour base
set cntrparam levels incremental 1, 1, 14
unset surface
set view 0,0
unset key
set arrow 1 from 0.75,1.9 to 0.75, 2 nohead 
set arrow 2 from 1.2,1.9 to 1.2, 2 nohead 
set arrow 3 from 0.75,1.9 to 1.2, 1.9 nohead 
set arrow 4 from 0.8,1.5 to 0.8, 1.9 nohead 
splot "result.dat" u 1:2:3 with lines

set output 
