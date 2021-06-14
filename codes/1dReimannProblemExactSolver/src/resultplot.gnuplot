
do for [i=0:10] {
    set term png
    set output 'result'.i.'png'
    set xlabel "x"
    set ylabel "h"
    set y2label "u"
    set y2tics
    set ytics nomirror
    set xrange [-10:20]
    set yrange [0:2]
    set y2range [-1:11]
    plot "ExactSolver.dat" index i u 1:2 w l t "h" axis x1y1, \
    "ExactSolver.dat" index i u 1:3 w l t "v" axis x1y2  
}
