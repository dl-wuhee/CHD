reset
set terminal pdfcairo enhanced size 10,8
set output "ori-result-t=5.pdf"

resultfile = "dambreak-ori-result.dat"

set multiplot layout 2, 1
set style line 1 lt 1 lc 1 lw 2 pt 1 ps 1
set format y "%5.2f"
set xrange [0:100]
set yrange [0.05:0.25]
set ylabel "{/Times:Italic h}(m)"
plot resultfile index 51 u 3:4 w l ls 1 title "水深"
set style line 2 lt 1 lc 2 lw 2 pt 2 ps 1
set format y "%5.2f"
set xrange [0:100]
set yrange [-0.1:0.5]
set xlabel "{/Times:Italic x}(m)"
set ylabel "{/Times:Italic v}(m/s)"
plot resultfile index 51 u 3:5 w l ls 2 title "流速"
unset multiplot
reset
