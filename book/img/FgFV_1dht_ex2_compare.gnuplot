set term tikz standalone color tightboundingbox size 8cm,6cm

reset

set style line 11 lt 1 lw 3 lc rgb '#0072bd' # blue
set style line 12 dt 2 lw 3 pt 6 ps 1.5 lc rgb '#d95319' # orange
set style line 13 lt 1 lw 3 lc rgb '#edb120' # yellow
set style line 14 lt 1 lw 3 lc rgb '#7e2f8e' # purple
set style line 15 lt 1 lw 3 lc rgb '#77ac30' # green
set style line 16 lt 1 lw 3 lc rgb '#4dbeee' # light-blue
set style line 17 lt 1 lw 3 lc rgb '#a2142f' # red

set border lw 2 

set xrange [0:0.02]
set xtics nomirror 0.004
set xlabel "\\zihao{5}$x(\\mathrm{m}$)"

set yrange [50:300]
set ytics nomirror 50
set ylabel "\\zihao{5}$T(\\mathrm{^{\\circ}C})$"

set key right bottom spacing 1.3

TA = 100.0
TB = 200.0
L = 0.02
q = 1000000
k = 0.5
T(x) = ((TB-TA)/L + q/(2*k)*(L -x))*x + TA

set output "FgFV_1dht_ex2_compare.tex"
plot "FgFV_1dht_ex2_compare.dat" u 1:2 w lp ls 12 title "数值解", \
     T(x) w line ls 11 title "解析解"
     #phi(x) w line ls 11 title "$T_{exact}$"
