set term gif animate
set output "result.gif"
set xrange [0:100]
set yrange [0.05:0.25]

do for [i=0:4] {
plot "result.dat" index i u 3:4
}
