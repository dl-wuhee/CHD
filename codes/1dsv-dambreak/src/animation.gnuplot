set term gif animate delay 10
set output "result.gif"


do for [i=0:100] {
    t = i * 0.1
    #if (i==0) {
        #t = 0.0
    #}
    #if (i==1){
        #t = 1.0
    #} 
    #if (i==2) {
        #t = 5.0
    #} 
    #if (i==3) {
        #t = 10.0
    #} 
    set multiplot layout 2, 1
    set style line 1 lt 1 lc 1 lw 2 pt 1 ps 1
    set format y "%5.2f"
    set xrange [0:100]
    set yrange [0.05:0.25]
    set ylabel "{/Times:Italic h}(m)"
    plot "result.dat" index i u 3:4 w l ls 1 title sprintf('{/Times:Italic t}= %4.1fs', t)
    set style line 2 lt 1 lc 2 lw 2 pt 2 ps 1
    set format y "%5.2f"
    set xrange [0:100]
    set yrange [-0.1:0.5]
    set ylabel "{/Times:Italic v}(m/s)"
    plot "result.dat" index i u 3:5 w l ls 2 title sprintf('{/Times:Italic t}= %4.1fs', t)
    unset multiplot
}

set output
