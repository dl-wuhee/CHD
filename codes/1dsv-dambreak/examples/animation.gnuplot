reset
set term gif animate delay 10
set output "result-compare.gif"
art_resultfile = "./dambreak-artvis/result/dambreak-art-result.dat"
ori_resultfile = "./dambreak-original/result/dambreak-ori-result.dat"
tvd_resultfile = "./dambreak-TVD/result/dambreak-tvd-result.dat"

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
    set title sprintf('{/Times:Italic t}= %4.1fs', t)
    set style line 1 lt 1 lc 1 lw 2 pt 1 ps 1 dashtype 2
    set style line 2 lt 1 lc 2 lw 2 pt 2 ps 1 dashtype 2
    set style line 3 lt 1 lc 3 lw 2 pt 3 ps 1 dashtype 2
    set format y "%5.2f"
    set xrange [0:100]
    set yrange [0.05:0.25]
    set ylabel "{/Times:Italic h}(m)"
    plot ori_resultfile index i u 3:4 w l ls 1 title "MacCormack", \
         art_resultfile index i u 3:4 w l ls 2 title "MacCormack Artifical Viscosity", \
         tvd_resultfile index i u 3:4 w l ls 3 title "MacCormack TVD"
    set format y "%5.2f"
    set xrange [0:100]
    set yrange [-0.1:0.5]
    set xlabel "{/Times:Italic x}(m)"
    set ylabel "{/Times:Italic v}(m/s)"
    plot ori_resultfile index i u 3:5 w l ls 1 title "MacCormack", \
         art_resultfile index i u 3:5 w l ls 2 title "MacCormack Artifical Viscosity", \
         tvd_resultfile index i u 3:5 w l ls 3 title "MacCormack TVD"
    unset multiplot
}
set output
reset
