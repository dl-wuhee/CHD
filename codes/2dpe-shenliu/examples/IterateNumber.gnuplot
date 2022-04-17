set terminal pdfcairo enhanced size 15,5
set output "iterationcompare.pdf"
#set terminal pngcairo enhanced size 2160,720
#set output "iterationcompare.png"

set xlabel "Iteration step"

set style line 1 linetype 1 linecolor 1 linewidth 1.25
set style line 2 linetype 1 linecolor 2 linewidth 1.25
set style line 3 linetype 1 linecolor 3 linewidth 1.25
set style line 4 linetype 1 linecolor 4 linewidth 1.25
set style line 5 linetype 1 linecolor 5 linewidth 1.25
set style line 6 linetype 1 linecolor 6 linewidth 1.25

set logscale y 10
set ylabel "Residual"
set format y "10^{%L}"


set multiplot layout 1,3 #margins 0.1,0.98,0.1,0.98 spacing 0.08,0.08
#set origin 0, 0
set title "Coarse Grid (75x50)"
jacob_coarse_log = "JacobIter-coarse/result/jacob-coarse-log.dat"
guass_coarse_log = "GuassSIter-coarse/result/guass-coarse-log.dat"
#ssor1_5_coarse_log = "SSORIter-coarse/result/ssor1_5-coarse-log.dat"
#ssor1_7_coarse_log = "SSORIter-coarse/result/ssor1_7-coarse-log.dat"
#ssor1_8_coarse_log = "SSORIter-coarse/result/ssor1_8-coarse-log.dat"
ssor1_9_coarse_log = "SSORIter-coarse/result/ssor1_9-coarse-log.dat"
plot jacob_coarse_log w l ls 1 title "Jacobean Iter", \
     guass_coarse_log w l ls 2 title "GuassSidel Iter", \
     ssor1_9_coarse_log w l ls 6 title "SOR Iter Omega=1.9"
     #ssor1_5_coarse_log w l ls 3 title "SOR Iter Omega=1.5", \
     #ssor1_7_coarse_log w l ls 4 title "SOR Iter Omega=1.7", \
     #ssor1_8_coarse_log w l ls 5 title "SOR Iter Omega=1.8", \

#set size 5,5
#set origin 5.5, 0
set title "Normal Grid (150x100)"
jacob_normal_log = "JacobIter-normal/result/jacob-normal-log.dat"
guass_normal_log = "GuassSIter-normal/result/guass-normal-log.dat"
ssor1_5_normal_log = "SSORIter-normal/result/ssor1_5-normal-log.dat"
ssor1_7_normal_log = "SSORIter-normal/result/ssor1_7-normal-log.dat"
ssor1_8_normal_log = "SSORIter-normal/result/ssor1_8-normal-log.dat"
ssor1_9_normal_log = "SSORIter-normal/result/ssor1_9-normal-log.dat"
plot jacob_normal_log w l ls 1 title "Jacobean Iter", \
     guass_normal_log w l ls 2 title "GuassSidel Iter", \
     ssor1_5_normal_log w l ls 3 title "SOR Iter Omega=1.5", \
     ssor1_7_normal_log w l ls 4 title "SOR Iter Omega=1.7", \
     ssor1_8_normal_log w l ls 5 title "SOR Iter Omega=1.8", \
     ssor1_9_normal_log w l ls 6 title "SOR Iter Omega=1.9"

#set size 5,5
#set origin 11, 0
set title "Finer Grid (375x250)"
jacob_finer_log = "JacobIter-finer/result/jacob-finer-log.dat"
guass_finer_log = "GuassSIter-finer/result/guass-finer-log.dat"
rbguass_finer_log = "RBGuassSIter-finer/result/rbguass-finer-log.dat"
#ssor1_5_finer_log = "SSORIter-finer/result/ssor1_5-finer-log.dat"
#ssor1_7_finer_log = "SSORIter-finer/result/ssor1_7-finer-log.dat"
#ssor1_8_finer_log = "SSORIter-finer/result/ssor1_8-finer-log.dat"
ssor1_9_finer_log = "SSORIter-finer/result/ssor1_9-finer-log.dat"
plot jacob_finer_log w l ls 1 title "Jacobean Iter", \
     guass_finer_log w l ls 2 title "GuassSidel Iter", \
     rbguass_finer_log w l ls 3 title "Red Black GuassSidel Iter", \
     ssor1_9_finer_log w l ls 6 title "SOR Iter Omega=1.9"
     #ssor1_5_finer_log w l ls 3 title "SOR Iter Omega=1.5", \
     #ssor1_7_finer_log w l ls 4 title "SOR Iter Omega=1.7", \
     #ssor1_8_finer_log w l ls 5 title "SOR Iter Omega=1.8", \



unset multiplot

set output
