set term x11 persist

# Guia6 -Problema 5

##################################################
####            h o l i s    ( :            ######
##################################################

    set title  "Modelo SIR"
    set xlabel "tiempo t [s]"
    set ylabel "S(t) , I(t), R(t) "
    set grid
    #set logscale xy
    #set sample 500

    
##################################################
######      G R A F.   D A T O S            ######
##################################################

    # grafico del pendulo y pendulo P0    

    plot "salida_rk4_SIR.dat" u 1:2 title "S(t) vs t" w lp pointtype 7
    replot "salida_rk4_SIR.dat" u 1:3 title "I(t) vs t" w lp pointtype 7
    replot "salida_rk4_SIR.dat" u 1:4 title "R(t) vs t" w lp pointtype 7
##################################################
######          E X P O R T A R             ######
##################################################
############       P   N   G         #############

    set terminal png size 1200,900
    set output './Graficos/figura_SIR.png'
    replot

exit
