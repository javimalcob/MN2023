set term x11 persist

# Guia6 -Problema 6

##################################################
####            h o l i s    ( :            ######
##################################################

    set title  "Modelo Ecuacuion Logistica"
    set xlabel "tiempo t [s]"
    set ylabel "N(t)"
    set grid
    #set logscale xy
    #set sample 500

    
##################################################
######      G R A F.   D A T O S            ######
##################################################

    # grafico de la ecuacion Logistica    

    plot "salida_rk4_logistica.dat" u 1:2 title "S(t) vs t" w lp pointtype 7
    #replot "salida_rk4_SIR.dat" u 1:3 title "I(t) vs t" w lp pointtype 7
    #replot "salida_rk4_SIR.dat" u 1:4 title "R(t) vs t" w lp pointtype 7
##################################################
######          E X P O R T A R             ######
##################################################
############       P   N   G         #############

    set terminal png size 1200,900
    set output './Graficos/figura_logistica.png'
    replot

exit
