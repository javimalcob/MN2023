set term x11 persist

# Guia6 -Problema 4

##################################################
####            h o l i s    ( :            ######
##################################################

    set title  "Funcion tita(t)"
    set xlabel "tiempo t [s]"
    set ylabel "tita(t) [rad]"
    set grid
    #set logscale xy
    #set sample 500

    
##################################################
######      G R A F.   D A T O S            ######
##################################################

    # grafico del pendulo y pendulo P0    

    plot "salida_rk4_pendulo.dat" u 1:2 title "Datos tita(t) vs t" w lp pointtype 7
    replot "salida_rk4_pendulo_PO.dat" u 1:2 title "Datos tita_PO(t) vs t" w lp pointtype 7

##################################################
######          E X P O R T A R             ######
##################################################
############       P   N   G         #############

    set terminal png size 1200,900
    set output './Graficos/figura_pendulo.png'
    replot

exit
