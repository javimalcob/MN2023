set term x11 persist
    set title "Funcion f1"
    set ylabel "{f(x)}"
    set xlabel "{x}"
    set grid
    
    #set sample 1000
    #f1(x) = log(x+1)
    #set xrange [0.0:0.9]

    #plot f1(x) t "f1(x)" 
    plot "f1.dat" u 1:2 t "f1(x)" with linespoints pointtype 0
    replot "f1_lagrange.dat" u 1:2 t "p(x) lagrange f1" with linespoints pointtype 0

exit
