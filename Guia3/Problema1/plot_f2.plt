set term x11 persist
    set title "Funcion f2"
    set ylabel "{f(x)}"
    set xlabel "{x}"
    set grid
    
    #set sample 1000
    #f2(x) = sqrt(x+1)
    #set xrange [0.0:0.9]

    #plot f2(x) t "f2(x)"
    plot "f2.dat" u 1:2 t "f2(x)" with linespoints pointtype 0
    replot "f2_lagrange.dat" u 1:2 t "p(x) lagrange f2" with linespoints pointtype 0
    replot "f2_newton.dat" u 1:2 t "p(x) newton f2" with linespoints pointtype 0
exit

