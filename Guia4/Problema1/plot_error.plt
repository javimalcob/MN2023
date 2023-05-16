set term x11 persist
	set title "Grafico error"
	set xlabel "n iteracion"
	set ylabel "error"
	set grid
	set logscale y
	


	plot "datosp1g4.dat" u 1:4 w lp pointtype 7 

	

exit
