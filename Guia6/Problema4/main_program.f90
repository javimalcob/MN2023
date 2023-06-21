program main
use mod_prec
use funciones
use metodos
implicit none
    !Declaracion de variables
    real(wp)                                :: a
    real(wp)                                :: b
    real(wp)                                :: h
    real(wp), dimension(1:2)                :: alfa1, alfa2
    
    !Inicializacion de las variables
    h = 0.1_wp
    a = 0.0_wp
    b = 10.0_wp
    alfa1 = (/0.50_wp, 0.0_wp/)
    alfa2 = (/0.25_wp, 0.0_wp/)
 
    !#################################### METODO DE RUNGE-KUTA4 #####################################################
    !Bloque de procesamiento usando el metodo de Runge Kuta orden 4 para las aproximaciones
    call rk4_vect(a, b, h, alfa1)
    
    !call rk4_vect(a, b, h, alfa2)
     !#################################### METODO DE RUNGE-KUTA4 #####################################################
    !Bloque de procesamiento usando el metodo de Runge Kuta orden 4 para las aproximaciones
    call rk4_vect_PO(a, b, h, alfa1)
    
end program main
