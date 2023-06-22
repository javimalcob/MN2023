program main
use mod_prec
use funciones
use metodos
implicit none
    !Declaracion de variables
    !La declaracion de variables esta en el modulo_prec
    real(wp), allocatable     :: alfa(:)  
    allocate(alfa(n))
    !Inicializacion de las variables
    alfa = (/0.0_wp, 0.0_wp/)
    
    !#################################### METODO DE EULER #####################################################
    !Bloque de procesamiento usando el metodo de Euler para las aproximaciones
    call euler1_vect(a, b, h, alfa)
     !#################################### METODO DE RUNGE-KUTA2 #####################################################
    !Bloque de procesamiento usando el metodo de Runge kuta orden 2 para las aproximaciones
    call rk2_vect(a, b, h, alfa)
    !#################################### METODO DE RUNGE-KUTA4 #####################################################
    !Bloque de procesamiento usando el metodo de Runge Kuta orden 4 para las aproximaciones
    call rk4_vect(a, b, h, alfa)
    
    deallocate(alfa)
end program main
