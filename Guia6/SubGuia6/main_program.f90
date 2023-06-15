program main
use mod_prec
use funciones
use metodos
implicit none
    !Declaracion de variables
    real(wp)                                :: a
    real(wp)                                :: b
    real(wp)                                :: h
    real(wp)                                :: alfa
    
    !Inicializacion de las variables
    h = 0.2_wp
    a = 0.0_wp
    b = 2.0_wp
    alfa = 0.5_wp
    
    !#################################### METODO DE EULER #####################################################
    !Bloque de procesamiento usando el metodo de Euler para las aproximaciones
    call euler1(a, b, h, alfa)
     !#################################### METODO DE RUNGE-KUTA2 #####################################################
    !Bloque de procesamiento usando el metodo de Runge kuta orden 2 para las aproximaciones
    call rk2(a, b, h, alfa)
    !#################################### METODO DE RUNGE-KUTA4 #####################################################
    !Bloque de procesamiento usando el metodo de Runge Kuta orden 4 para las aproximaciones
    call rk4(a, b, h, alfa)
    
end program main
