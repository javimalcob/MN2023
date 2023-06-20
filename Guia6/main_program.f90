program main
use mod_prec
use funciones
use metodos
implicit none
    !Declaracion de variables
    real(wp)                                :: a
    real(wp)                                :: b
    real(wp)                                :: alfa
    real(wp), dimension(:), allocatable     :: ti
    real(wp), dimension(:), allocatable     :: wi
    !real(wp), dimension(:), allocatable     :: yi
    integer(il)                             :: n 
    integer(il)                             :: fu, i
    character(80)                           :: file_euler, file_euler_func
    character(80)                           :: file_rk2, file_rk2_func
    character(80)                           :: file_rk4, file_rk4_func
    !Inicializacion de las variables
    n = 10
    a = 0.0_wp
    b = 2.0_wp
    alfa = 0.5_wp
    file_euler =  'salida_euler.dat'
    file_euler_func = 'salida_euler_funcion.dat'
    file_rk2 =  'salida_rk2.dat'
    file_rk2_func = 'salida_rk2_funcion.dat'
    file_rk4 = 'salida_rk4.dat'
    file_rk4_func = 'salida_rk4_funcion.dat'
    
    !#################################### METODO DE EULER #####################################################
    allocate(ti(0:n), wi(0:n))
    
    !Bloque de procesamiento usando el metodo de Euler para las aproximaciones
    call euler1(a, b, n, alfa, ti, wi)
     
    open(newunit=fu, file=file_euler)
        write(fu, *) "            ti            ", "           wi                "
        do i = 0, n
            write(fu, *) ti(i) , wi(i)
        end do 
     close(fu)
     
     !Bloque para comparar aproximaciones de euler con la funcion exacta
     open(newunit=fu, file=file_euler_func)
        write(fu, *) "        ti        ", "       wi         ", "         yi        ", "     |yi - wi|    "
        do i = 0, n
            write(fu, *) ti(i) , wi(i) , y(ti(i)) , abs(y(ti(i)) - wi(i)) 
        end do     
     close(fu)
     
     deallocate(ti, wi)
     
     !#################################### METODO DE RUNGE-KUTA2 #####################################################
    allocate(ti(0:n), wi(0:n))
    
    !Bloque de procesamiento usando el metodo de Runge kuta orden 2 para las aproximaciones
    call rk2(a, b, n, alfa, ti, wi)
     
    open(newunit=fu, file=file_rk2) 
        write(fu, *) "            ti            ", "           wi                "
        do i = 0, n
            write(fu, *) ti(i) , wi(i)
        end do 
     close(fu)
     
     !Bloque para comparar aproximaciones de Runge Kuta orden 2 con la funcion exacta
     open(newunit=fu, file=file_rk2_func)
        write(fu, *) "        ti        ", "       wi         ", "         yi        ", "     |yi - wi|    "
        do i = 0, n
            write(fu, *) ti(i) , wi(i) , y(ti(i)) , abs(y(ti(i)) - wi(i)) 
        end do     
     close(fu)
     
     deallocate(ti, wi)
     
     !#################################### METODO DE RUNGE-KUTA4 #####################################################
    allocate(ti(0:n), wi(0:n))
    
    !Bloque de procesamiento usando el metodo de Runge Kuta orden 4 para las aproximaciones
    call rk4(a, b, n, alfa, ti, wi)
     
    open(newunit=fu, file=file_rk4) 
        write(fu, *) "            ti            ", "           wi                "
        do i = 0, n
            write(fu, *) ti(i) , wi(i)
        end do 
     close(fu)
     
     !Bloque para comparar aproximaciones de Runge Kuta de orden 4 con la funcion exacta
     open(newunit=fu, file=file_rk4_func)
        write(fu, *) "        ti        ", "       wi         ", "         yi        ", "     |yi - wi|    "
        do i = 0, n
            write(fu, *) ti(i) , wi(i) , y(ti(i)) , abs(y(ti(i)) - wi(i)) 
        end do     
     close(fu)
     
     deallocate(ti, wi)
     
     
         
end program main
