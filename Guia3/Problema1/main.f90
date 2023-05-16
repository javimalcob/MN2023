program problema1
use iso
use mod_funciones
use mod_interpolacion
implicit none
    
    !Declaracion de variables
    real(wp), dimension(0:2)           :: x = (/0.0_wp, 0.6_wp, 0.9_wp/)
    real(wp), dimension(0:2)           :: y1, y2
    real(wp)                           :: xc, pxc, xp, a, b
    integer(il)                        :: n, i, fu, np
    character(80)                      :: archivo1, archivo2, archivo3, archivo4
    
    !Inicializacion del array y1 y array y2
    n = size(x) - 1
    do i = 0 , n 
        y1(i) = f1(x(i)) 
    end do
    write(*,*) x, y1

    do i = 0 , n
        y2(i) = f2(x(i))
    end do
    write(*,*) x, y2

    
        
    !genera 200 puntos para graficar la funcion y los guarda en un archivo
    archivo1 = "f1.dat"
    archivo2 = "f1_lagrange.dat"
    archivo3 = "f1_taylor.dat"
    archivo4 = "f1_newton.dat"
    

    np = 199
    a = x(0)
    b = x(2)
    !Genera puntos x y f(x) funcion f1 original para graficar
    open(newunit=fu, file=archivo1)
        write(fu,*)  "puntos x " , " f(x) "
        
        do i=0, np
            xp = a + i*(b-a)/np
            write(fu,*) xp, f1(xp)
        end do
        
    close(fu)
    !----------------------------------------------------------------
    !Genera puntos x y P(x) de la funcion f1 para graficar usando lagrange
    open(newunit=fu, file=archivo2)
        write(fu,*)  "puntos x " , " pn(x)"
        
        do i=0, np
            xp = a + i*(b-a)/np
            call lagrange(n, x, y1, xp, pxc)
            write(fu,*) xp, pxc
        end do
        
    close(fu)
    
    !Genera puntos x y P(x) de la funcion f1 para graficar usando newton
    open(newunit=fu, file=archivo4)
        write(fu,*)  "puntos x " , " pn(x)"

        do i=0, np
            xp = a + i*(b-a)/np
            call newton(n, x, y1, xp, pxc)
            write(fu,*) xp, pxc
        end do

    close(fu)
!-------------------------------------------------------------------------------------
!funcion f2 (LO MISMO QUE LO ANTERIOR)   
    archivo1 = "f2.dat"
    archivo2 = "f2_lagrange.dat"
    archivo3 = "f2_taylor.dat"
    archivo4 = "f2_newton.dat"
    
    np = 199
    a = x(0)
    b = x(2)
    !Genera puntos x y f(x) funcion f2 original para graficar
    open(newunit=fu, file=archivo1)
        write(fu,*)  "puntos x " , " f(x) "

        do i=0, np
            xp = a + i*(b-a)/np
            write(fu,*) xp, f2(xp)
        end do

    close(fu)
    !Genera puntos x y P(x) de la funcion f2 para graficar usando lagrange
    open(newunit=fu, file=archivo2)
        write(fu,*)  "puntos x " , " pn(x)"

        do i=0, np
            xp = a + i*(b-a)/np
            call lagrange(n, x, y2, xp, pxc)
            write(fu,*) xp, pxc
        end do

    close(fu)

    !Genera puntos x y P(x) de la funcion f2 para graficar usando newton
    open(newunit=fu, file=archivo4)
        write(fu,*)  "puntos x " , " pn(x)"

        do i=0, np
            xp = a + i*(b-a)/np
            call newton(n, x, y2, xp, pxc)
            write(fu,*) xp, pxc
        end do

    close(fu)





    !subroutine lagrange(n ,x, y, xc, pxc)
    xc = 0.45_wp
    call lagrange(n , x, y1, xc, pxc)
    write(*,*) "Lagrange p(xc)", pxc

    call lagrange(n , x, y2, xc, pxc) 
    write(*,*) "Lagrange p(xc)", pxc



    !subroutine newton(n, x, y, xc, pxc)
    xc = 0.45_wp
    call newton( n , x, y1, xc, pxc)
    write(*,*) "Newton p(xc)", pxc

    call newton(n , x, y2, xc, pxc)
    write(*,*) "Newton p(xc)", pxc


end program problema1
