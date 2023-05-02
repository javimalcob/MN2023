program problema1
use iso
use mod_funciones
use mod_interpolacion
implicit none
    
    !Declaracion de variables
    real(wp), dimension(0:2)           :: x = (/0.0_wp, 0.6_wp, 0.9_wp/)
    real(wp), dimension(0:2)           :: y1, y2
    real(wp)                         :: xc, pxc, xp, a, b
    integer(il)                      :: n, i, fu, np
    character(80)                    :: archivo1, archivo2, archivo3
    
    !Inicializacion del array y1 y array y2
    n = size(x) 
    do i = 1 , n
        y1(i) = f1(x(i)) 
    end do
    write(*,*) x, y1

    do i = 1 , n
        y2(i) = f2(x(i))
    end do
    write(*,*) x, y2

    
        
    !genera 200 puntos para graficar la funcion y los guarda en un archivo
    archivo1 = "f1.dat"
    archivo2 = "f1_lagrange.dat"
    archivo3 = "f1_taylor.dat"
    np = 199
    a = x(0)
    b = x(2)
    open(newunit=fu, file=archivo1)
        write(fu,*)  "puntos x " , " f(x) "
        
        do i=0, np
            xp = a + i*(b-a)/np
            write(fu,*) xp, f1(xp)
        end do
        
    close(fu)

    open(newunit=fu, file=archivo2)
        write(fu,*)  "puntos x " , " pn(x)"
        
        do i=0, np
            xp = a + i*(b-a)/np
            call lagrange(x,y1,xp,pxc)
            write(fu,*) xp, pxc
        end do
        
    close(fu)


    !subroutine lagrange(x, y, xc, pxc)
    xc = 0.45_wp
    call lagrange(x, y1, xc, pxc)
    write(*,*) "Lagrange p(xc)", pxc

    call lagrange(x, y2, xc, pxc) 
    write(*,*) "Lagrange p(xc)", pxc
end program problema1
