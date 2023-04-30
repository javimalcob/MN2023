program problema1
use iso
use mod_funciones
use mod_lagrange
implicit none
    
    !Declaracion de variables
    real(wp), dimension(3)           :: x = (/0.0_wp, 0.6_wp, 0.9_wp/)
    real(wp), dimension(3)           :: y1, y2
    real(wp)                         :: xc, pxc
    integer(il)                      :: n, i
    character(80)                    :: archivo1
    
    !Inicializacion del array y1
    n = size(x) 
    do i = 1 , n
        y1(i) = f1(x(i)) 
    end do
    write(*,*) x, y1

    do i = 1 , n
        y2(i) = f2(x(i))
    end do
    write(*,*) x, y2

    !subroutine lagrange(x, y, xc, pxc)
    xc = 0.45_wp
    call lagrange(x, y1, xc, pxc)
    write(*,*) "Lagrange p(xc)", pxc

    call lagrange(x, y2, xc, pxc) 
    write(*,*) "Lagrange p(xc)", pxc
end program problema1
