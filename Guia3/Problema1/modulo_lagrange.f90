module mod_lagrange
use iso

implicit none

contains
    
    subroutine lagrange(x, y, xc, pxc)
        !Descripcion modulo lagrange
        
        !Declaracion Dummy variables
        !integer(il),intent(in)      :: n    !orden del polinomio de lagrange
        real(wp), dimension(0:)     :: x    !vector x que es xi de (xi, fxi) pts a interpolar
        real(wp), dimension(0:)     :: y    !vector y que es fxi de (xi, fxi) pts a interpolar
        real(wp), intent(in)        :: xc    !punto a evaluar el polinomio
        real(wp), intent(out)       :: pxc   !Devuelve el Polinomio de Lagrange evaluado en xc
        
        !Declaracion de variables auxiliares
        real(wp)                    :: lixc
        integer(il)                 :: n, i, j    !subindice de polinomios basico de Lagrange
        n = size(x) - 1  !n es el grado del polinomio de lagrange(notar que x tiene n+1 puntos)
        
        pxc = 0.0_wp
        
        do i = 0, n
            lixc = 1.0_wp
            do j = 0, n
                if (j /= i ) then
                   lixc = lixc * (xc-x(j))/(x(i)-x(j))
                end if
            end do
            pxc = pxc + y(i)*lixc      
        end do
        

    end subroutine lagrange
end module mod_lagrange
