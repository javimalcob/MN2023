module mod_interpolacion
use iso
use mod_funciones
implicit none

contains
!-----------------------------------------------------------------------------------------------------    
    subroutine lagrange(n, x, y, xc, pxc)
        !Descripcion de la subrutina lagrange
        
        !Declaracion Dummy variables
        real(wp), dimension(0:), intent(in) :: x    !vector x que es xi de (xi, fxi) pts a interpolar
        real(wp), dimension(0:), intent(in) :: y    !vector y que es fxi de (xi, fxi) pts a interpolar
        real(wp), intent(in)                :: xc   !punto a evaluar el polinomio
        real(wp), intent(out)               :: pxc  !Devuelve el Polinomio de Lagrange evaluado en xc
        integer(il), intent(in)             :: n
        !Declaracion de variables auxiliares
        real(wp)                            :: lixc
        integer(il)                         :: i, j    !subindice de polinomios basico de Lagrange
        
        !Bloque de procesamiento
        !n = size(x) - 1  !n es el grado del polinomio de lagrange(notar que x tiene n+1 puntos)
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

!------------------------------------------------------------------------------------------------------
    subroutine newton(n, x, y, xc, pxc)
        !Descripcion la subrutina newton 

        !Declaracion Dummy variables
        real(wp), dimension(0:), intent(in) :: x
        real(wp), dimension(0:), intent(in) :: y
        real(wp), intent(in)                :: xc
        real(wp), intent(out)               :: pxc
        integer(il), intent(in)             :: n
        !n = size(x) - 1  !n es el grado del polinomio de newton(notar que x tiene n+1 puntos)
        
        !Declaracion de variables auxiliares
        real(wp), dimension(0:n, 0:n)       :: d  !matriz de diferencias divididas
        integer(il)                         :: i , j
        real(wp)                            :: aux
        
        d = 0.0_wp 
        !Bloque de procesamiento
        do i = 0, n
            d(i,0) = y(i)
        end do 
        
        do j = 1, n
            do i = j, n
                d(i,j) = (d(i,j-1) - d(i-1, j-1))/(x(i) - x(i-j))
            end do 
        end do

        !Evaluar el polinomio con el algoritmo de Horner
        
        !sin horner 
        pxc = d(0,0)
        do i = 1, n
            aux = 1.0_wp
            do j = 1, i
                aux = aux*(xc-x(j-1))  
            end do
            pxc = pxc + d(i,i) * aux
        end do
    end subroutine newton                



end module mod_interpolacion
