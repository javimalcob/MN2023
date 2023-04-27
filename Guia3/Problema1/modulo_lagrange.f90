module mod_lagrange
use iso

implicit none

contains
    
    subroutine lagrange(x, y, c, pc)
        !Descripcion modulo lagrange
        
        !Declaracion Dummy variables
        real(wp), allocatable   :: x !vector x
        real(wp), allocatable   :: y !vector y       
        real(wp), intent(in)    :: c !punto a evaluar el polinomio
        real(wp), intent(out)   :: pc!Polinomio de Lagrange

    end subroutine lagrange
end module mod_lagrange
