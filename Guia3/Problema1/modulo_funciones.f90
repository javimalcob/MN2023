module mod_funciones
use iso
implicit none

contains
!-------------------------------------------------------

    real(wp) function f1(x)
        real(wp), intent(in)    :: x

        f1 = log(x + 1)    
    end function f1

!--------------------------------------------------------
    real(wp) function f2(x) 
        real(wp), intent(in)    :: x
        
        f2 = sqrt(x + 1)
    end function f2
end module mod_funciones
