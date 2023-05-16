module funciones
use iso
implicit none
contains
 !--------------------------------------------
    real(wp) function f(x)
        real(wp)          :: x 

        f = x * exp(x)
    end function f

 !----------------------------------------------
    real(wp) function df(x)
        real(wp)         :: x
        df = (1+x)*exp(x)
    end function df
 !----------------------------------------------
end module funciones
