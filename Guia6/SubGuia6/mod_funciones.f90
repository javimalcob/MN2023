module funciones
use mod_prec
implicit none
contains


    !-----------------------------------------------------------------------------------------------
    real(wp) function f(t, y)
        real(wp), intent(in)                 :: t
        real(wp), intent(in)                 :: y
        f = y - t**2._wp + 1  
    end function f
    !------------------------------------------------------------------------------------------------
    real(wp) function y(t)
        real(wp), intent(in)                :: t
        y = (t + 1._wp)**2._wp - 0.5_wp*exp(t)
    end function y

end module funciones
