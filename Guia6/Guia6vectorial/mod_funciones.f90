module funciones
use mod_prec
implicit none
contains


    !-----------------------------------------------------------------------------------------------
    function F(t, y)
        real(wp), intent(in)                                :: t
        real(wp), dimension(1:2), intent(in)                :: y
        real(wp), dimension(1:2)                            :: F
        F(1) = y(2)
        F(2) =  -(g/l) * sin(y(1))
    end function F
    !------------------------------------------------------------------------------------------------
    function F1(t, y)
        real(wp), intent(in)                                :: t
        real(wp), dimension(1:2), intent(in)                :: y
        real(wp), dimension(1:2)                            :: F1
        F1(1) = y(1) + (y(2))**2 - t**3
        F1(2) = y(2) + (y(1))**3 + cos(t)
    end function F1
   
   !-------------------------------------------
    real(wp) function y(t)
        real(wp), intent(in)                :: t
        y = (t + 1._wp)**2._wp - 0.5_wp*exp(t)
    end function y

end module funciones




