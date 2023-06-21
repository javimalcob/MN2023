module funciones
use mod_prec
implicit none
contains


    !-----------------------------------------------------------------------------------------------
    function F(t, y)
       real(wp), intent(in)                                :: t
       real(wp), dimension(:), intent(in)                  :: y
       real(wp), dimension(size(y))                        :: F
       F(1) = y(2)
       F(2) =  -(g/l) * sin(y(1))
    end function F
    !------------------------------------------------------------------------------------------------
    function F1(t, y)
       real(wp), intent(in)                                :: t
       real(wp), dimension(:), intent(in)                  :: y
       real(wp), dimension(size(y))                        :: F1
       F1(1) = y(2)
       F1(2) =  -(g/l) * y(1)
    end function F1
    !------------------------------------------------------------------------------------------------
    
    
    !function F(t, y)
    !    real(wp), intent(in)                                :: t
    !    real(wp), dimension(:), intent(in)                  :: y
    !    real(wp), dimension(size(y))                        :: F
    !    F(1) = y(2)
    !    F(2) = -1.0_wp * g * exp((-1.0_wp*t)/af)
        !F(n) = agregar mas ecuaciones si hacen falta
        
    !end function F
   
   !-------------------------------------------
    

end module funciones




