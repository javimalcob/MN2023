module funciones
use mod_prec
implicit none
contains


    !-----------------------------------------------------------------------------------------------
    function F(t, y)
       real(wp), intent(in)                                :: t
       real(wp), dimension(:), intent(in)                  :: y
       real(wp), dimension(size(y))                        :: F
       F(1) = r * y(1)* (1 - (y(1)/K)) 
       
    end function F
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




