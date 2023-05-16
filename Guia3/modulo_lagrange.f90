module interpolacion
use iso

implicit none


contains
    subroutine lagrange(n, x, fx, c, pc)
        integer(il), intent(in)                  ::  n  !grado del polinomio
        real(wp), dimension(0:n), intent(in)     :: x  !vector de los xi
        real(wp), dimension(0:n), intent(in)     :: fx !vector de los fxi
        real(wp), intent(in)                     :: c   !punto a evaluar
        real(wp), intent(out)                    :: pc  !polinomio evaluado c
        
        !variables auxiliares
        real(wp)                    :: lci
        integer(il)                 :: i, k
        !-----------------------------------------------------
        !Proceso
        pc = 0.0_wp
        !auxl = (c - x(i)) / (x(k)- x(i))
        do k = 0, n
            lci = 1.0_wp      
            do i = 0, n
                if (k /= i) then
                    lci = lci * (c - x(i)) / (x(k)- x(i))                     
                end if
                
            end do
            pc = pc + lci * fx(k)
        end do
        
    end subroutine lagrange



end module interpolacion
