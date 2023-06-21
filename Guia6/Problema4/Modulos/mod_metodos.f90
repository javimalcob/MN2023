module metodos
use mod_prec
use funciones
implicit none
!######################AAAAAAAAAAAAHHHHHH!!!!######################################################
contains  !<<<<<<!!COOOOOOOOOOOONNNNNNNNTAAAAAAAAAAAAIIIIIIIIIINNNNNNNSSSSSSSSSS!!!!!!!!!!!! :c
!^^^^^^^^^^^^^^^^^^^^^ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!--------------------------------------------------------------------------------------------------
   
       
    !----------------------------------------------------------------------------------------------- 
    !#################################################################################################
    !###############METODO DE RUNGE-KUTTA PARA SISTEMAS DE ECUACIONES DIFERENCIALES###################
    !#################################################################################################
    !------------------------------------------------------------------------------------------------
    
     subroutine rk4_vect(a, b, h, alfa)
        !==Descripcion del metodo rk4==:
            !El metodo obtiene aproximaciones para el problema del valor inicial
            ! y' = f(t,y)  , a <= t <= b  y(a) = alfa
            ! en (n+1) numeros igualmente espaciados del intervalo [a, b]
        ! con h = (b-a)/n y ti = a + i*h y si f es C5 entonces:
        
        !==Idea del algoritmo==:
        !w0 = alfa
        !k1 = h * f(ti,wi)
        !k2 = h * f(ti + (h/2), wi + (1/2)*k1)
        !k3 = h * f(ti + (h/2), wi + (1/2)*k2)
        !k4 = h * f(ti+1, wi + k3)
        !w_i+1 = wi + (1/6)*(k1 + 2*k2 + 2*k3 + k4)   
        !-----------------------------------------------------------------------------------    
        !Declaracion de dummy varialbes
        real(wp), intent(in)                    :: a    !extremo izquierdo del intervalo     
        real(wp), intent(in)                    :: b    !extremo derecho del intervalo
        real(wp), intent(in)                    :: h    !tamaño de paso
        real(wp), dimension(:), intent(in)      :: alfa !vector condicion inicial alfa = (x1_0, x2_0)
        
        !----------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp), dimension(size(alfa))         :: w  !almacena las sucesiva aproximaciones wi
        real(wp), dimension(size(alfa))         :: k1, k2, k3 , k4
        integer(il)                             :: i, j, fu, n
        character(80), parameter                :: archivo = 'salida_rk4_pendulo.dat'
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        t = a
        n = size(alfa)
        !inicio explicitamente el w para notar que es un vector(directamente se puede hacer w = alfa)
        init_w: do j = 1 , n
            w(j) = alfa(j)
        end do init_w
        
        write(*,*) " h ", " t " , "   tita(t)   ", "  u(t)   "
        write(*,*) h, t , w(1), w(2)
        i = 1
        !-----------------------------------------------------------------------------------------
        !BLOQUE DE PROCESAMIENTO
        open(newunit=fu, file=archivo)
            write(fu, *) "            ti            ", "           tita(ti)                ", "      u(ti)       "
            write(fu, *) t,  w(1), w(2)
            main_do: do while (abs(t) < abs(b))
                k1 = h * F(t, w)
                k2 = h * F(t + (h/2.0_wp), w + (1.0_wp/2.0_wp) * k1 )
                k3 = h * F(t + (h/2.0_wp), w + (1.0_wp/2.0_wp) * k2 )
                k4 = h * F(t + h, w + k3)
                w = w + (1.0_wp/6.0_wp)*(k1 + 2.0_wp * k2 + 2.0_wp* k3 + k4)  !calcula wi
                t = a + i*h                                                   !calcula ti
                write(fu, *) t , w(1), w(2)
                i = i + 1
            end do main_do
        close(fu)
        !-----------------------------------------------------------------------------------------
     end subroutine rk4_vect  
     
     
     subroutine rk4_vect_PO(a, b, h, alfa)
        !==Descripcion del metodo rk4==:
            !El metodo obtiene aproximaciones para el problema del valor inicial
            ! y' = f(t,y)  , a <= t <= b  y(a) = alfa
            ! en (n+1) numeros igualmente espaciados del intervalo [a, b]
        ! con h = (b-a)/n y ti = a + i*h y si f es C5 entonces:
        
        !==Idea del algoritmo==:
        !w0 = alfa
        !k1 = h * f(ti,wi)
        !k2 = h * f(ti + (h/2), wi + (1/2)*k1)
        !k3 = h * f(ti + (h/2), wi + (1/2)*k2)
        !k4 = h * f(ti+1, wi + k3)
        !w_i+1 = wi + (1/6)*(k1 + 2*k2 + 2*k3 + k4)   
        !-----------------------------------------------------------------------------------    
        !Declaracion de dummy varialbes
        real(wp), intent(in)                    :: a    !extremo izquierdo del intervalo     
        real(wp), intent(in)                    :: b    !extremo derecho del intervalo
        real(wp), intent(in)                    :: h    !tamaño de paso
        real(wp), dimension(:), intent(in)      :: alfa !vector condicion inicial alfa = (x1_0, x2_0)
        
        !----------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp), dimension(size(alfa))         :: w  !almacena las sucesiva aproximaciones wi
        real(wp), dimension(size(alfa))         :: k1, k2, k3 , k4
        integer(il)                             :: i, j, fu, n
        character(80), parameter                :: archivo = 'salida_rk4_pendulo_PO.dat'
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        t = a
        n = size(alfa)
        !inicio explicitamente el w para notar que es un vector(directamente se puede hacer w = alfa)
        init_w: do j = 1 , n
            w(j) = alfa(j)
        end do init_w
        
        write(*,*) " h ", " t " , "   tita(t)   ", "  u(t)   "
        write(*,*) h, t , w(1), w(2)
        i = 1
        !-----------------------------------------------------------------------------------------
        !BLOQUE DE PROCESAMIENTO
        open(newunit=fu, file=archivo)
            write(fu, *) "            ti            ", "           tita(ti)                ", "      u(ti)       "
            write(fu, *) t,  w(1), w(2)
            main_do: do while (abs(t) < abs(b))
                k1 = h * F1(t, w)
                k2 = h * F1(t + (h/2.0_wp), w + (1.0_wp/2.0_wp) * k1 )
                k3 = h * F1(t + (h/2.0_wp), w + (1.0_wp/2.0_wp) * k2 )
                k4 = h * F1(t + h, w + k3)
                w = w + (1.0_wp/6.0_wp)*(k1 + 2.0_wp * k2 + 2.0_wp* k3 + k4)  !calcula wi
                t = a + i*h                                                   !calcula ti
                write(fu, *) t , w(1), w(2)
                i = i + 1
            end do main_do
        close(fu)
        !-----------------------------------------------------------------------------------------
     end subroutine rk4_vect_PO  
end module metodos
     
  



