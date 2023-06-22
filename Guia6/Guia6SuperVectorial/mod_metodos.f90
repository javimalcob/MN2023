module metodos
use mod_prec
use funciones
implicit none

contains  !<<<<<<!!COOOOOOOOOOOONNNNNNNNTAAAAAAAAAAAAIIIIIIIIIINNNNNNNSSSSSSSSSS!!!!!!!!!!!! :c

!-------------------------------------------------------------------------------------------------------------------------
    !--------------------------------------------------------------------------------------------------------------------- 
    !#####################################################################################################################
    !###########    METODO DE EULER PARA SISTEMAS DE ECUACIONES DIFERENCIALES(PRIMER ORDEN)             ##################
    !#####################################################################################################################
    !---------------------------------------------------------------------------------------------------------------------
    subroutine euler1_vect(a, b, h, alfa)
        !==Descripcion del metodo de euler==:
            !El metodo obtiene aproximaciones para el problema del valor inicial
            ! y' = f(t,y)  , a <= t <= b  y(a) = alfa
            ! en (n+1) numeros igualmente espaciados del intervalo [a, b]
        !==Idea del algoritmo==:
        !w0 = alfa
        !w_i+1 = wi + h*f(ti, wi)    
        !-----------------------------------------------------------------------------------    
        !Declaracion de dummy varialbes
        real(wp), intent(in)                    :: a    !extremo izquierdo del intervalo     
        real(wp), intent(in)                    :: b    !extremo derecho del intervalo
        real(wp), intent(in)                    :: h    !tamaño de paso
        real(wp), dimension(:), intent(in)      :: alfa !condicion inicial alfa = (x1_0, x2_0)
        !----------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp), dimension(size(alfa))         :: w  !almacena las sucesivas aproximaciones wi
        integer(il)                             :: i, fu, j , n
        character(80), parameter                :: archivo = 'salida_euler_n_vectorial.dat'
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        t = a
        n = size(alfa)
        !inicio explicitamente el w para notar que es un vector(directamente se puede hacer w = alfa)
        init_w: do j = 1 , n
            w(j) = alfa(j)
        end do init_w
        
        write(*,*) " h ", " t " , "   w1   ", "  w2   "
        write(*,*) h, t , w(1), w(2) !do implicito!
        i = 1
        !-----------------------------------------------------------------------------------------
        !BLOQUE DE PROCESAMIENTO
        open(newunit=fu, file=archivo)
            write(fu, *) "            ti            ", "           w1i                ", "      w2i       "
            write(fu, *) t,  w(1), w(2)
            main_do: do while (abs(t) < abs(b))
                w = w + h * F(t, w) !calcula wi ,cada elemento de w es una EDO-1          
                t = a + i*h          !calcula ti
                write(fu, *) t , w(1), w(2)
                i = i + 1   
            end do main_do
        close(fu)
        !------------------------------------------------------------------------------------------
     end subroutine euler1_vect
     
    !--------------------------------------------------------------------------------------------------------------------- 
    !#####################################################################################################################
    !#######        METODO DE EULER MODIFICADO(SEGUNDO ORDEN) PARA SISTEMAS DE ECUACIONES DIFERENCIALES       ############
    !#####################################################################################################################
    !---------------------------------------------------------------------------------------------------------------------
    subroutine euler_mod_vect(a, b, h, alfa)
        !==Descripcion del euler modificado==:
            !El metodo obtiene aproximaciones para el problema del valor inicial
            ! y' = f(t,y)  , a <= t <= b  y(a) = alfa
            ! en (n+1) numeros igualmente espaciados del intervalo [a, b]
            ! con h = (b-a)/n y ti = a + i*h entonces:
        !==Idea del algoritmo==:
        !w0 = alfa
        !k1 = h*f(ti, wi)
        !k2 = h*f(t_i+1, wi + k1)
        !w_i+1 = wi + 1/2 *(k1 + k2)
        
        !Error del metodo Euler modificado
        !error local O(h**3), error global O(h**2)   
     
      !----------------------------------------------------------------------------------------------    
        !Declaracion de dummy varialbes
        real(wp), intent(in)                    :: a    !extremo izquierdo del intervalo     
        real(wp), intent(in)                    :: b    !extremo derecho del intervalo
        real(wp), intent(in)                    :: h    !tamaño de paso
        real(wp), dimension(:), intent(in)      :: alfa !vector condicion inicial alfa = (x1_0, x2_0)
        
        !--------------------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp), dimension(size(alfa))         :: w  !almacena las sucesiva aproximaciones wi
        real(wp), dimension(size(alfa))         :: k1, k2
        integer(il)                             :: i, fu, j , n
        character(80), parameter                :: archivo = 'salida_euler_mod_n_vectorial.dat'
        !---------------------------------------------------------------------------------------------
        !Inicializacion de variables
        
        t = a
        n = size(alfa)
        !inicio explicitamente el w para notar que es un vector(directamente se puede hacer w = alfa)
        init_w: do j = 1 , n
            w(j) = alfa(j)
        end do init_w
        
        write(*,*) " h ", " t " , "   w1   ", "  w2   "
        write(*,*) h, t , w(1), w(2)
        i = 1
        !----------------------------------------------------------------------------------------------
        !BLOQUE DE PROCESAMIENTO
        open(newunit=fu, file=archivo)
            write(fu, *) "            ti            ", "           w1i                ", "      w2i       "
            write(fu, *) t,  w(1), w(2)
            
            main_do: do while (abs(t) < abs(b))
                k1 = h * F(t, w)
                k2 = h * F(t + h, w + k1)
                w = w + (1.0_wp/2.0_wp) * (k1 + k2)             !calcula wi
                t = a + i*h                                     !calcula ti        
                write(fu, *) t , w(1), w(2)                                 
                i = i + 1
            end do main_do
        close(fu)
        !----------------------------------------------------------------------------------------------
     
    end subroutine euler_mod_vect
     
    !--------------------------------------------------------------------------------------------------------------------- 
    !#####################################################################################################################
    !#######        METODO DE RUNGE-KUTTA 2 (SEGUNDO ORDEN) PARA SISTEMAS DE ECUACIONES DIFERENCIALES         ############
    !#####################################################################################################################
    !---------------------------------------------------------------------------------------------------------------------
    subroutine rk2_vect(a, b, h, alfa)
        !==Descripcion del metodo rk2==:
            !El metodo obtiene aproximaciones para el problema del valor inicial
            ! y' = f(t,y)  , a <= t <= b  y(a) = alfa
            ! en (n+1) numeros igualmente espaciados del intervalo [a, b]
            ! con h = (b-a)/n y ti = a + i*h entonces:
        !==Idea del algoritmo==:
        !w0 = alfa
        !k = h*f(ti, wi)
        !w_i+1 = wi + h*f(ti + h/2, wi + k/2)   
        
        !Error del metodo RK2
        !error local O(h**3), error global O(h**2)
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
        real(wp), dimension(size(alfa))         :: k 
        integer(il)                             :: i, fu, j , n
        character(80), parameter                :: archivo = 'salida_rk2_n_vectorial.dat'
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        t = a
        n = size(alfa)
        !inicio explicitamente el w para notar que es un vector(directamente se puede hacer w = alfa)
        init_w: do j = 1 , n
            w(j) = alfa(j)
        end do init_w
        
        write(*,*) " h ", " t " , "   w1   ", "  w2   "
        write(*,*) h, t , w(1), w(2)
        i = 1
        !-----------------------------------------------------------------------------------------
        !BLOQUE DE PROCESAMIENTO
        open(newunit=fu, file=archivo)
            write(fu, *) "            ti            ", "           w1i                ", "      w2i       "
            write(fu, *) t,  w(1), w(2)
            
            main_do: do while (abs(t) < abs(b))
                k = h * F(t, w)
                w = w + h*F(t + (h/2.0_wp) , w + (k/2.0_wp))    !calcula wi
                t = a + i*h                                     !calcula ti        
                write(fu, *) t , w(1), w(2)                                 
                i = i + 1
            end do main_do
        close(fu)
        !-----------------------------------------------------------------------------------------
     end subroutine rk2_vect
     
    !--------------------------------------------------------------------------------------------------------------------- 
    !#####################################################################################################################
    !#######        METODO DE HEUN(TERCER ORDEN) PARA SISTEMAS DE ECUACIONES DIFERENCIALES                    ############
    !#####################################################################################################################
    !---------------------------------------------------------------------------------------------------------------------
    subroutine heun_vect(a, b, h, alfa)
        !==Descripcion del metodo de Heun==:
            !El metodo obtiene aproximaciones para el problema del valor inicial
            ! y' = f(t,y)  , a <= t <= b  y(a) = alfa
            ! en (n+1) numeros igualmente espaciados del intervalo [a, b]
        !==Idea del algoritmo==:
        !w0 = alfa
        !w_i+1 = wi + h/4 *[f(ti,wi) + 3*f(ti + (2*h/3), wi + (2*h/3)*f(ti+ (h/3), wi + (h/3)*f(ti, wi))]   
        
        !Error del metedo de Heun
        !error local O(h**4) y error global O(h**3)
        !-----------------------------------------------------------------------------------    
        !Declaracion de dummy varialbes
        real(wp), intent(in)                    :: a    !extremo izquierdo del intervalo     
        real(wp), intent(in)                    :: b    !extremo derecho del intervalo
        real(wp), intent(in)                    :: h    !tamaño de paso
        real(wp), dimension(:), intent(in)      :: alfa !condicion inicial alfa = (x1_0, x2_0)
        !----------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp), dimension(size(alfa))         :: w  !almacena las sucesivas aproximaciones wi
        integer(il)                             :: i, fu, j , n
        character(80), parameter                :: archivo = 'salida_heun_n_vectorial.dat'
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        t = a
        n = size(alfa)
        !inicio explicitamente el w para notar que es un vector(directamente se puede hacer w = alfa)
        init_w: do j = 1 , n
            w(j) = alfa(j)
        end do init_w
        
        write(*,*) " h ", " t " , "   w1   ", "  w2   "
        write(*,*) h, t , w(1), w(2)
        i = 1
        !-----------------------------------------------------------------------------------------
        !BLOQUE DE PROCESAMIENTO
        open(newunit=fu, file=archivo)
            write(fu, *) "            ti            ", "           w1i                ", "      w2i       "
            write(fu, *) t,  w(1), w(2)
            main_do: do while (abs(t) < abs(b))
                w = w + (h/4.0_wp) * (F(t, w) & 
                        + 3.0_wp * F(t+(2.0_wp*h)/3.0_wp, w &
                        + ((2.0_wp*h)/3.0_wp)*F(t + (h/3.0_wp), w &
                        + (h/3.0_wp) * F(t,w))))                    !calcula wi ,cada elemento de w es una EDO-1          
                t = a + i*h                                         !calcula ti
                write(fu, *) t , w(1), w(2)
                i = i + 1   
            end do main_do
        close(fu)
         !-----------------------------------------------------------------------------------------
     end subroutine heun_vect
       
    !-------------------------------------------------------------------------------------------------------------------- 
    !####################################################################################################################
    !#########           METODO DE RUNGE-KUTTA 4(CUARTO ORDEN) PARA SISTEMAS DE ECUACIONES DIFERENCIALES     ############
    !####################################################################################################################
    !--------------------------------------------------------------------------------------------------------------------
    
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
        
        !Error del Meteodo RK4
        !error local O(h**5) , error global O(h**4) 
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
        character(80), parameter                :: archivo = 'salida_rk4_n_vectorial.dat'
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        t = a
        n = size(alfa)
        !inicio explicitamente el w para notar que es un vector(directamente se puede hacer w = alfa)
        init_w: do j = 1 , n
            w(j) = alfa(j)
        end do init_w
        
        write(*,*) " h ", " t " , "   w1   ", "  w2   "
        write(*,*) h, t , w(1), w(2)
        i = 1
        !-----------------------------------------------------------------------------------------
        !BLOQUE DE PROCESAMIENTO
        open(newunit=fu, file=archivo)
            write(fu, *) "            ti            ", "           w1i                ", "      w2i       "
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
     
       
end module metodos
     
  



