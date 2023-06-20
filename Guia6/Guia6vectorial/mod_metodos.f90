module metodos
use mod_prec
use funciones
implicit none
!######################AAAAAAAAAAAAHHHHHH!!!!######################################################
contains  !<<<<<<!!COOOOOOOOOOOONNNNNNNNTAAAAAAAAAAAAIIIIIIIIIINNNNNNNSSSSSSSSSS!!!!!!!!!!!! :c
!^^^^^^^^^^^^^^^^^^^^^ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!--------------------------------------------------------------------------------------------------
    subroutine euler1_vect(a, b, h, alfa)
        !NOTAS: No se usa n sino h porque puede generar fuentes de error 
        !guardar todo dentro de la rutina y no en el programa principal porque un vector con 50k de datos ocuparia mucha memoria 
        !n, ti , wi BORRAR   
    
    
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
        real(wp), dimension(1:2), intent(in)    :: alfa !condicion inicial alfa = (x1_0, x2_0)
        
             
        !----------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp), dimension(1:2)                :: w  !almacena las sucesivas aproximaciones wi
        integer(il)                             :: i, fu
        character(80), parameter                :: archivo = 'salida_euler_vectorial.dat'
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        t = a
        w(1) = alfa(1)
        w(2) = alfa(2)
        write(*,*) " h ", " t " , "   w1   ", "  w2   "
        write(*,*) h, t , w(1), w(2)
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
     end subroutine euler1_vect
     
    !----------------------------------------------------------------------------------------------- 
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
     
      !-----------------------------------------------------------------------------------    
        !Declaracion de dummy varialbes
        real(wp), intent(in)                    :: a    !extremo izquierdo del intervalo     
        real(wp), intent(in)                    :: b    !extremo derecho del intervalo
        real(wp), intent(in)                    :: h    !tamaño de paso
        real(wp), dimension(1:2), intent(in)    :: alfa !vector condicion inicial alfa = (x1_0, x2_0)
        
        !----------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp), dimension(1:2)                :: w  !almacena las sucesiva aproximaciones wi
        real(wp), dimension(1:2)                :: k 
        integer(il)                             :: i, fu
        character(80), parameter                :: archivo = 'salida_rk2_vectorial.dat'
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        
        t = a
        w(1) = alfa(1)
        w(2) = alfa(2)
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
        real(wp), dimension(1:2), intent(in)    :: alfa !vector condicion inicial alfa = (x1_0, x2_0)
        
        !----------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp), dimension(1:2)                :: w  !almacena las sucesiva aproximaciones wi
        real(wp), dimension(1:2)                :: k1, k2, k3 , k4
        integer(il)                             :: i, j, fu
        character(80), parameter                :: archivo = 'salida_rk4_vectorial.dat'
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        t = a
        w(1) = alfa(1)
        w(2) = alfa(2)
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
     
  



