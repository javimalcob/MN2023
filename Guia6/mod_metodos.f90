module metodos
use mod_prec
use funciones
implicit none
!######################AAAAAAAAAAAAHHHHHH!!!!######################################################
contains  !<<<<<<!!COOOOOOOO   OOOONNNNNNNNTAAAAAAAAAAAAIIIIIIIIIINNNNNNNSSSSSSSSSS!!!!!!!!!!!! :c
!^^^^^^^^^^^^^^^^^^^^^ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!--------------------------------------------------------------------------------------------------
    subroutine euler1(a, b, n, alfa,ti, wi)
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
        integer(il), intent(in)                 :: n    !numero de intervalos deseado
        real(wp), intent(in)                    :: alfa !condicion inicial y(a) = alfa
        real(wp), dimension(0:n), intent(out)   :: ti   !vector ti de los tiempos 
        real(wp), dimension(0:n), intent(out)   :: wi   !vector wi de las aproximaciones a y(ti)
             
        !----------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp)                                :: w  !almacena las sucesivas aproximaciones wi
        real(wp)                                :: h  !paso constante del tiempo
        integer(il)                             :: i
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        h = (b - a) / n
        t = a
        w = alfa
        write(*,*) " h ", " t " , " w "
        write(*,*) h, t , w
        ti(0) = t
        wi(0) = w
        !-----------------------------------------------------------------------------------------
        !BLOQUE DE PROCESAMIENTO
        
        main_do: do i = 1 , n
            w = w + h * f(t, w)  !calcula wi
            t = a + i*h          !calcula ti
                
            !Almaceno los resultados en los vectores ti, wi
            ti(i) = t
            wi(i) = w
        end do main_do
        
     end subroutine euler1
     
     !----------------------------------------------------------------------------------------------- 
     subroutine rk2(a, b, n, alfa, ti , wi)
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
        integer(il), intent(in)                 :: n    !numero de intervalos deseado
        real(wp), intent(in)                    :: alfa !condicion inicial y(a) = alfa
        real(wp), dimension(0:n), intent(out)   :: ti   !vector ti de los tiempos 
        real(wp), dimension(0:n), intent(out)   :: wi   !vector wi de las aproximaciones a y(ti)
        !----------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp)                                :: w  !almacena las sucesiva aproximaciones wi
        real(wp)                                :: h  !paso constante del tiempo
        real(wp)                                :: k 
        integer(il)                             :: i
        
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        h = (b - a) / n
        t = a
        w = alfa
        write(*,*) " h ", " t " , " w "
        write(*,*) h, t , w
        ti(0) = t
        wi(0) = w
        !-----------------------------------------------------------------------------------------
        !BLOQUE DE PROCESAMIENTO
        main_do: do i = 1, n
            k = h * f(t, w)
            w = w + h*f(t + (h/2.0_wp) , w + (k/2.0_wp))    !calcula wi
            t = a + i*h                                     !calcula ti
            !Almacena los resultados en los vectores ti y wi
            ti(i) = t
            wi(i) = w
        end do main_do
        !-----------------------------------------------------------------------------------------
     
     end subroutine rk2   
     
       
    !----------------------------------------------------------------------------------------------- 
     subroutine rk4(a, b, n, alfa, ti , wi)
        !==Descripcion del metodo rk2==:
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
        integer(il), intent(in)                 :: n    !numero de intervalos deseado
        real(wp), intent(in)                    :: alfa !condicion inicial y(a) = alfa
        real(wp), dimension(0:n), intent(out)   :: ti   !vector ti de los tiempos 
        real(wp), dimension(0:n), intent(out)   :: wi   !vector wi de las aproximaciones a y(ti)
        !----------------------------------------------------------------------------------    
        !Declaracion de variables auxiliares
        real(wp)                                :: t  !almacena el tiempo ti, con  0<= i <=n
        real(wp)                                :: w  !almacena las sucesiva aproximaciones wi
        real(wp)                                :: h  !paso constante del tiempo
        real(wp)                                :: k1, k2, k3 , k4
        integer(il)                             :: i
        !--------------------------------------------------------------------------------------
        !Inicializacion de variables
        h = (b - a) / n
        t = a
        w = alfa
        write(*,*) " h ", " t " , " w "
        write(*,*) h, t , w
        ti(0) = t
        wi(0) = w
        !-----------------------------------------------------------------------------------------
        !BLOQUE DE PROCESAMIENTO
        main_do: do i = 1, n
            k1 = h * f(t, w)
            k2 = h * f(t + (h/2.0_wp), w + (1.0_wp/2.0_wp) * k1 )
            k3 = h * f(t + (h/2.0_wp), w + (1.0_wp/2.0_wp) * k2 )
            k4 = h * f(t + h, w + k3)
            w = w + (1.0_wp/6.0_wp)*(k1 + 2.0_wp * k2 + 2.0_wp* k3 + k4)  !calcula wi
            t = a + i*h                                                   !calcula ti
            !Almacena los resultados en los vectores ti y wi
            ti(i) = t
            wi(i) = w
        end do main_do
        !-----------------------------------------------------------------------------------------
     end subroutine rk4  
        
end module metodos




