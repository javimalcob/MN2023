program testp1
use iso
use funciones
implicit none
      !Declaracion de variables
      real(wp)              :: deltaf, x, h, error
      integer(il)           :: fu, i 
      character(80)         :: archivo
      !Entrada de datos
      x = 2.0_wp
      h = 1
      archivo = "datosp1g4.dat"

      !Bloque de Procesamiento
      open(newunit=fu, file=archivo)
          write(*,*) "n ","deltaf(x)  ", "   df(x)       " ,  " error   "
          do i = 1, 10
              h = h/ 10._wp
              deltaf = (f(x + h) - f (x - h))/(2*h)
              error = abs(df(x) - deltaf)
              write(fu,*) i, deltaf, df(x) , error
              write(*,*) i, deltaf, df(x) , error
          end do
      close(fu)
end program testp1

