# MN2023
Curso de Metodos Numericos FAMAF 2023

## NOTAS GUIA 5
### Integracion numerica Regla del Trapecio
Area = (b-a)f(a) + (b-a)(f(b) - f(a)) / 2


```fortran
program trapecio
h = (b-a)/n
It = (f(a) + f(b)) / 2._wp
do i = 1, n-1
    it = it + f(a + i*h)

end do
it = it * h

end program trapecio
```
### Lista de Tarea
+ Subrutina Trapecio
+ Subrutina Simpson
