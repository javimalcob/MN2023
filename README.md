# MN2023
Curso de Metodos Numericos FAMAF 2023

## NOTAS GUIA 5
### Integracion numerica Regla del Trapecio
Area = (b-a)f(a) + (b-a)(f(b) - f(a)) / 2


```fortran
h = (b-a)/n
it = (f(a) + f(b)) / 2
do i = 1, n-1
    x = a + i*h
    it = it + f(xi)

end do
```
