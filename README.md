# MN2023
Curso de Metodos Numericos FAMAF 2023

## NOTAS GUIA 5
### Integracion numerica Regla del Trapecio
Area = (b-a)f(a) + (b-a)(f(b) - f(a)) / 2


```fortran
program trapecio
h = (b-a)/n
it = (f(a) + f(b)) / 2._wp
do i = 1, n-1
    it = it + f(a + i*h)

end do
it = it * h

end program trapecio
```
### Lista de Tarea
+ Subrutina Trapecio
+ Subrutina Simpson

```bash
   ssh -X jluna@surubi.famaf.unc.edu.ar
   scp -r dir jluna@surubi:/home/jluna/NM/Guias/Ps
   scp jluna@surubi:/path/archivo ./
```
[comandos ssh](https://geekytheory.com/copiar-archivos-a-traves-de-ssh-con-scp/)
