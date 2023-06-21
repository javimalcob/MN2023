module mod_prec
use ISO_FORTRAN_ENV

implicit none

integer(kind=int8), parameter :: is = int8, id = int16, il = int32 , ix = int64
integer(kind=int8), parameter :: rs = real32, rd = real64, rl = real128

integer(kind=int8), parameter :: wp = rd
real(kind=wp), parameter :: g = 10.0_wp
real(kind=wp), parameter :: l = 1.0_wp
real(kind=wp), parameter :: af= 3.14392_wp
real(kind=wp), parameter :: m = 0.001_wp
real(kind=wp), parameter :: N = 1000.0_wp
real(kind=wp), parameter :: gama = 0.1_wp
real(kind=wp), parameter :: beta= 0.5_wp
end module mod_prec
