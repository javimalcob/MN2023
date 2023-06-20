module mod_prec
use ISO_FORTRAN_ENV

implicit none

integer(kind=int8), parameter :: is = int8, id = int16, il = int32 , ix = int64
integer(kind=int8), parameter :: rs = real32, rd = real64, rl = real128

integer(kind=int8), parameter :: wp = rd
real(kind=wp), parameter :: g = 9.67978_wp
real(kind=wp), parameter :: l = 1.0_wp
real(kind=wp), parameter :: af= 3.14392_wp
real(kind=wp), parameter :: m = 0.001_wp
end module mod_prec
