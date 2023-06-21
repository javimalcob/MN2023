module mod_prec
use ISO_FORTRAN_ENV

implicit none

integer(kind=int8), parameter :: is = int8, id = int16, il = int32 , ix = int64
integer(kind=int8), parameter :: rs = real32, rd = real64, rl = real128

integer(kind=int8), parameter :: wp = rd

real(kind=wp), parameter :: r = 2.0_wp
real(kind=wp), parameter :: K = 100.0_wp
end module mod_prec
