module utils
implicit none

! This module implements some handy utilities given that Fortran isn't
! Matlab.

interface linspace
  module procedure rspace, dspace
end interface

interface cbrt
  module procedure rcbrt, dcbrt
end interface

interface quad
  module procedure rsimpson, dsimpson
end interface

interface dblequad
  module procedure rdblequad, ddblequad
end interface

private :: rcbrt, dcbrt
private :: rspace, dspace
private :: rsimpson, dsimpson
private :: rdblequad, ddblequad

contains

!============= Linspace===========================

! Linearly-spaced array of values

! The array to be returned must be allocated in advance
! by one means or another. It must be large enough to 
! hold the results and if it is not exactly sized to
! Nints, it must be called with
! A(1:Nints)=linspace(lo,hi,Nints)

function rspace(lo,hi,Nints) result(rlinspace)
real,  dimension(Nints)           :: rlinspace
real,    intent(in)               :: lo, hi
integer, intent(in)               :: Nints

real                    :: h
integer                 :: i

   h=(hi-lo)/real(Nints-1)
   do i=1, Nints
      rlinspace(i)=lo+(i-1)*h
   enddo

end function rspace

function dspace(lo,hi,Nints) result(dlinspace)
double precision, dimension(Nints)  :: dlinspace
double precision,    intent(in)     :: lo, hi
integer,             intent(in)     :: Nints

double precision       :: h
integer                :: i

   h=(hi-lo)/real(Nints-1)
   do i=1, Nints
      dlinspace(i)=lo+(i-1)*h
   enddo

end function dspace

!===================================================

!============= Cube root ===========================

function rcbrt(x)
real             :: rcbrt
real, intent(in) :: x
real, parameter  :: third=1./3.

    rcbrt=x**third

end function rcbrt
    

function dcbrt(x)
double precision             :: dcbrt
double precision, intent(in) :: x
double precision, parameter  :: third=1.d0/3.d0

    dcbrt=x**third

end function dcbrt

!===================================================

!============= Integration ==========================
! Later I should make this adaptive, this will work for now

function rsimpson(f,a,b)
real                 :: rsimpson
real, intent(in)     :: a,b
interface
    function f(x)
    real             :: f
    real, intent(in) :: x
    end function f
end interface

integer             :: Nsteps=100
real                :: h, X, XI0, XI1, XI2
integer             :: i

   h=(b-a)/(2.*Nsteps)

   XI0=f(a)-f(b)
   XI1=0.
   XI2=0.

   do i=1,2*Nsteps-1
      X=a+i*h
      if ( mod(i,2) == 0 ) then
           XI2=XI2+f(X)
      else
           XI1=XI1+f(X)
      endif
   enddo

   rsimpson=h*(XI0+2.*XI2+4.*XI1)/3.

end function rsimpson

function dsimpson(f,a,b)
double precision                 :: dsimpson
double precision, intent(in)     :: a,b
interface
    function f(x)
    double precision             :: f
    double precision, intent(in) :: x
    end function f
end interface

integer             :: Nsteps=500
double precision    :: h, X, XI0, XI1, XI2
integer             :: i

   h=(b-a)/(2.*Nsteps)

   XI0=f(a)-f(b)
   XI1=0.
   XI2=0.

   do i=1,2*Nsteps-1
      X=a+i*h
      if ( mod(i,2) == 0 ) then
           XI2=XI2+f(X)
      else
           XI1=XI1+f(X)
      endif
   enddo

   dsimpson=h*(XI0+2.*XI2+4.*XI1)/3.

end function dsimpson


function rdblequad(f,c,d,a,b)
real                :: rdblequad
real, intent(in)    :: a,b
interface
   function f(x,y)
   real             :: f
   real, intent(in) :: x,y
   end function f
   function c(x)
   real             :: c
   real, intent(in) :: x
   end function c
   function d(x)
   real             :: d
   real, intent(in) :: x
   end function d
end interface

integer             :: Nsteps=100
real                :: h, hx, Ninv, cx, dx, x, y, z
real                :: J1, J2, J3, K1, K2, K3, L
integer             :: i,j

   h=(b-a)/(2.*Nsteps)

   J1=0.
   J2=0.
   J3=0.

   Ninv=1./(2.*real(Nsteps))

   do i=0,2*Nsteps
      x=a+i*h
      cx=c(x)
      dx=d(x)
      hx=(dx-cx)*Ninv
      K1=f(x,cx)+f(x,dx)
      K2=0.
      K3=0.
      do j=1,2*Nsteps-1
         y=cx+j*hx
         z=f(x,y)
         if ( mod(j,2) == 0 ) then
               K2=K2+z
         else
               K3=K3+z
         endif
      enddo 
      L=(K1+2.*K2+4.*K3)*hx/3.
      if ( i == 0 .or. i == 2*Nsteps ) then
           J1=J1+L
      else if ( mod(i,2) == 0 ) then
           J2=J2+L
      else 
           J3=J3+L
      endif
   enddo

   rdblequad=(J1+2.*J2+4.*J3)*h/3.

end function rdblequad


function ddblequad(f,c,d,a,b)
double precision                :: ddblequad
double precision, intent(in)    :: a,b
interface
   function f(x,y)
   double precision             :: f
   double precision, intent(in) :: x,y
   end function f
   function c(x)
   double precision             :: c
   double precision, intent(in) :: x
   end function c
   function d(x)
   double precision             :: d
   double precision, intent(in) :: x
   end function d
end interface

integer             :: Nsteps=500
double precision    :: h, hx, Ninv, cx, dx, x, y, z
double precision    :: J1, J2, J3, K1, K2, K3, L
integer             :: i,j

   h=(b-a)/(2.*Nsteps)

   J1=0.
   J2=0.
   J3=0.

   Ninv=1./(2.*real(Nsteps))

   do i=0,2*Nsteps
      x=a+i*h
      cx=c(x)
      dx=d(x)
      hx=(dx-cx)*Ninv
      K1=f(x,cx)+f(x,dx)
      K2=0.
      K3=0.
      do j=1,2*Nsteps-1
         y=cx+j*hx
         z=f(x,y)
         if ( mod(j,2) == 0 ) then
               K2=K2+z
         else
               K3=K3+z
         endif
      enddo 
      L=(K1+2.*K2+4.*K3)*hx/3.
      if ( i == 0 .or. i == 2*Nsteps ) then
           J1=J1+L
      else if ( mod(i,2) == 0 ) then
           J2=J2+L
      else 
           J3=J3+L
      endif
   enddo

   ddblequad=(J1+2.*J2+4.*J3)*h/3.

end function ddblequad


end module utils
