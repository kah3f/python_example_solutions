program testquad
use utils
double precision sint, fint

interface 
  function f(x)
  real f
  real,intent(in):: x
  end function f
  
  function ff(x,y)
  double precision ff
  double precision,intent(in):: x,y
  end function ff

  function c(x)
  double precision c
  double precision,intent(in):: x
  end function c

  function d(x)
  double precision d
  double precision,intent(in):: x
  end function d
end interface

  fint= quad(f,0.,3.14159)
  sint=dblequad(ff,c,d,.1d0,.5d0)

  print*,fint, sint
end program testquad

function f(x)
real x
   f=sin(x)
end function f

function ff(x,y)
double precision ff
double precision x,y
   ff=exp(y/x)
end function ff

function c(x)
double precision c
double precision x
   c=x**3
end function c

function d(x)
double precision d
double precision x
   d=x**2
end function d
