program testquad
use utils
real sint, fint

interface 
  function f(x)
  real f
  real,intent(in):: x
  end function f
  
  function ff(x,y)
  real ff
  real,intent(in):: x,y
  end function ff

  function c(x)
  real c
  real,intent(in):: x
  end function c

  function d(x)
  real d
  real,intent(in):: x
  end function d
end interface

  rint=quad(f,-3.14159/2.,3.14159/2.)
  sint=dblequad(ff,c,d,-3.15159/2.0,3.14159/2.0)

  print*, rint, sint
end program testquad

function f(x)
real x
   f=cos(x)
end function f

function ff(x,y)
real x,y
   ff=cos(x)
end function ff

function c(x)
real x
   c=0.d0
end function c

function d(x)
real x
   d=2.*3.14159
end function d
