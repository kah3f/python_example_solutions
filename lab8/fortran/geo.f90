module geo
implicit none

   real                                      :: pi=4.0*atan(1.0)

contains

   elemental function radians(theta)
   real                                      :: radians
   real,                          intent(in) :: theta
      radians=theta*pi/180.
   end function radians


   function area(radius,lat_res,lon_res,lat,topo,nlat,nlon)
   real                                      :: area
   real                         , intent(in) :: radius
   real                         , intent(in) :: lat_res,lon_res
   real,    dimension(nlat)     , intent(in) :: lat
   real,    dimension(nlat,nlon), intent(in) :: topo
   integer,                       intent(in) :: nlat,nlon

   real,    dimension(nlat)                  :: rlat, lat_line
   integer, dimension(nlat,nlon)             :: mask
   real                                      :: r2, dphi, dtheta
   integer                                   :: i,j

! Assume constant dlat and dlon.  Make sure lat_res is positive.
      r2=radius**2

      dphi  =radians(lon_res)
      dtheta=radians(abs(lat_res))

! Note tradeoff of memory and speed (assuming array operations are easily
! optimized by the compiler).  Also this code should be clean and easy to read.

      rlat=radians(lat)
      
      where (topo<0) 
         mask=1
      else where 
         mask=0
      end where

      do i=1,nlat
         lat_line(i)=sum(mask(i,:)*cos(rlat(i))*dphi*dtheta)
      enddo

      area=r2*sum(lat_line)

   end function area


   function volume(radius,lat_res,lon_res,lat,topo,nlat,nlon)
   real                                        :: volume
   real,      dimension(nlat)     , intent(in) :: lat
   real,      dimension(nlat,nlon), intent(in) :: topo
   real                           , intent(in) :: radius
   real                           , intent(in) :: lat_res,lon_res
   integer,                         intent(in) :: nlat,nlon

   real,    dimension(nlat)                    :: rlat, lat_line
   integer, dimension(nlat,nlon)               :: mask
   real                                        :: r2, dphi, dtheta
   integer                                     :: i,j

! Assume constant dlat and dlon.  Make sure lat_res is positive.
      r2=radius**2

      dphi  =radians(lon_res)
      dtheta=radians(abs(lat_res))
      rlat  =radians(lat)

      where ( topo <= 0 ) 
         mask=abs(topo)
      else where
         mask=0
      end where

      do i=1,nlat
         lat_line(i)=sum(mask(i,:)*cos(rlat(i))*dtheta*dphi)
      enddo

      volume=r2*sum(lat_line)

   end function volume


   subroutine extract_subgrid(nx,ny,x_bounds,y_bounds,x,y,arr,             &
                                                 subx,suby,subarr)
!  This subroutine expects to extract a rectangular subgrid.

   integer                           , intent(in)  :: nx, ny
   real,  dimension(2)               , intent(in)  :: x_bounds, y_bounds
   real,  dimension(nx)              , intent(in)  :: x
   real,  dimension(ny)              , intent(in)  :: y
   real,  dimension(nx,ny)           , intent(in)  :: arr
   real,  dimension(:)  , allocatable, intent(out) :: subx,suby
   real,  dimension(:,:), allocatable, intent(out) :: subarr

   logical, dimension(nx)                          :: xmask 
   logical, dimension(ny)                          :: ymask 
   integer                                         :: nsubx,nsuby
   integer                                         :: istart,istop,jstart,jstop
   integer                                         :: i,j

      xmask=.false.
      ymask=.false.

      if ( x_bounds(1) > x_bounds(2) ) then
          where ( x <= x_bounds(1) .and. x>=x_bounds(2) )
             xmask=.true. 
          endwhere
      else
          where ( x >= x_bounds(1) .and. x<=x_bounds(2) )
             xmask=.true. 
          endwhere
      endif

      where ( y >= y_bounds(1) .and. y<=y_bounds(2) )
           ymask=.true.
      endwhere

      nsubx=count(xmask)
      nsuby=count(ymask)

      allocate(subx(nsubx),suby(nsuby))
      allocate(subarr(nsubx,nsuby))

      istart=1
      istop=nx
      do i=2,nx-1
         if ( .not. xmask(i-1) .and. xmask(i) ) istart=i
         if ( xmask(i) .and. .not. xmask(i+1) ) istop=i
      enddo

      jstart=1
      jstop=ny
      do j=2,ny-1
         if ( .not. ymask(j-1) .and. ymask(j) ) jstart=j
         if ( ymask(j) .and. .not. ymask(j+1) ) jstop=j
      enddo

      subx=x(istart:istop)
      suby=y(jstart:jstop)
      subarr=arr(istart:istop,jstart:jstop)

   end subroutine extract_subgrid

  
end module geo
   
