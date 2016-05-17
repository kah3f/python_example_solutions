! Example program
! Author:    K. Holcomb
! Changelog: 20150604 Initial version

program main
  implicit none

  interface
    integer function indexx(arr,value)
    implicit none
    real, dimension(:), intent(in) ::  arr
    real,               intent(in) ::  value
    end function indexx
  end interface

  real, dimension(10) :: temps
  real                :: incr
  integer             :: i, indx, counter

  temps=-999.
  temps(1)=0.
  incr=10.
  do i=2,6
    temps(i)=temps(i-1)+incr
  enddo

  counter=0
  do i=1,size(temps)
    if (temps(i) > -999.) then
        counter=counter+1
    endif
  enddo

  indx=indexx(temps,30.)

  write(*,*), counter,indx

end

integer function indexx(arr,value)
    implicit none
    real, dimension(:), intent(in) ::  arr
    real,               intent(in) ::  value
    integer                        ::  i

    indexx = -999
    do i=1,size(arr)
      if (arr(i)==30.) then
         indexx=i
         exit
      endif
    enddo
end function 
