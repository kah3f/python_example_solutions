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

  real, dimension(:), allocatable :: temps, storage
  real                            :: start,incr
  real, dimension(:), allocatable :: vals_added
  integer                         :: init_size,num_add
  integer                         :: i, indx

  init_size=6
  num_add  =1

  if (allocated(vals_added)) deallocate(vals_added)
  allocate(vals_added(num_add))
  vals_added=60.

  if ( allocated(temps) ) deallocate(temps)
  allocate(temps(init_size))

  start= 0.0
  incr =10.0

  temps(1)=start
  do i=2,init_size
    temps(i)=temps(i-1)+incr
  enddo

  if ( allocated(storage) ) deallocate(storage)
  allocate(storage(init_size+num_add))

  storage(:init_size)=temps
  storage(init_size+1:)=vals_added
  
  deallocate(temps)
  allocate(temps(size(storage)))
  temps=storage

  deallocate(storage)

  indx=indexx(temps,30.)

  write(*,*), size(temps),indx

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
