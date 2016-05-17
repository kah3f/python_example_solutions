module strings
! A few useful string utilities

integer, parameter, private                 :: MAX_LINE  =132
integer, parameter, private                 :: MAX_FIELDS=132

contains

subroutine split_line(line,fields,delimiter)
character(len=*)                            :: line
character(len=*),dimension(:), allocatable  ::  fields
chararacter(len=1), optional                ::  delimiter

character(len=len_trim(line))               :: tempstrg,rem_string
character(len=MAX_LINE),dimension(:),allocatable :: fn
character(len=1)                            :: delimiter,ch

integer                                     :: i, n

integer                                     :: nfiles

! Parses the single character string into fields and returns them as an array.

  if ( present(delimiter) ) then
     delimit=delimiter
  else
     delimit=' '
  endif

  tempstrg=adjustl(line)

  allocate(fn(MAX_FIELDS))

  n=1
  do
     call split(tempstrg,delimiter,fn(n),rem_string)
     tempstrg=adjustl(rem_string)
     if ( tempstrg .eq. '' ) then
        exit
     else
        n=n+1
     endif

  enddo

  allocate(fields(n))

  do i=1,n
     fields(i)=trim(adjustl(fn(i)))
  enddo

end subroutine split_line


subroutine split(string,delimiter,res_string,rem_string)
character(len=1)                             ::  delimiter
character(len=*)                             ::  string
character(len=*)                             ::  res_string, rem_string

integer                                      ::  i, ii, loc

! Splits a string and returns the part of the string before a delimiter, along
! with the remainder of the string.

  res_string=''
  rem_string=''

  string=adjustl(string)

  loc=scan(string,delimiter)

! Return if we don't find the delimiter in the string
  if (loc .eq. 0) then
      res_string=adjustl(string)
      rem_string=''
      return
  endif

  do i=1,loc-1
     res_string(i:i)=string(i:i)
  enddo

  do i=loc+1,len_trim(string)
     ii=i-loc
     rem_string(ii:ii)=string(i:i)
  enddo

end subroutine split

subroutine ws_strip(string)
character(len=*)                             ::  string

character(len=len(string))                   ::  tmpstring
character(len=1)                             ::  c
integer                                      ::  i, n, len_string

! Removes whitespace (spaces and tabs) from a string.

  string=adjustl(string)
  len_string=len_trim(string)

  tmpstring=''

  n=0
  do i=1, len_string
     c = string(i:i)
     if ( (c .eq. ' ') .or. (c .eq. '\t') ) then
         cycle
     else
        n=n+1
        tmpstring(n:n)=c
     endif
  enddo

  string=trim(adjustl(tmpstring))

end subroutine ws_strip


end module strings
