program topography
! 
! This program reads topographical (elevation) data and computes some 
! interesting values from it.
! We assume a full range of latitudes and longitudes in the initial data.
!
! Author:    K. Holcomb
! Changelog: 2014-04-03 Initial version modified from 2013 version
!
use geo
implicit none

real,    dimension(:,:), allocatable :: elevation
real,    dimension(:),   allocatable :: lat, lon
integer                              :: nlats, nlons
real,    parameter                   :: circle=360.
integer, dimension(2)                :: maxcoords,mincoords
real,    dimension(2)                :: lat_bounds,lon_bounds
real                                 :: ocean_area, ocean_volume
real,    parameter                   :: E_radius=6378100 
real                                 :: start_lat,start_lon
real                                 :: lat_res,lon_res
real,   dimension(:),   allocatable  :: na_lat,na_lon
real,   dimension(:,:), allocatable  :: na_topo
integer                              :: nargs,ios
integer                              :: punit,tunit
integer                              :: i,j,k
character(len=40)                    :: paramfile,topofile

!Add params namelist here

! Get file names from command line
    nargs=command_argument_count()
    if (nargs < 1) then
        stop "Topo file not specified"
    else if (nargs < 2) then
        paramfile="params.txt"
        call get_command_argument(1,topofile)     
    else if (nargs > 2 .and. nargs < 3) then
         stop "Topo file not specified"
    else
        call get_command_argument(1,paramfile)
        call get_command_argument(2,topofile)
    endif

! Read input parameters from namelist file
    punit=10
    open(punit,file=paramfile,iostat=ios)
    if ( ios .eq. 0 ) then
        read(10,params)
    else
        stop "Unable to open parameter file"
    endif

! Read topographical data
    tunit=11
    open(tunit,file=topofile,iostat=ios)
    if ( ios .ne. 0 ) then
        stop "Unable to open topography file"
    endif

    allocate(lat(nlats),lon(nlons))
    allocate(elevation(nlats,nlons))

! Set up lat/lon arrays

    do i=1,nlats
       read(11,*) elevation(i,:)
    enddo

    lon(1)=start_lon
    do j=2,nlons
       lon(j)=lon(j-1)+lon_res
    enddo

    lat(1)=start_lat
    if ( start_lat > 0.0 ) then
       do i=2,nlats
          lat(i)=lat(i-1)-lat_res
       enddo
    else
       do i=2,nlats
          lat(i)=lat(i-1)+lat_res
       enddo
    endif

! Compute some interesting figures

    maxcoords=maxloc(elevation)
    mincoords=minloc(elevation)

    write(*,'(a,f8.1,a,2f6.1)') 'Maxval ',maxval(elevation),' at ',            &
                                         lat(maxcoords(1)),lon(maxcoords(2))

    write(*,'(a,f8.1,a,2f6.1)') 'Minval ',minval(elevation),' at ',            &
                                         lat(mincoords(1)),lon(mincoords(2))

    ocean_area  =area(E_radius,lat_res,lon_res,lat,elevation,nlats,nlons)
    ocean_volume=volume(E_radius,lat_res,lon_res,lat,elevation,nlats,nlons)

    write(*,'(a,es10.3)') 'Total ocean surface area in m^2 is approximately ', &
                                           ocean_area
    write(*,'(a,f6.3)') 'Ocean surface area as fraction of Earth area is about'&
                                          ,ocean_area/(4.*pi*E_radius**2)
    write(*,'(a,es10.3)') 'Total ocean volume in m^3 is approximately ',       &
                                           ocean_volume

! Extract North America
    lat_bounds(1)=12.0
    lat_bounds(2)=66.0
    lon_bounds(1)=230.
    lon_bounds(2)=300.
    call extract_subgrid(nlats,nlons,lat_bounds,lon_bounds,lat,lon,elevation,  &
                                                       na_lat,na_lon,na_topo)

    open(13,file='plotna.csv')
    call csv_write(13,na_lat)
    call csv_write(13,na_lon)
    do i=1,size(na_lat)
       call csv_write(13,na_topo(i,:))
    enddo

end program topography
