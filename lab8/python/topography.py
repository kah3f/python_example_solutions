import sys
import math
import numpy as np
import pylab as plt
import geo
from mpl_toolkits.basemap import Basemap

def max_loc(arr):
    return np.where(arr==arr.max())

def min_loc(arr):
    return np.where(arr==arr.min())

def main():

# Set constants
    earth_radius=6378100.  # in meters (elevation data is in meters)

    lat_start=89.5
    lon_start=0.5
    lat_res=-1.0
    lon_res=1.0

    if len(sys.argv)<2:
       sys.exit("You did not provide a file")
    else:
       filename=sys.argv[1]

# Read the topograpical data
    topo=np.loadtxt(filename)

#  Compute the latitude and longitude arrays
#  Several ways to do this
    lat_end=-lat_start

    lon_end=lon_start+359./lon_res

    if lat_start > lat_end:
        lat=np.arange(lat_start, lat_end-1, -lat_res)
    else:
        lat=np.arange(lat_start, lat_end+1, lat_res)

    lon=np.arange(lon_start, lon_end+1, lon_res)

    nlats=len(lat); nlons=len(lon)

#  Part 1.  Make a contour plot of the data and annotate with the maximum
#  and minimum locations.

    maxlocs=max_loc(topo)
    minlocs=min_loc(topo)
    maxcoords=(lat[maxlocs[0]],lon[maxlocs[1]])
    mincoords=(lat[minlocs[0]],lon[minlocs[1]])
    print "The maximum elevation %5.1f occurs at %5.1f %5.1f" % (topo.max(),maxcoords[0],maxcoords[1])
    print "The minimum elevation %5.1f occurs at %5.1f %5.1f" % (topo.min(),mincoords[0],mincoords[1])

    fig=plt.figure()
    plt.contourf(lon,lat,topo)
    plt.colorbar()
# Notice plotting order (due to lat,lon being actually y,x in the conventional
# representation)
    plt.plot(maxcoords[1],maxcoords[0],'*',color='black')
    plt.plot(mincoords[1],mincoords[0],'*',color='black')
    plt.annotate("Maximum elevation",xy=(maxcoords[1]+0.5,maxcoords[0]+0.5))
    plt.annotate("Minimum elevation",xy=(mincoords[1]+0.5,mincoords[0]+0.5))
    plt.title("Contour Map of Earth Topography at 1deg Resolution")
    plt.xlabel("Longitude")   
    plt.ylabel("Latitude")   
    plt.show()

# Part 2.  Compute some interesting figures.

# Compute fraction of surface that is ocean
    mask=np.where(topo<0,True,False)
    print "The surface area of the ocean is approximately %8.3e m^2" % \
           geo.geo_area(mask,lat,earth_radius)

    ofrac=geo.geo_area(mask,lat,earth_radius)/(4.*math.pi*earth_radius**2)
    print "The fractional surface area of the Earth that is ocean is approximately %6.3f" % ofrac

# Compute the volume of the ocean
# Mask is same here
    ovol=geo.geo_volume(mask,topo,lat,earth_radius)
    print "The volume of the ocean is approximately %8.3e m^3" % ovol

#Part 3.  Decorate with Basemap.
    m=Basemap(projection='mill',lat_ts=10,
              llcrnrlon=lon.min(),urcrnrlon=lon.max(),
              llcrnrlat=lat.min(),urcrnrlat=lat.max(),
              resolution='c')
    Lon,Lat=plt.meshgrid(lon,lat)
    x,y = m(Lon,Lat)
    plt.figure()
    surf_t=m.pcolormesh(x,y,topo,shading='flat',cmap=plt.cm.jet)
    m.drawcoastlines()
    plt.colorbar(surf_t)
    plt.title('Earth Topography')
    plt.show()
               

# Part 4.  Extract North America and plot.
    lat_bounds=np.array([12.,66.])
    lon_bounds=np.array([230.,300.])
    (na_lats,na_lons,na_elevs)=geo.extract_subgrid(lat_bounds,lon_bounds,lat,lon,topo)

    plt.clf()
    plt.contourf(na_lons,na_lats,na_elevs)
    plt.colorbar()
    plt.title("Contour Map of North America at 1deg Resolution")
    plt.xlabel("Longitude")   
    plt.ylabel("Latitude")   
    plt.show()

if __name__=="__main__":
    sys.exit(main())
