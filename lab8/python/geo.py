from math import *
import numpy as np

"""This module contains several useful functions for geographical data"""

def extract_subgrid(lat_bounds,lon_bounds,lat,lon,topo):
#This function expects to extract a rectangular subgrid.  
    (nlat,nlon)=np.shape(topo)
    if ( lat_bounds[0] > lat_bounds[1] ):
        lat_cond=np.logical_and(lat<=lat_bounds[0],lat>=lat_bounds[1])
    else:
        lat_cond=np.logical_and(lat>=lat_bounds[0],lat<=lat_bounds[1])

    lon_cond=np.logical_and(lon>lon_bounds[0],lon<lon_bounds[1])
    sublats=np.compress(lat_cond,lat)
    sublons=np.compress(lon_cond,lon)
    subgrid=topo[lat_cond,:]
    subgrid=subgrid[:,lon_cond]
    return (sublats,sublons,subgrid)

def geo_area(mask,lat,radius):
    (nlat,nlon)=np.shape(mask)
    dtheta=pi/nlat
    dphi=2.*pi/nlon
    latsum=sum([sum(mask[i,:]*np.cos(np.radians(lat[i])))for i in range(nlat)])
    return radius**2*dphi*dtheta*latsum

def geo_volume(mask,topo,lat,radius):
    (nlat,nlon)=np.shape(topo)
    dtheta=pi/nlat
    dphi=2.*pi/nlon
    vol=np.where(mask,abs(topo),0)
    latsum=sum([sum(vol[i,:]*np.cos(np.radians(lat[i])))for i in range(nlat)])
    return radius**2*dtheta*dphi*latsum
    
