"""This program reads data from the Christmas Bird Count for a particular
   species, then computes the minimum, the maximum, and the mean over the 
   interval for which data are available, then plots the counts as a function
   of time.
      Author: K. Holcomb
      Initial Version: 2013-05-24
"""
import numpy as np
import pylab as plt
import sys

if len(sys.argv) < 2:
   sys.exit("No data file specified")

try:
   infile=sys.argv[1]
except (ValueError,TypeError):
   sys.exit("Invalid file name")

try:
   fin=open(infile,"r")
except IOError:
   sys.exit("Unable to find data file")

speciesID=infile.split("_")[0]

#Loadtxt can take either a file name or a file descriptor.  If you give
#it the name you could put the try around the loadtxt.
(years,numObs)=np.loadtxt(infile,skiprows=1,usecols=(2,3),delimiter=",",unpack=True)

years=years+1900

#Check for sensible input
if np.size(numObs) < 2:
   sys.exit("Too few observations")

#Compute basic statistics
maxObs=np.max(numObs)
minObs=np.min(numObs)
meanObs=np.mean(numObs)
stdObs=np.std(numObs)

maxYears=years[numObs==maxObs]
maxString=",".join(map(str,maxYears))

print "The maximum number observed is %.0f, the minimum number is %.0f" \
                         % (maxObs,minObs)
print "The mean is %.2f" % meanObs
print "The std is %.2f" % stdObs

print "The maximum occurred in "+maxString

#Plot the result
plt.plot(years,numObs)
plt.show()
