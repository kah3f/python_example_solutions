"""This program reads data from the Christmas Bird Count for a particular
   species, then computes the maximum, the mean, and the standard deviation
   over the interval for which data are available, then writes a file of a 
   reduced set of variables for plotting.  Does not use NumPy.
      Author: K. Holcomb
      Initial Version: 2013-05-24
"""
import math

def read_file(filename):
   fin=open(filename,"rb")
   #Skip header
   fin.readline()
   year=[]
   number=[]
   for line in fin:
      data=line.rstrip("\r\n").split(",")
      speciesID=data[0]
      speciesName=data[1]
      try:
         year.append(int(data[2])+1900)
         number.append(float(data[3]))
      except:
         continue
   return (speciesID,speciesName,year,number)

def mean(number):
   return sum(number)/len(number)

def std(number):
   variance=0.0
   my_mean=mean(number)
   for n in range(len(number)):
      variance+=(number[n]-my_mean)**2
   return math.sqrt(variance/len(number))

def maxval(number):
   #also see the enumerate built-in function
   max_num=number[0]
   max_ind=0
   for i in range(1,len(number)):
      if number[i]>max_num:
         max_num=number[i]
         max_ind=i
   return (max_num,max_ind)

filename=raw_input("Please enter the name of the file:")
(speciesID,speciesName,years,numbers)=read_file(filename)

meanObs=mean(numbers)
stdObs =std(numbers)
maxObs,maxInd=maxval(numbers)
maxYear=years[maxInd]

print "The maximum number observed is %.0f in year %d" % (maxObs,maxYear)
print "The mean is %.2f" % meanObs
print "The standard deviation is %.2f" % stdObs

#Write the data of interest to a file
nyears=len(years)
outfile=speciesID+".csv"
fout=open(outfile,"wb")
for n in range(nyears):
   line=str(years[n])+","+str(numbers[n])+"\n"
   fout.write(line)
