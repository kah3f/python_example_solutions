"""
 This program does a few exercises with loops and lists.
 Author:    K. Holcomb
 Changelog: Initial version 2015-05-27
"""

Temps=range(0,51,10)

#or Temps=[0,10,20,30,40,50]

print "The number of temperatures is ",len(Temps)
print "The index of 30 deg is ", Temps.index(30)

Temps.append(60)

for C in Temps:
   F=(9./5.)*C+32
   print float(C),F

