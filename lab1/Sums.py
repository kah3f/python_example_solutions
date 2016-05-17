"""
  This program computes the sum of the integers from 1 to N
  Author:    K. Holcomb
  Changelog: Initial version 2015-05-27 
"""

N=25

sum_it=0
for i in range(1,N+1):
   sum_it+=i

print N, sum_it, N*(N+1)/2

