"""
  This program computes and prints a table the sums of the integers from 1 to N
  Author:    K. Holcomb
  Changelog: Initial version 2015-05-27 
"""

print "Number     Loop Sum    Formula "
Max_N=25
for N in range(1,Max_N+1):
    sum_it=0
    for i in range(1,N+1):
       sum_it+=i
    print N, sum_it, N*(N+1)/2

