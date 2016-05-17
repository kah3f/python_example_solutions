# -*- coding: utf-8 -*-
"""
Created on Mon May 20 10:30:26 2013
This program converts a sequence of decimal numbers into another base.
Author:     K. Holcomb
Changelog:  2013-05-20  Initial version 
"""

def convert_base(N,base):
   if i==0:
       conversion=str(i)
   else:
       digits=[]
       result=i
       while result>0:
           digit=result%base
           result=result//base
           if digit<=9:
               base_digit=str(digit)
           else:
               alpha_order=digit%10
               base_digit=chr(alpha_order+65)
           digits.insert(0,base_digit)
       conversion="".join(digits)
   return conversion

N   =int(raw_input("Please enter the maximum integer to be converted:"))
base=int(raw_input("Please enter the target base as an integer:"))

new_base="Base "+str(base)
print "   Base 10        %s"%new_base
for i in range(N+1):
    print "  %6d        %s" % (i,convert_base(i,base).rjust(6))
