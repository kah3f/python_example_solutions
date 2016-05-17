# -*- coding: utf-8 -*-
"""
This program computes the day of the week given a date in the Gregorian 
calendar.  The user inputs the day, month, and year as integers.
Author:    K. Holcomb
Changelog: Initial version 2013-05-20
"""

#Tables for lookups 
months=[0,3,3,6,1,4,6,2,5,0,3,5]
days=["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

day  =27
month=5
year =2015

D=day
M=months[month-1]
century = 100*(year//100)
Y=year-century

century_leap_year= century%400==0

leap_year    = False
if Y>0:
   leap_year = Y%4==0
elif century_leap_year:
   leap_year=True

L = Y//4

if century_leap_year:
    L+=1
    
if leap_year and month<3:
    L-=1

if century==1400 or century==1800 or century==2200:
    C=2
elif century==1500 or century==1900 or century==2300:
    C=0
elif century==1600 or century==2000 or century==2400:
    C=5
elif century==1700 or century==2100 or century==2500:
    C=4
else:
    print "This algorithm doesn't cover the century requested"
    C=-1

W=(C+Y+L+M+D)%7

print "The day of the week is ", days[W]
