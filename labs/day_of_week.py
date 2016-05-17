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

#Request input from user.
day  =int(raw_input("Please input the day as an integer:"))
month=int(raw_input("Please input the month as an integer:"))
year =int(raw_input("Please input the year as an integer:"))

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

#Validate the input
if month<1 or month>12:
   valid_date=False
elif D<1 or D>days[month-1]:
   valid_date=False
elif month==2 and not (leap_year or century_leap_year):
   if D>28:
      valid_date=False
   else:
      valid_date=True
else:
   valid_date=True

if not valid_date:
   print "Please enter a valid date."

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
