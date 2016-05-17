# -*- coding: utf-8 -*-
"""
This program investigates the Collatz conjecture.  According to this conjecture,
given an integer N, if N is odd we multiply it by 3 and add 1, and if it is even
we divide it by two.  We repeat the process from a starting N until 1 is 
reached.  The Collatz conjecture is that 1 will always be reached.
Author:   K. Holcomb
Date:     2013-05-20
"""
i=27
seq=[i]
result=i
while result>1:
    if result%2 == 0:   # even
       result=result//2
    else:
       result=3*result+1
    seq.append(result)
print i, len(seq), max(seq)
