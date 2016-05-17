"""
  This program does some more exercises with loops and lists.
  Author:     K. Holcomb
  Changelog:  Initial version 2015-05-27
"""

C=[]
F=[]

for i in range(0,61,10):
   Ctemp=float(i)
   C.append(Ctemp)
   F.append((9./5.)*Ctemp+32.)

for i in range(len(C)):
   print C[i], F[i]
