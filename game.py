"""This plays an extremely stupid FPS text-based game.
   Author:    K. Holcomb
   Changelog: Initial version 2013-05-22
"""
import random
from characters import Zombie, Fighter

def print_status(status):
   print "Health: %.2f" % status[0], "Ammo: %.2f" % status[1]

#Permitted commands
comms=["flee","move","shoot","restore","reload","status","quit"]

#Maximum zombies per turn
max_zombies=4

print "Welcome to the Fighter versus Zombies game."
name=raw_input("Please enter the name of your character:")

player=Fighter(name)

Zombies=[]

while True:
   print "Please enter a command from"
   for comm in comms:
      print comm
   try:
      command=raw_input(":")
   except IOError:
      print "Unable to read command"
      continue

   if command not in comms:
      print "Invalid command, please try again:"
      continue

   if player.health<=0.01:
      print "%s's health is zero, thanks for playing!" % player.name
      print "Your score was %d" % player.score
      break

   if command=="flee":
      player.flee(Zombies)

   if command=="move":
      new_Zombie=player.move()
      if new_Zombie:
         nZombs=len(Zombies)
         Zombies.append(Zombie(nZombs))
         new_nZombs=len(Zombies)
         if new_nZombs==1:
           print "There is now a zombie."
         elif new_nZombs>1:
           print "There are now %d zombies." % new_nZombs
      else:
         print "No zombie appeared."
   elif command=="shoot":
      if len(Zombies)==0:
         print "There are no zombies to shoot!"
      else:
         target=random.randint(0,len(Zombies)-1)
         if player.shoot(Zombies[target]):
             del Zombies[target]
         else:
             player.take_hit(Zombies[target].hit_enemy())
         print "Number of remaining zombies is %d." % len(Zombies)
   elif command=="restore":
       player.restore()
       print_status(player.status())
   elif command=="reload":
       player.reload()
       print_status(player.status())
   elif command=="status":
       print_status(player.status())
   elif command=="quit":
       print "Thanks for playing! Your score is %d" % player.score
       break
