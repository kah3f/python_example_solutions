import random

class Character(object):
   def __init__(self,health,ammo):
      self.health=health
      self.ammo=ammo

   def hit_enemy(self):
     if self.ammo>0:
        damage=random.random()
        self.ammo-=0.2*damage
     else:
        damage=0
     return damage

   def take_hit(self,damage):
     self.health-=damage*self.health

class Zombie(Character):
   def __init__(self,number):
     self.max_health=1
     self.max_ammo=1
     Character.__init__(self,self.max_health,self.max_ammo)
     self.number=number

   def die(self):
     if self.health<=0.25:
        return True
     else:
        return False

class Fighter(Character):
   def __init__(self,name):
      self.max_health=10
      self.max_ammo=10
      Character.__init__(self,self.max_health,self.max_ammo)
      self.name=name
      self.score=0

   def move(self):
      chance=random.random()
      if chance<0.5:
         return False
      else:
         return True

   def reload(self):
      gap=self.max_ammo-self.ammo
      self.ammo+=gap*random.random()

   def restore(self):
      wound=self.max_health-self.health
      self.health+=wound*random.random()

   def flee(self,zombies):
      del zombies[:]

   def shoot(self,zombie):
      if self.ammo>0: 
         zombie.take_hit(self.hit_enemy())
         if zombie.die():
            self.score+=1
            return True
         else:
            return False

   def status(self):
       return (self.health,self.ammo)
