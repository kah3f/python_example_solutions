class Actor (object):
    """Encapsulates data about an actor and his resume"""
   
    def __init__(self,name,filmography=""):
        self._name=name
        self._filmography=filmography

    def get_name(self):
        return self._name

    def get_filmlist(self):
        return self._filmography

    def get_filmography(self):
        return "".join(self._filmography)

    def printme(self):
        print self.get_name()," has appeared in ",self.get_filmography()
