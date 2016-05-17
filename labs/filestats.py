import string

class Wordstats:

   def __init__(self,filename):
      try:
         fin=open(filename,'r')
      except IOError:
         print "Unable to open file %s"%filename
         return None

      ignore=['the', 'of', 'and', 'to', 'a', 'in', 'be','been', 'it', 'by', 'if', 'that', 'or', 'for', 'which', 'this', 'an', 'all', 'its', 'not', 'with', 'their','they','them','me','my', 'is', 'as', 'from', 'may', 'i', 'on', 'but', 'can', 'only', 'his','her','our','there','here','what','when','where','by','be','is','are','have','has','had','he','she','no','we','us','you','your','these','those','was','were','above','below','behind','beneath','before','after','upon','under','between','through','at','any','all','so','than']

      self.counts = {}                    
      for line in fin:
        words=line.split()
        for word in words:
           word=word.lower().strip(string.punctuation)
           if word in ignore:
              continue
           elif word not in self.counts: 
              self.counts[word] = 1   
           else:
              self.counts[word] += 1  

   def number_unique(self):
       return len(self.counts)

   def frequencies(self,N=0):
       if N==0:
          return sorted(self.counts,key=self.counts.get,reverse=True)
       else:
          return sorted(self.counts,key=self.counts.get,reverse=True)[:N]

   def match_words(self,compare):
       match_list=[]
       for word in compare:
           if word in self.counts:
              match_list.append(word)
       return (match_list,len(match_list))

   def histogram(self,N):
       freq_list=self.frequencies(N)
       for word in freq_list:
           self.counts[word]
