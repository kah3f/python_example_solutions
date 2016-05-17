from filestats import Wordstats

filename='jqadams.txt'
filename2='jadams.txt'

jawords=Wordstats(filename)
jqawords=Wordstats(filename2)
histo=jawords.histogram(2)

print jawords.number_unique()
print jqawords.number_unique()

jqalist=jqawords.frequencies(25)
jalist=jawords.frequencies(25)
print jqalist
print jalist
