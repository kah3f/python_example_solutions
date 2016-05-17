import sys
from ActorData import Actor

def movie_dict(filmographies):
    """Create the dictionary"""
    castlist={}
    for actor in filmographies:
        for movie in actor.get_filmlist():
           if movie in castlist:
               castlist[movie].add(actor.get_name())
           else:
               castlist[movie]=set()
               castlist[movie].add(actor.get_name())
    return castlist

def compress_ws(string):
    """Removes excess whitespace."""
    return ' '.join(string.split())

def fix_string(name):
    """Manipulates a string into a title format with no leading the or apostrophes."""
    name1=name.strip()
    name2=name1.replace("'","")
    name3=name2.lower().strip()
    if name3.startswith("the "):
        fixed_name=compress_ws(name3[4:])
    else:
        fixed_name=compress_ws(name3)           
    return fixed_name.title()

def fix_data(movies):
    """Adjusts titles to standard format.  Works through side effects."""
    for i,movie in enumerate(movies):
        movies[i]=fix_string(movie)

def prntCastList(castlist,movie):
    if movie in castlist:
        print ",".join(list(set(castlist[movie])))
    else:
        print
        print"Cannot find this movie."
        print

def main():

    if len(sys.argv)<2:
        print "Error: no data file specified.  Using default."
        in_file="movies.txt"
    else:
        in_file=sys.argv[1]

    try:
        moviefile=open(in_file,'rU')
    except IOError:
        sys.exit("Unable to open movie data file.")

    actors=[]
    for line in moviefile:
        player,movies=line.rstrip("\r\n").split(";")
        values=movies.rstrip(',').split(',')
        fix_data(values)
        actors.append(Actor(player,values))
      
    castlist=movie_dict(actors)

    while True:
        movie=raw_input("Type a movie name, or q/Q to quit:")
        if movie.lower()=='q':
            break
        else:
            prntCastList(castlist,movie)

if __name__=='__main__':
    main()
