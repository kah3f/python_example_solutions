def movie_dict(filmographies):
    """Create the dictionary"""
    castlist={}
    for actor in filmographies:
        for movie in actor.filmography:
           if movie in castlist:
               castlist[movie].add(actor.get_name())
           else:
               castlist[movie]=set()
               castlist[movie].add(actor.get_name())
    return castlist
