# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =#
#       Fichier : recap_Movies.R                                                                   #
#       Description : Fonction récap des films sur les bases                                       #
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =#

stat_Movies = function(data.Ratings){
  
  vect.Movies = unique(data.Ratings$movieID)
  nb.Movies = length(vect.Movies)
  
  stat.RatingPerMovie= as.data.frame(matrix(0, nrow = nb.Movies, ncol = 7) )
  colnames(stat.RatingPerMovie) = c("movieID","nb.Ratings","mean","sd", "max","min","med")
  # matrice comprenant l'ID du film 
  #                    le nombre d'utilisateurs ayant note le film
  #                    la moyenne des notes
  #                    l'ecart-type des notes
  #                    la note maximale 
  #                    la note minimale 
  #                    la mediane des notes
  
  for (movie_fakeID in 1:nb.Movies){
    movie = vect.Movies[movie_fakeID]
    x=data.Ratings$rating[data.Ratings$movieID == movie]
    stat.RatingPerMovie[movie_fakeID,] = c(movie,length(x),round(mean(x),2),round(sd(x),2),max(x),min(x),median(x))
  }

  return(stat.RatingPerMovie)
  
}

