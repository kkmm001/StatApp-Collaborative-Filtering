stat_Movies = function(data.Ratings){
  # INPUT data.Ratings  : la base des notes
  # OUTPUT              : la base contenant les statistiques sur les notes des films
  
  # Les statistiques pour un film donné sont : 
  # - nb.Ratings  : le nombre de notes reçues ; 
  # - mean        : la moyenne de ses notes ; 
  # - sd          : l'écart-type de ses notes ; 
  # - max         : la note maximale reçue ; 
  # - min         : la note minimale reçue ; 
  # - med         : la note médiane
  
  movieID = sort(unique(data.Ratings$movieID))
  stat.Movies = as.data.frame(movieID)
  
  # Détermination du nombre de notes reçues
  stat.Movies$nb.Ratings = tapply(data.Ratings$rating, data.Ratings$movieID, length)

  # Détermination de la moyenne de chaque film
  stat.Movies$mean = tapply(data.Ratings$rating, data.Ratings$movieID, function(x) round(mean(x),2))
  
  # Détermination de l'écart-type de chaque film
  stat.Movies$sd = tapply(data.Ratings$rating, data.Ratings$movieID, function(x) round(sd(x),2))
  
  # Détermination de la note maximale de chaque film
  stat.Movies$max = tapply(data.Ratings$rating, data.Ratings$movieID, function(x) round(max(x),2))
  
  # Détermination de la note minimale de chaque film
  stat.Movies$min = tapply(data.Ratings$rating, data.Ratings$movieID, function(x) round(min(x),2))
  
  # Détermination de la note médiane de chaque film
  stat.Movies$med = tapply(data.Ratings$rating, data.Ratings$movieID, function(x) round(median(x),2))
  
  return(stat.Movies)
  
}
