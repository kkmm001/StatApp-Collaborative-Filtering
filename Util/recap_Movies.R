recap_Movies = function(data.Movies, data.Ratings){
  # INPUT data.Movies   : la base des films
  #       data.Ratings  : la base des notes
  # OUTPUT              : la base contenant les informations des films (contenues dans data.Movies)
  #                       et les statistiques sur les notes des films (contenues dans data.Ratings)
  
  # Les statistiques pour un film donné sont : 
  # - nb.Ratings  : le nombre de notes reçues ; 
  # - mean        : la moyenne de ses notes ; 
  # - sd          : l'écart-type de ses notes ; 
  # - max         : la note maximale reçue ; 
  # - min         : la note minimale reçue ; 
  # - med         : la note médiane
  
  # Détermination du nombre de notes reçues
  data.Movies$nb.Ratings = tapply(data.Ratings$rating, data.Ratings$movieID, length)

  # Détermination de la moyenne de chaque film
  data.Movies$mean = tapply(data.Ratings$rating, data.Ratings$movieID, function(x) round(mean(x),2))
  
  # Détermination de l'écart-type de chaque film
  data.Movies$sd = tapply(data.Ratings$rating, data.Ratings$movieID, function(x) round(sd(x),2))
  
  # Détermination de la note maximale de chaque film
  data.Movies$max = tapply(data.Ratings$rating, data.Ratings$movieID, function(x) round(max(x),2))
  
  # Détermination de la note minimale de chaque film
  data.Movies$min = tapply(data.Ratings$rating, data.Ratings$movieID, function(x) round(min(x),2))
  
  # Détermination de la note médiane de chaque film
  data.Movies$med = tapply(data.Ratings$rating, data.Ratings$movieID, function(x) round(median(x),2))
  
  return(data.Movies)
  
}
