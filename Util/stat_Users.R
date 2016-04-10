stat_Users = function(data.Ratings){
  # INPUT data.Ratings  : la base des notes
  # OUTPUT              : la base contenant les statistiques sur les notes attribuées
  
  # Les statistiques pour un utilisateur donné sont : 
  # - nb.Ratings  : le nombre de notes attribuées ; 
  # - mean        : la moyenne de ses notes ; 
  # - sd          : l'écart-type de ses notes ; 
  # - max         : la note maximale attribuée ; 
  # - min         : la note minimale attribuée ; 
  # - med         : la note médiane
  
  userID = sort(unique(data.Ratings$userID))
  stat.Users = as.data.frame(userID)
  
  # Détermination du nombre de notes attribuées
  stat.Users$nb.Ratings = tapply(data.Ratings$rating, data.Ratings$userID, length)
  
  # Détermination de la moyenne de chaque utilisateur
  stat.Users$mean = tapply(data.Ratings$rating, data.Ratings$userID, function(x) round(mean(x),2))
  
  # Détermination de l'écart-type de chaque utilisateur
  stat.Users$sd = tapply(data.Ratings$rating, data.Ratings$userID, function(x) round(sd(x),2))
  
  # Détermination de la note maximale de chaque utilisateur
  stat.Users$max = tapply(data.Ratings$rating, data.Ratings$userID, function(x) round(max(x),2))
  
  # Détermination de la note minimale de chaque utilisateur
  stat.Users$min = tapply(data.Ratings$rating, data.Ratings$userID, function(x) round(min(x),2))
  
  # Détermination de la note médiane de chaque utilisateur
  stat.Users$med = tapply(data.Ratings$rating, data.Ratings$userID, function(x) round(median(x),2))
  
  return(stat.Users)
  
}