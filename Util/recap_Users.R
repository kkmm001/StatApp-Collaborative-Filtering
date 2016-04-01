recap_Users = function(data.Users, data.Ratings){
  # INPUT data.Users    : la base des utilisateurs
  #       data.Ratings  : la base des notes
  # OUTPUT              : la base contenant les informations des utilisateurs (contenues dans data.Users)
  #                       et les statistiques sur les notes attribuées (contenues dans data.Ratings)
  
  # Les statistiques pour un utilisateur donné sont : 
  # - nb.Ratings  : le nombre de notes attribuées ; 
  # - mean        : la moyenne de ses notes ; 
  # - sd          : l'écart-type de ses notes ; 
  # - max         : la note maximale attribuée ; 
  # - min         : la note minimale attribuée ; 
  # - med         : la note médiane
  
  # Détermination du nombre de notes attribuées
  data.Users$nb.Ratings = tapply(data.Ratings$rating, data.Ratings$userID, length)
  
  # Détermination de la moyenne de chaque utilisateur
  data.Users$mean = tapply(data.Ratings$rating, data.Ratings$userID, function(x) round(mean(x),2))
  
  # Détermination de l'écart-type de chaque utilisateur
  data.Users$sd = tapply(data.Ratings$rating, data.Ratings$userID, function(x) round(sd(x),2))
  
  # Détermination de la note maximale de chaque utilisateur
  data.Users$max = tapply(data.Ratings$rating, data.Ratings$userID, function(x) round(max(x),2))
  
  # Détermination de la note minimale de chaque utilisateur
  data.Users$min = tapply(data.Ratings$rating, data.Ratings$userID, function(x) round(min(x),2))
  
  # Détermination de la note médiane de chaque utilisateur
  data.Users$med = tapply(data.Ratings$rating, data.Ratings$userID, function(x) round(median(x),2))
  
  return(data.Users)
  
}












stat_Users=function(data.Ratings){ 
  
  vect.Users = unique(data.Ratings$userID)
  nb.Users = length(vect.Users)
  
  stat.RatingPerUser = as.data.frame(matrix(0, nrow = nb.Users, ncol = 7))
  colnames(stat.RatingPerUser) = c("userID", "nb.Ratings","mean", "sd", "max", "min", "med")
  # matrice comprenant l'ID de l'utilisateur 
  #                    le nombre de films notes par utilisateur
  #                    la moyenne des notes
  #                    l'ecart-type des notes
  #                    la note maximale 
  #                    la note minimale
  #                    la mede des notes
  
  for (user_fakeID in 1:nb.Users){ 
    user = vect.Users[user_fakeID]
    x=data.Ratings$rating[data.Ratings$userID == user]
    stat.RatingPerUser[user_fakeID,] = c(user,length(x),round(mean(x),2),round(sd(x),2),max(x),min(x),median(x))
  }
  
  return(stat.RatingPerUser)
  
}
