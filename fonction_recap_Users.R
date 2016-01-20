recap_Users=function(TrainingU,data.Movies,data.Users)
{ # ===================================================
  # ATTENTION
  # data.Movies n'est pas utilisé dans cette fonction !
  
  U=TrainingU
  nb.Users = dim(data.Users)[1]
  
  stat.RatingsPerUser = matrix(0, nrow = nb.Users, ncol = 7) 
  stat.RatingsPerUser= as.data.frame(stat.RatingsPerUser)
  colnames(stat.RatingsPerUser) = c("userID", "nb.Ratings","mean", "sd", "max", "min", "med")
  
  # matrice comprenant l'ID de l'utilisateur 
  #                    le nombre de films notés par utilisateur
  #                    la moyenne des notes
  #                    l'écart-type des notes
  #                    la note maximale 
  #                    la note maximale 
  #                    la mede des notes
  
  for (user in 1:nb.Users) 
  { 
    x=U$rating[U$userID == user]
    stat.RatingsPerUser[user,] = c(user,length(x),round(mean(x),2),round(sd(x),2),max(x),min(x),median(x))
  }
  rm(x,user,nb.Users,U)
                              
  recap.Users = cbind( data.Users , stat.RatingsPerUser$nb.Ratings , stat.RatingsPerUser$mean ,stat.RatingsPerUser$sd , stat.RatingsPerUser$max , stat.RatingsPerUser$min , stat.RatingsPerUser$med )
  colnames(recap.Users) = c("userID", "age", "sexe", "occupation", "zip.code","nb.Ratings","mean", "sd", "max", "min", "med")
  recap.Users = recap.Users[ ,-5]
  rm(stat.RatingsPerUser) 
  return(recap.Users)
}
