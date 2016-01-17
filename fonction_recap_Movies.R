recap_Movies=function(TrainingU,data.Movies,data.Users)
{
  U=TrainingU
  nb.Movies = dim(data.Movies)[1]
  
  stat.RatingPerMovie= as.data.frame(matrix(0, nrow = nb.Movies, ncol = 7) )
  colnames(stat.RatingPerMovie) = c("movieID","nb.Ratings","mean","sd", "max","min","med")
  # matrice comprenant l'ID du film 
  #                    le nombre d'utilisateurs ayant notÃÂ© le film
  #                    la moyenne des notes
  #                    l'écart-type des notes
  #                    la note maximale 
  #                    la note minimale 
  #                    la mediane des notes
  
  
  ## Nombre d'hommes et de femmes ayant vu un film donné (ainsi que la moyenne des notes attribuées)
  
  stat.RatingsPerMoviePerSex =as.data.frame( matrix(0, nrow = nb.Movies, ncol = 5) )#matrice comprenant l'ID du film 
  colnames( stat.RatingsPerMoviePerSex) = c("movieID", "nb.Men","meanMen","nb.Women","meanWomen")
  # mean.women=matrix(0, nrow = nb.Movies, ncol = 2) 
  
  cond2=(data.Users$gender[U$userID]=="M")
  for (movie in 1:nb.Movies)
  { 
    cond1=U$movieID == movie
    x=U$rating[cond1]
    stat.RatingPerMovie[movie,] = c(movie,length(x),round(mean(x),2),round(sd(x),2),max(x),min(x),median(x))
    
    x=U$rating[cond1&cond2]
    y=U$rating[cond1&!(cond2)]
    stat.RatingsPerMoviePerSex[movie,] = c(movie,length(x),round(mean(x),2),length(y),round(mean(y),2))
    #mean.women[movie,]=c(movie,(mean(U$movieID == movie)-mean(x)*(length(x)/(length(x)+length(y))))*((length(x)+length(y))/length(y)))
    # on verifie que moyeWomen[i,2]=RatingPerMOviePerGender$avrRatingWomen[i] pour tout i
  }
  rm(x,y,movie,cond1,cond2)
  
  
  recap.Movies = cbind(stat.RatingPerMovie,stat.RatingsPerMoviePerSex) #il manque les data.Movies ? comme pour recap.Users ; recap = data + stat
  rm(stat.RatingPerMovie,stat.RatingsPerMoviePerSex)
  return(recap.Movies)
}

