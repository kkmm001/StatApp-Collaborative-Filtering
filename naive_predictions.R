# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =#
#       Fichier : naive_predictions.R                                                               #
#       Description : Fonction de predictions naives sur les bases                                   #
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =#

naive_predictions = function(train.Ratings, test.Ratings){
  
  #generation des statistiques sur les donnees de l'apprentissage
  stat.Users = stat_Users(train.Ratings)
  stat.Movies = stat_Movies(train.Ratings)
  
  nb.Tests = dim(test.Ratings)[1]
  resultTest = test.Ratings
    
  #prediction aleatoire sur [1,5]
  resultTest$random_unif = round(runif(nb.Tests,1,5),2)
  
  #prediction sur la distribution des notes
  resultTest$random_samp = sample(x=train.Ratings$rating,replace=T,size=nb.Tests)
    
  #prediction par la note moyenne des utilisateurs
  resultTest$meanOfUsers = round(mean(stat.Users$mean,na.rm=T), 2)
    
  #prediction par la note moyenne des films
  resultTest$meanOfMovies = round(mean(stat.Movies$mean,na.rm=T),2)
  
  #prediction par la note moyenne globale
  resultTest$mean = round(mean(train.Ratings$rating),2)
    
  #prediction par la moyenne par utilisateur
  for (rowIndex in 1:nb.Tests) { 
    flag = (stat.Users$userID == resultTest$userID[rowIndex])
    if (sum(flag)){
      resultTest$meanByUser[rowIndex]=stat.Users$mean[flag]
    }
  }
  
  #prediction par la moyenne par film
  for (rowIndex in 1:nb.Tests) { 
    flag = (stat.Movies$movieID == resultTest$movieID[rowIndex])
    if (sum(flag)){
      resultTest$meanByMovie[rowIndex]=stat.Movies$mean[flag]
    }
  }
    
  return(resultTest)
  
}
