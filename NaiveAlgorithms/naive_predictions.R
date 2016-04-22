naive_predictions = function(list.Datasets, train){
  # INPUT   list.Datasets : liste des sous-datasets de data.Ratings
  #         train         : numéro de l'apprentissage
  # OUTPUT                : data frame contenant les prédictions 
  
  ### PREAMBULE
  
  # Bases d'apprentissage et de test
  nb.Datasets = length(list.Datasets)
  dataset_to_keep = (1:nb.Datasets)[(1:nb.Datasets) != train]
  
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  test.Ratings = list.Datasets[[train]]
  
  # Génération des statistiques sur les données de l'apprentissage
  stat.Users = stat_Users(train.Ratings)
  stat.Movies = stat_Movies(train.Ratings)
  
  # Dimension de la base de tests
  nb.Tests = dim(test.Ratings)[1]

  # Eléments présents dans la base d'apprentissage
  vect.MoviesInTrain = sort(unique(stat.Movies$movieID))
  vect.UsersInTrain = sort(unique(stat.Users$userID))

  ### PREDICTION
  
  #prédiction aleatoire sur [1,5]
  test.Ratings$random_unif = round(runif(nb.Tests,1,5), 2)
  
  #prédiction sur la distribution des notes
  test.Ratings$random_samp = sample(train.Ratings$rating, replace = TRUE, size = nb.Tests)
    
  #prédiction par la note moyenne des utilisateurs
  test.Ratings$meanOfUsers = round(mean(stat.Users$mean), 2)
    
  #prédiction par la note moyenne des films
  test.Ratings$meanOfMovies = round(mean(stat.Movies$mean), 2)
  
  #prédiction par la note moyenne globale
  test.Ratings$mean = round(mean(train.Ratings$rating), 2)
    
  #prédiction par la moyenne par utilisateur
  meanByUser = function(userID, stat.Users){
    if(userID %in% vect.UsersInTrain){
      return(stat.Users$mean[stat.Users$userID == userID])
    }
    else{
      return(NA)
    }
  }
  
  test.Ratings$meanByUser = sapply(test.Ratings$userID, meanByUser, stat.Users)
  
  #prédiction par la moyenne par film
  meanByMovie = function(movieID, stat.Movies){
    if(movieID %in% vect.MoviesInTrain){
      return(stat.Movies$mean[stat.Movies$movieID == movieID])
    }
    else{
      return(NA)
    }
  }
  
  test.Ratings$meanByMovie = sapply(test.Ratings$movieID, meanByMovie, stat.Movies)
  
  return(test.Ratings)
  
}
