svd_predictions = function(list.Datasets, test, tau, list.SVD, stat.Users){
  # INPUT   list.Datasets : la liste des datasets pour la partie validation-croisée
  #         test         : le numéro du test
  #         tau           : le taux d'inertie conservé
  # OUTPUT                : prédit les notes pour les couples (train, test)
  
  ## Bases d'apprentissage et de test
  nb.Tests = length(list.Datasets)

  dataset_to_keep = (1:nb.Tests)[(1:nb.Tests) != test]
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  test.Ratings=list.Datasets[[test]]
  
  # Eléments présents dans la base d'apprentissage
  vect.MoviesInTrain = sort(unique(train.Ratings$movieID))
  vect.UsersInTrain = sort(unique(train.Ratings$userID))
  
  # Matrices US et SV créés pour un taux d'inertie tau
  matUS_matSV = matUS_matSV(list.SVD, tau) 
  mat.US = matUS_matSV$US
  mat.SV = matUS_matSV$SV
  
  # Dimension de la matrice US (rang du problème)
  k = dim(mat.SV)[1]
  
  # Prédictions
  nb.Ratings = length(test.Ratings$rating)

  for(i in 1:nb.Ratings){

    userID = test.Ratings$userID[i]
    movieID = test.Ratings$movieID[i]
    
    movieIND = which(vect.MoviesInTrain == movieID)    
    userIND = which(vect.UsersInTrain == userID)

    if(movieID %in% vect.MoviesInTrain & userID %in% vect.UsersInTrain){
      if(k!=1){
        test.Ratings$prating[i]=get_limited_value(as.numeric(stat.Users$mean[stat.Users$userID==userID]+mat.US[userIND,]%*%mat.SV[,movieIND]))
      }else{
        test.Ratings$prating[i] = get_limited_value(stat.Users$mean[stat.Users$userID==userID] + mat.US[userIND] * mat.SV[movieIND])
      }
    }else{
      test.Ratings$prating[i] = NA
    }
  }

  return(test.Ratings)
}
