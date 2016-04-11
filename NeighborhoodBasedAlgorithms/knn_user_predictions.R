knn_user_predictions = function(list.Datasets, train, Qmax, mat.sim, predicteur, list.dejaVu, stat.Users, stat.Movies){
  # INPUT
  # OUTPUT  : prédit les notes pour les couples (train, test)
  
  # Plus spécifiquement, à partir de list.Datasets et train, la fonction crée les bases d'apprentissages et de tests. Ensuite, elle 
  # recherche les plus proches voisins au sens de la similarité de mat.sim et donne sa prédiction pour chaque couple (movieID, userID)
  
  ## BASES D'APPRENTISSAGE ET DE TEST
  nb.Datasets = length(list.Datasets)
  dataset_to_keep = (1:nb.Datasets)[(1:nb.Datasets) != train]
  
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  test.Ratings = list.Datasets[[train]]
  
  # Ensemble des utilisateurs
  vect.Users = sort(unique(train.Ratings$userID))

  ## TESTS
  cat(sprintf("Début du test \n"))
  nb.Tests = dim(test.Ratings)[1]
  resultTest = test.Ratings
  
  cat(paste0(nb.Tests))
  
  ## PREDICTION
  for(test in 1:5000){ #TODO for(test in 1:nb.Tests){
    
    cat(paste0("|", test))
    
    userID = test.Ratings$userID[test]
    movieID = test.Ratings$movieID[test]
    
    qnn = Q_nearest_neighbors(userID, movieID, Qmax, list.dejaVu, vect.Users, mat.sim)
    vect.Ratings.byNN = matrix(NA, nrow = 1, ncol = Qmax)

    estPresent = movieID %in% stat.Movies$movieID
    if(estPresent){
      #nb.Ratings = stat.Movies$nb.Ratings[stat.Movies$movieID == movieID]
    
      for(q in 1:Qmax){
        vect.Ratings.byNN[q] = train.Ratings$rating[(train.Ratings$userID == qnn[q]) & (train.Ratings$movieID == movieID)]
        resultTest[test,q+3] = knn_user_predicteur(q, vect.Similarity.byNN, vect.Ratings.byNN, stat.Users, userID, predicteur,qnn)
      }
    }
    else{
        resultTest[test,] = NA
    }
    
  }
  
  return(resultTest)
  
}