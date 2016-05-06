knn_user_predictions = function(list.Datasets, train, seq_K, mat.sim, predicteur, list.dejaVu, stat.Users, stat.Movies){
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
  
  # Nombre de prédictions
  nb.K = length(seq_K)

  ## TESTS
  nb.Tests = dim(test.Ratings)[1]
  resultTest = test.Ratings

  Kmax = max(seq_K)
    
  cat(paste0(nb.Tests))
  
  ## PREDICTION
  for(test in 1:nb.Tests){
    
    cat(paste0("|", test))
    
    userID = test.Ratings$userID[test]
    movieID = test.Ratings$movieID[test]

    knn = K_nearest_neighbors(userID, movieID, Kmax, list.dejaVu, vect.Users, mat.sim)

    vect.Neighbors = knn$neighbors
    vect.Similarity.byNN = knn$similarities
    vect.Ratings.byNN = as.vector(matrix(NA, nrow = 1, ncol = Kmax))

    estPresent = (movieID %in% stat.Movies$movieID & userID %in% stat.Users$userID)
    if(estPresent){
      for(k in 1:Kmax){ #Remplissage des notes des voisins
        if(!is.na(vect.Neighbors[k])){
          vect.Ratings.byNN[k] = train.Ratings$rating[(train.Ratings$userID == vect.Neighbors[k]) & (train.Ratings$movieID == movieID)]
        }
      }
    }
    
    #Prédiction
    if(estPresent){
      for(kIND in 1:nb.K){
        k = seq_K[kIND]
        resultTest[test,kIND+3] = knn_user_predicteur(vect.Similarity.byNN[1:k], vect.Ratings.byNN[1:k], stat.Users, userID, predicteur,vect.Neighbors[1:k])
      }
    }else{
      resultTest[test,3:3+nb.K] = NA
    }
  }
  
  return(resultTest)
  
}