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
  nb.Tests = 2000 #dim(test.Ratings)[1]
  resultTest = test.Ratings
  
  cat(paste0(nb.Tests))
  
  ## PREDICTION
  for(test in 1:nb.Tests){
    
    cat(paste0("|", test))
    
    userID = test.Ratings$userID[test]
    movieID = test.Ratings$movieID[test]
    
    qnn = Q_nearest_neighbors(userID, movieID, Qmax, list.dejaVu, vect.Users, mat.sim)

    vect.Neighbors = qnn$neighbors
    vect.Similarity.byNN = qnn$similarities
    vect.Ratings.byNN = as.vector(matrix(NA, nrow = 1, ncol = Qmax))

    estPresent = movieID %in% stat.Movies$movieID
    if(estPresent){
      for(q in 1:Qmax){
        if(!is.na(vect.Neighbors[q])){
          vect.Ratings.byNN[q] = train.Ratings$rating[(train.Ratings$userID == vect.Neighbors[q]) & (train.Ratings$movieID == movieID)]
        }
          resultTest[test,q+3] = knn_user_predicteur(vect.Similarity.byNN[1:q], vect.Ratings.byNN[1:q], stat.Users, userID, predicteur,vect.Neighbors[1:q])
      }
    } 
    
    else{
        resultTest[test,3:3+Qmax] = NA
    }
    
  }
  
  return(resultTest)
  
}