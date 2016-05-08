svd_DG_recommendation = function(userID, recap.Users, recap.Movies, data.Ratings, nb.recommandations, nbMin.Ratings)
{

  # Indice de userID dans la dataframe data.Ratings 
  userIND = which(data.Ratings$userID == userID)      
  
  # Liste des films qu’userID a déjà vu :
  list.dejaVu= sort(data.Ratings$movieID[data.Ratings$userID ==userID])
  
  # Ensemble des films qui sont susceptibles d'être recommandés à userID
  ###### Filtre des films ayant dépassés un certain seuil
  vect.Recommandable = sort(recap.Movies$movieID[recap.Movies$nb.Ratings >= nbMin.Ratings])
  
  ###### Les films qu’userID n’a pas encore vu
  vect.Recommandable = vect.Recommandable[!(vect.Recommandable %in% list.dejaVu)]
  
  # Génération de la matrice Prediction : matrice contenant les prédictions pour tous les films susceptibles d'être recommandés et leur identifiants
  nb.RecommandableMovies= length(vect.Recommandable)
  Prediction = matrix(NA, nrow =nb.RecommandableMovies,ncol = 2)
    
  # Transformation d ela matrice PRediction en data frame
  Prediction=as.data.frame(Prediction)
  colnames(Prediction) = c("movieID", "prating")
  
  cat(nb.RecommandableMovies)  
  
  
  ######Prediction calculate###########
  vector.userID = sort(as.numeric(unique(data.Ratings$userID)))
  vector.movieID = sort(as.numeric(unique(data.Ratings$movieID)))
  
  iteration.times = 20
  lambda = 10
  
  matrix.training = transform.data.rating(data.Ratings, vector.userID, vector.movieID)
  #matrix.prevision = proximalG(matrix.training, iteration.times, 20)
  matrix.prevision = descentG(matrix.training, iteration.times, lambda)
  pred = restablish.data.rating.col(matrix.prevision, vector.userID, vector.movieID)
  
  Prediction = cbind(vect.Recommandable, pred[userID, vect.Recommandable])
  Prediction = Prediction[order(Prediction[,2], decreasing = TRUE),]
  
  if(length(vect.Recommandable)<nb.recommandations){
    nb.recommandations = vect.Recommandable
  }
  
  # Création de la data frame de Recommendation 
  Recommendation = matrix(NA, nrow =nb.recommandations,ncol = 2)
  Recommendation = as.data.frame(Recommendation)
  colnames(Recommendation) = c("movieID", "prating")
  
  
  Recommendation$prating=Prediction[1:nb.recommandations,2]
  Recommendation$movieID=Prediction[1:nb.recommandations,1]
  
  return(Recommendation)
}
