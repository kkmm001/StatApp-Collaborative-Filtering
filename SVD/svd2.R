svd2=function(AvrRtg="Item",train.Ratings){

# il fait charger le package expm pour avoir acces a la fonction sqare root d une matrice
# X est le pourcentage de l'inertie totale qu oi veut garder
# la paramètre train.ratings représente une des 5 bases tests 
# AvrRtg="Item" ou "User" onle met par défaut sur Item

  # Il faudra tester les deux pour vérifier si on trouve qu'en choisissant Item on obtient de meilleurs résulatats comme dans l'article
# We first removed sparsity by filling our customer-product ratingsmatrix.  
# We tried two different approaches: using the average ratings for a customer and 
# using the average ratings for a product. 
# We found the product average produce a better result.  
  
  
  # Dimension du problème
  vect.Users = sort(unique(train.Ratings$userID))
  nb.Users = length(vect.Users) # nombre d'individus différents dans la data frame train.Ratings
  vect.Movies = sort(unique(train.Ratings$movieID))
  nb.Movies = length(vect.Movies) # nombre de films différents dans la data frame train.Ratings
  
  
  #generation des statistiques sur les donnees de l'apprentissage
  stat.Users = stat_Users(train.Ratings)
  stat.Movies = stat_Movies(train.Ratings)

  # Crétion de la matrice R
  matR = matrix(NA, nrow =nb.Users , ncol = nb.Movies) 
  
  if(AvrRtg=="User"){
    for (userInd1 in 1:nb.Users){
      userID1=vect.Users[userInd1]
      mat.MoviesOfuserID1 = train.Ratings[train.Ratings$userID == userID1, c("movieID", "rating")]
      meanID1=as.numeric(stat.Users$mean[stat.Users$userID==userID1])
      for(movieInd1 in 1 : nb.Movies){
        movieID1=vect.Movies[movieInd1]
        note=mat.MoviesOfuserID1$rating[mat.MoviesOfuserID1$movieID==movieID1]  
        # cette ligne peut sans doute etre améliorée mais je n'ai rien trouvé de mieu pour le moment
        if(isTRUE(note>0)){ # Que se passe t'il si on enlève le isTrue?
          matR[userInd1,movieInd1]=note
        }else{
          matR[userInd1,movieInd1]=meanID1
        }
      }
    }
  }else{
    for (movieInd1 in 1:nb.Movies){
      movieID1=vect.Movies[movieInd1]
      mat.UsersOfmovieID1 = train.Ratings[train.Ratings$movieID == movieID1, c("userID", "rating")]
      meanID1=as.numeric(stat.Movies$mean[stat.Movies$movieID==movieID1])
      for(userInd1 in 1 : nb.Users){
        userID1=vect.Users[userInd1]
        note=mat.UsersOfmovieID1$rating[mat.UsersOfmovieID1$userID==userID1]  
        # cette ligne peut sans doute etre améliorée mais je n'ai rien trouvé de mieu pour le moment
        if(isTRUE(note>0)){
          matR[userInd1,movieInd1]=note
        }else{
          matR[userInd1,movieInd1]=meanID1
        }
      }
    }
  }
  
  
  # We also considered twonormalization techniques: 
  # conversion of ratings to z-scores and 
  # subtraction of customer average from each rating.  => 
  # We found the latter approach to providebetter results.
  meanByUser=as.numeric(c(stat.Users$mean[1:10]))
  matR=matR-meanByUser
  
  #factor Rnorm using SVD to obtain U, S and V.
  SVD=svd(matR)
  # matR3=SVD$u%*%diag(SVD$d)%*%t(SVD$v) verification : on retrouve bien matR
  return(SVD)
}
