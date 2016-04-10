svd=function(AvrRtg=Item,train.Ratings,data.Movies,data.Users,X){

# X est le pourcentage de l'inertie totale qu oi veut garder
# la paramÃ¨tre train.ratings reprÃ©sente une 5 bases tests et non pas la data frame train.Ratings en entier 
  
# AvrRtg=Item ou User
# We first removed sparsity by filling our customer-product ratingsmatrix.  
# We tried two different approaches: using the average ratings for a customer and 
# using the average ratings for a product. 
# We found the product average produce a better result.  
  
  
  # Dimension du problÃ¨me
  vect.Users = sort(unique(train.Ratings$userID))
  nb.Users = length(vect.Users) # nombre d'individus diffÃ©rents dans la data frame train.Ratings
  vect.Movies = sort(unique(train.Ratings$movieID))
  nb.Users = length(vect.Movies) # nombre de films diffÃ©rents dans la data frame train.Ratings
  
  
  #generation des statistiques sur les donnees de l'apprentissage
  stat.Users = stat_Users(train.Ratings)
  stat.Movies = stat_Movies(train.Ratings)
  # a optimiser seule une des deux entrÃ©es est necessaire en fonction de la valeur de AvrRtg
  
  # CrÃ©ation de la matrice R
  matR = matrix(NA, nrow =nb.Users , ncol = nb.Movies) 
  
  if(AvrRtg==User){
    for (userID1 in 1:nb.Users){
      mat.MoviesOfuserID1 = train.Ratings[train.Ratings$userID == userID1, c("movieID", "rating")]
      meanID1=as.numeric(stat.Users$mean[stat.Users$userID==userID1])
      for(movieID1 in 1 : nb.Movies){
        note=mat.MoviesOfuserID1$rating[mat.MoviesOfuserID1$movieID==movieID1]  
        # cette ligne peut sans doute etre amÃ©liorÃ© mais je n'ai rien trouvÃ© de mieu pour le moment
        if(isTRUE(note>0)){
          matR[userID1,movieID1]=note
        }else{
          matR[userID1,movieID1]=meanID1
        }
      }
    }
  }else{
    for (movieID1 in 1:nb.Movies){
      mat.UsersOfmovieID1 = train.Ratings[train.Ratings$movieID == movieID1, c("userID", "rating")]
      meanID1=as.numeric(stat.Movies$mean[stat.Movies$movieID==movieID1])
      for(userID1 in 1 : nb.Users){
        note=mat.UsersOfmovieID1$rating[mat.UsersOfmovieID1$userID==userID1]  
        # cette ligne peut sans doute etre amÃ©liorÃ© mais je n'ai rien trouvÃ© de mieu pour le moment
        if(isTRUE(note>0)){
          matR[userID1,movieID1]=note
        }else{
          matR[userID1,movieID1]=meanID1
        }
      }
    }
  }
  
  
  # We also considered twonormalization techniques: 
  # conversion of ratings to z-scores and 
  # subtraction of customer average from each rating.  => 
  ####### pq a la premiere Ã©tape remplir la matrice avec les moyennes quand la note n'est pas donnÃ©e si aprÃ¨s on doit l'enlever??? 
  # We found the latter approach to providebetter results.
  meanByUser=apply(matR,1,mean)
  matR2=matR-meanByUser
  
  #factor Rnorm using SVD to obtain U, S and V.
  SVD=svd(matR2)
  matR3=SVD$u%*%diag(SVD$d)%*%t(SVD$v)
  
  # reduce the matrix S - ici diag(SVD$d) - to dimension k ne gardant que X% de l'inertie totale
  tot=sum(SVD$d)
  k=1
  m=sum(SVD$d[1:k])
  while(m/tot<X){
    k=k+1
    m=sum(SVD$d[1:k])
  }
  Sk=diag(SVD$d[1:k])
  
  
  # il fait charger le package expm pour avoir acces a la fonction sqare root d une matrice
  # library("expm", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
  # compute the square-root of the reducedmatrix Sk, to obtain Sk^(1/2)
  install.packages("expm")
  # cocher expm dans Packages
  sqr_Sk=sqrtm(Sk)
  
  # compute two resultant matrices: UkSk^(1/2) and Sk^(1/2)Vk
  UkSk=1 # a changer !!
  SkVk=2 # a changer !!
  res=list(US=UkSk,SV=SkVk)
  return(res)
  
}
