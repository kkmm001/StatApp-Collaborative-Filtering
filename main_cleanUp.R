# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquÃ©e
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_cleanUp.R
#       Description : nettoyage des bases de données

#Code partiellement testé
#Ne pas implémenter => cela entrainera des problèmes au niveau des autres codes (en raison des indices de films non-existants)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 

# ======================================== 2.OUVERTURE DES FICHIERS =================================

source("open_file.R")
#open_file(repository = ml-100k)

# ======================================== 2.SUPPRESSION DES FILMS TEST  =================================

delete_movie = function(movieID,data.Movies,data.Ratings){
  
  newdata.Movies = data.Movies[data.Movies$movieID != movieID,]
  newdata.Ratings = data.Ratings[data.Movies$movieID != movieID,]
  
  write.csv2(newdata.Movies, "./movies_v2.csv",row.names = FALSE)
  write.csv2(newdata.Ratings, "./ratings_v2.csv",row.names = FALSE)
  
}

delete_movie(267,data.Movies, data.Ratings) # Il faudra un code pour détecter les films test

# ======================================== 2.SUPPRESSION DES FILMS EN DOUBLON  =================================

merge_movies = function(data.Movies, data.Ratings){
  
  #base contenant les films en doublons => basé sur leur title
  data.SameMovies = data.Movies[duplicated(data.Movies$title),c("movieID", "title", "date")]
  
  doublons = matrix(NA,nrow = dim(data.SameMovies)[1], ncol = 1)
  
  # doublons : enregistre les films en doublons sous la forme ('vrai' ID du film, IDs en doublons du film)
  for(ind in 1:dim(data.SameMovies)[1]){
    title = data.SameMovies$title[ind]
    doublons[ind] = list(data.Movies$movieID[title == data.Movies$title])
  }
  
  newdata.Movies = data.Movies
  newdata.Ratings = data.Ratings
  
  #suppression des films en double de la base data.Movies
  #remplacement des IDs en double par le 'vrai' ID du film
  for(doublon in 1:length(doublons)){
    realID = doublons[[doublon]][1]
    for(ind_fake in 2:length(doublons[[doublon]])){
      fakeID = doublons[[doublon]][ind_fake]
      newdata.Ratings$movieID[newdata.Ratings$movieID == fakeID] <- realID
      newdata.Movies = newdata.Movies[newdata.Movies$movieID != fakeID,]
    }
  }
  
  #enregistrement
  write.csv2(newdata.Ratings, "./ratings_v2.csv",row.names = FALSE)
  write.csv2(newdata.Movies, "./movies_v2.csv",row.names = FALSE)
  
}
