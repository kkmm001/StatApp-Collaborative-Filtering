# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquee
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_clean_ml-100k.R
#       Description : nettoyage et renommage des données du problème ml-100k
#
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

# Clean up
rm(list=ls()) 
cat("\014") 

# ================================ 2.OUVERTURE DES FICHIERS =======================================

#Ouverture du fichier des notes
file.Ratings = file.choose() # u.data
data.Ratings = read.table(file=file.Ratings,header=F,colClasses = c(V4 = "NULL"))
colnames(data.Ratings) = c("userID", "movieID", "rating")

#Ouverture du  fichier des films
file.Movies = file.choose() # u.item
data.Movies = read.table(file=file.Movies,header=F,sep="|", quote = "\"",colClasses = c(V4 = "NULL"))

vect.MovieGenres = c("unknown", "Action", "Adventure", "Animation", "Children's", "Comedy", 
                     "Crime", "Documentary", "Drama", "Fantasy", "Film-noir", "Horror", 
                     "Musical", "Mystery", "Romance", "Sci-fi", "Thriller", "War", "Western")

nb.Genres = length(vect.MovieGenres)
colnames(data.Movies) = c("movieID", "title", "date", "IMDbURL", vect.MovieGenres)

#Ouverture du fichiers des utilisateurs
file.Users = file.choose() # u.user
data.Users = read.table(file=file.Users,header=F, sep='|', stringsAsFactors = TRUE)
colnames(data.Users) = c("userID", "age", "sex", "occupation", "zip.code")

# ====================== 3. FONCTION POUR LE NETTOYAGE DES DOUBLONS ============================

merge_movies = function(data.Ratings,data.Movies,data.Users){
  
  data.SameMovies = data.Movies[duplicated(data.Movies$title),c("movieID", "title", "date")]
  doublons = matrix(NA,nrow = dim(data.SameMovies)[1], ncol = 1)
  for(ind in 1:dim(data.SameMovies)[1]){
    title = data.SameMovies$title[ind]
    doublons[ind] = list(data.Movies$movieID[data.Movies$title == title])
  }
  
  for(doublon in 1:length(doublons)){
    realID = doublons[[doublon]][1]
    for(ind_other in 2:length(doublons[[doublon]])){
      otherID = doublons[[doublon]][ind_other]
      data.Ratings$movieID[data.Ratings$movieID == otherID] = realID
      data.Movies = data.Movies[data.Movies$movieID != otherID,]
    }
  }
  
  print(doublons)
  
  #Enregistrement
  write.csv2(data.Ratings, "./Data/ml-100k/data.Ratings.csv",row.names = FALSE)
  write.csv2(data.Movies, "./Data/ml-100k/data.Movies.csv",row.names = FALSE)
  write.csv2(data.Users, "./Data/ml-100k/data.Users.csv",row.names = FALSE)
  
}

# ======================= 4.FONCTION PRINCIPAL ===================================================

#suppression du film test (ID 267)
data.Movies = data.Movies[data.Movies$movieID != 267,]
data.Ratings = data.Ratings[data.Ratings$movieID != 267,]

#suppression des films en doublons et remplacement de leur id respectif
# 246 268 | 297 303 | 329 348 | 304 500 | 573 670 | 266 680 | 305 865 | 876 881 | 878 1003 |
# 1256 1257 | 309 1606 | 1395 1607 | 1175 1617 | 1477 1625 | 1645 1650 | 1234 1654 | 711 1658 | 1429 1680 |
merge_movies(data.Ratings, data.Movies, data.Users)
