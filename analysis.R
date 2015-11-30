# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah, Ulrich Mpeli Mpeli
#
#       Fichier : descriptions.R
#       Description : analyses descriptives sur la base des notes, des utilisateurs et des films
#
#       Auteur : 
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 

## Set working
getwd() 
#setwd("C:/Users/Maestro/Desktop/2A/Stat_App/Donnees") # Changer le repertoire de travail #

# ================================== 2.OUVERTURE DES FICHIERS =======================================

## Lecture du fichier des notes

Data.Ratings = read.table(file=file.choose(),header=F,colClasses = c(V4 = "NULL"))
colnames(Data.Ratings) = c("userID", "movieID", "rating")

## Lecture du fichier des utilisateurs

Data.Users = read.table(file=file.choose(),header=F, sep='|', stringsAsFactors = TRUE)
colnames(Data.Users) = c("userID", "age", "gender", "occupation", "zip.code")

## Lecture du fichier des films

Data.Movies = read.table(file=file.choose(),header=F,sep="|", quote = "\"",colClasses = c(V4 = "NULL"))
Movie.Genres = c("unknown", "action", "adventure", "animation", "children's", "comedy", 
                      "crime", "documentary", "drama", "fantasy", "film-noir", "horror", 
                      "musical", "mystery", "romance", "sci-fi", "thriller", "war", "western")
#MG=read.table(file=file.choose(),header=F,sep="|", quote = "\"") ##choisir u.genre
#MG=MG$V1
#nbG=length(MG)

nb.Genres = length(Movie.Genres)
colnames(Data.Movies) = c("movieID", "title", "date", "IMDbURL", Movie.Genres)
#colnames(Data.Movies)=c("movieID", "title", "date", "IMDbURL", MG) à modifier de tel sorte que MG soit une string


# ======================== 3.DETERMINATION DU VOLUME DE DONNEES DES BASES (contrôles) ============================

#3.1# A PARTIR DE LA BASE DES NOTES

## Nombre de notes

nb.Ratings = dim(Data.Ratings)[1]
cat("La base de données contient exactement", nb.Ratings, "notes.")

## Nombre d'utilisateurs uniques

nb.Users = length(unique(Data.Ratings$userID))
cat("La base de données contient exactement", nb.Users, "utilisateurs uniques.")

## Nombre de films uniques

nb.Movies = length(unique(Data.Ratings$movieID))
cat("La base de données contient exactement", nb.Movies, "films uniques.")

#3.2# VERIFICATION AVEC LA BASE DES UTILISATEURS

## Nombre d'utilisateurs

if(nb.Users == dim(Data.Users)[1]){
  cat("La base de données contient bien", nb.Users, "utilisateurs.")
} else{
  cat("Attention : il y a un conflit sur le nombre d'utilisateurs.")
  break
}

#3.3# VERIFICATION AVEC LA BASE DES FILMS

## Nombre de films

if (nb.Movies == dim(Data.Movies)[1]){
  cat("La base de données contient bien", nb.Movies, "films.")
} else{
  cat("Attention : il y a un conflit sur le nombre de films.")
  break
}

# ====================================== 4.STATISTIQUE D'ORDRE 1 =========================================

#4.1# SUR LES NOTES

## Statistiques des notes

avgRatings = summary(Data.Ratings$rating)
cat("La moyenne des", nb.Ratings, "notes est de", avgRatings[4], ";
    l'écart-type est", round(ave(Data.Ratings$rating, FUN=sd)[1],2))
summary(Data.Ratings$rating)
boxplot(Data.Ratings$rating, main = "Notes discrètes attribuées aux films")

#4.2# SUR LES UTILISATEURS

## Age

avgAge = summary(Data.Users$age)
avgAge
cat("La moyenne des âges est", avgAge[4], "ans avec un écart-type de",round(ave(Data.Users$age, FUN=sd)[1],2), "ans")

## Proportion homme/femme

nb.Men = sum(Data.Users$gender == "M")
nb.Women = sum(Data.Users$gender == "F")
cat("La base de données est composée de", nb.Men, "hommes, soit", round(100*nb.Men/nb.Users,2), 
    "% et", nb.Women, "femmes, soit" ,round(100*nb.Women/nb.Users,2),"% .")

## Proportions dans les métiers

as.data.frame(summary(Data.Users$occupation))

## Proportions dans les codes postales

as.data.frame(summary(Data.Users$zip.code))

cat("le code postal n'est pas du tout pertinent (699 other)")

#4.3# SUR LES FILMS

## Répartition par genres de films

nb.MoviePerGenre = matrix(0, nrow = nb.Genres, ncol = 1)
rownames(nb.MoviePerGenre) = Movie.Genres
for (genre in 1:nb.Genres){
  nb.MoviePerGenre[genre] = sum(Data.Movies[Movie.Genres[genre]])
}
nb.MoviePerGenre
boxplot(nb.MoviePerGenre, main = "nombre de films par genre")

# ==================================== 5.STATISTIQUE D'ORDRE 2 =========================================

#5.1# LIEN ENTRE NOMBRE DE FILMS ET UTILISATEURS

## Répartition du nombre de films notés par utilisateur

nb.RatingsPerUser = matrix(0, nrow = nb.Users, ncol = 2) #matrice comprenant l'ID de l'utilisateut et son nombre de films notés
for (user in 1:nb.Users) { 
  nb.RatingsPerUser[user,] = c(user,sum(Data.Ratings$userID == user))
}
nb.RatingsPerUser = as.data.frame(nb.RatingsPerUser)
colnames(nb.RatingsPerUser) = c("userID", "nb.Ratings")

## Statistiques sur le nombre de films notés par utilisateur

summary(nb.RatingsPerUser$nb.Ratings)
boxplot(nb.RatingsPerUser$nb.Ratings, main = "Nombre de films notés par utilisateur")

#5.2# LIEN ENTRE MOYENNE DES NOTES ET UTILISATEUR

## Note moyenne par utilisateur / Ecart-type des moyennes par utilisateur
## Note maximale par utilisateur / Note minimale par utilisateur / Note médiane par utilisateur
####### POUR NE PAS FAIRE TOURNER LA MEME BOUCLE DEUX FOIS


RatingPerUser = matrix(0, nrow = nb.Users, ncol = 6) # matrice comprenant l'ID de l'utilisateur 
                                                     #                    la moyenne des notes
                                                     #                    l'écart-type des notes
                                                     #                    la note maximale 
                                                     #                    la note maximale 
                                                     #                    la mediane des notes

for (user in 1:nb.Users) { 
  x=Data.Ratings$rating[Data.Ratings$userID == user]
  RatingPerUser[user,] = c(user,round(mean(x),2),round(sd(x),2),max(x),min(x),median(x))
}
rm(x)

RatingPerUser = as.data.frame(RatingPerUser)
colnames(RatingPerUser) = c("userID","avgRating","SdRating", "MaxRating","MinRating","MedRating")


## Statistiques sur les notes moyennes données par utilisateur

summary(RatingPerUser$avgRating)
boxplot(RatingPerUser$avgRating, main = "Note moyenne donnée par utilisateur")

## Tableau récapitulatif

RecapUsers = cbind(nb.RatingsPerUser,Data.Users$age,Data.Users$gender,Data.Users$occupation,RatingPerUser$avgRating,RatingPerUser$SdRating,
                   RatingPerUser$MinRating,RatingPerUser$MedRating,RatingPerUser$MaxRating)
colnames(RecapUsers) = c("userID","nb.movie","age","gender","occupation","avg", "sd", "min", "median", "max")
rm(RatingPerUser,nb.RatingsPerUser)




#5.3# LIEN ENTRE NOMBRE DE NOTES ET FILM

## Répartition du nombre de notes par film

nb.RatingsPerMovie =  matrix(0, nrow = nb.Movies, ncol = 2) #matrice comprenant l'ID du film et som nombre de notes reçues
for (movie in 1:nb.Movies) { 
  nb.RatingsPerMovie[movie,] = c(movie,sum(Data.Ratings$movieID == movie))
}
nb.RatingsPerMovie = as.data.frame(nb.RatingsPerMovie)
colnames(nb.RatingsPerMovie) = c("movieID", "nb.Ratings")


## Statistiques sur le nombre de notes par films

summary(nb.RatingsPerMovie$nb.Ratings)
boxplot(nb.RatingsPerMovie$nb.Ratings, main = "Nombre de notes par film")

#5.4# LIEN ENTRE MOYENNE DES NOTES ET FILM

## Note moyenne par film / Ecart-type des moyennes par film
## Note maximale par film / Note minimale par film / Note médiane par film
####### POUR NE PAS FAIRE TOURNER LA MEME BOUCLE DEUX FOIS


RatingPerMovie = matrix(0, nrow = nb.Movies, ncol = 6) # matrice comprenant l'ID du film 
#                    la moyenne des notes
#                    l'écart-type des notes
#                    la note maximale 
#                    la note maximale 
#                    la mediane des notes

for (movie in 1:nb.Movies) { 
  x=Data.Ratings$rating[Data.Ratings$movieID == movie]
  RatingPerMovie[movie,] = c(movie,round(mean(x),2),round(sd(x),2),max(x),min(x),median(x))
}
rm(x)

RatingPerMovie = as.data.frame(RatingPerMovie)
colnames(RatingPerMovie) = c("movieID","avgRating","SdRating", "MaxRating","MinRating","MedRating")


## Statistiques sur les notes moyennes reçues par film

summary(RatingPerMovie$avgRating)
boxplot(RatingPerMovie$avgRating, main = "Note moyenne des films")





## Nombre d'hommes ayant vu un film donné (ainsi que la moyenne des notes attribuées)


######################### METHODE 1 ###########################
# Recherche du sexe 

data = Data.Ratings
v = rep(0,dim(data)[1])
data = cbind(data,v)
data
str(data)
for (i in 1:dim(data)[1]){
  for (j in 1: dim(Data.Users)[1]){
    if (data$userID[i] == Data.Users$userID[j]){
      data$v[i] = Data.Users$gender[j]
    }
  }
}

##### Repartition par sexe

nb.MenPerMovie = rep(0,nb.Movies)
moy.Men = rep(0,nb.Movies)
nb.WomPerMovie = rep(0,nb.Movies)
moy.Wom = rep(0,nb.Movies)
movieID = 1:nb.Movies
MenPerMovie = cbind(movieID,nb.MenPerMovie,moy.Men,nb.WomPerMovie,moy.Wom)
MenPerMovie = as.data.frame(MenPerMovie)


for (i in 1:nb.Movies)
{
  m = 0
  w = 0
  notem = 0
  notew = 0
  for (j in 1:nb.Ratings)
  {
    if (data$movieID[j] == MenPerMovie$movieID[i])
    {
      if (data$v[j] == 2)
      {  m = m+1
      notem = notem + data$rating[j]
      }
      else
      { w=w+1
      notew = notew + data$rating[j]  
      }
    }
  }
  MenPerMovie$nb.MenPerMovie[i] = m 
  MenPerMovie$moy.Men[i] = notem/m # Attention traiter à part le cas où k vaut 0
  MenPerMovie$nb.WomPerMovie[i] = w
  MenPerMovie$moy.Wom[i] = notew/w
}

######################### METHODE  2 ###################################### 

RatingPerMoviePerGender = matrix(0, nrow = nb.Movies, ncol = 5) #matrice comprenant l'ID du film 
MoyWomen=matrix(0, nrow = nb.Movies, ncol = 2) 
for (movie in 1:nb.Movies) { 
  cond1=Data.Ratings$movieID == movie
  cond2=(Data.Users$gender[Data.Ratings$userID]=="M")
  x=Data.Ratings$rating[cond1&cond2]
  y=Data.Ratings$rating[cond1&!(cond2)]
  RatingPerMoviePerGender[movie,] = c(movie,length(x),round(mean(x),2),length(y),round(mean(y),2))
  MoyWomen[movie,]=c(movie,(mean(Data.Ratings$movieID == movie)-mean(x)*(length(x)/(length(x)+length(y))))*((length(x)+length(y))/length(y)))
}

RatingPerMoviePerGender = as.data.frame(RatingPerMoviePerGender)
colnames(RatingPerMoviePerGender ) = c("movieID", "nbMen","avgRatingMen","nbWomen","avgRatingWomen")

# on verifie que moyeWomen[i,2]=RatingPerMOviePerGender$avrRatingWomen[i] pour tout i

## Tableau récapitulatif

RecapMovie = cbind(nb.RatingsPerMovie,RatingPerMovie$avgRating,RatingPerMovie$SdRating,
                   RatingPerMovie$MinRating,RatingPerMovie$MedRating,RatingPerMovie$MaxRating,
                   MenPerMovie$nb.MenPerMovie,MenPerMovie$moy.Men,MenPerMovie$nb.WomPerMovie,MenPerMovie$moy.Wom)
colnames(RecapMovie) = c("movieID","nb.user","avg", "sd", "min", "median", "max","nbMen","avgMen","nbWom","avgWom")
rm(RatingPerMovie,nb.RatingsPerMovie,MenPerMovie)

# ====================== 6.CORRELATION ENTRE LES FILMS ET LES UTILISATEURS ===============================

## Corrélation entre note des films et nombre de visionnage de ce film (normalement oui, et positive)
stat.Movie <- merge(nb.RatingsPerMovie, AvgRatingPerMovie, by.x = "movieID", by.y = "movieID")
plot(stat.Movie$nb.Ratings, stat.Movie$avgRating, 
     main = "Lien entre nombre de visionnage et note moyenne par film", 
     xlab = "nombre de visionnage", 
     ylab = "note moyenne")

## Corrélation entre note attribuée et nombre de films vus (pas nécessairement sauf les rageux et les floodeurs)
stat.User <- merge(nb.RatingsPerUser,AvgRatingPerUser, by.x = "userID", bu.y = "userID")
plot(stat.User$nb.Ratings, stat.User$avgRating, 
     main = "Lien entre nombre de films notés et note moyenne par utilisateur", 
     xlab = "nombre de films notés", 
     ylab = "note moyenne")

# ===================================== 7.STATISTIQUE D'ORDRE ===========================================

#7.1# SUR LES FILMS (PAR RAPPORT AUX NOMBRE DE NOTES)

## Films les plus notés
nb.MostRatedMovies <- 25
order.MostRatedMovies <- order(stat.Movie$nb.Ratings, decreasing =TRUE)
cat("Les", nb.MostRatedMovies, "films ayant reçu le plus de notes : ")
for (movie in 1:nb.MostRatedMovies){
  cat("Le film d'ID", stat.Movie$movieID[order.MostRatedMovies[movie]], ",", 
      as.character(Data.Movies$title[order.MostRatedMovies[movie]]), ", a reçu", 
      stat.Movie$nb.Ratings[order.MostRatedMovies[movie]], "notes.")
  cat("\n")
}

## Films les moins notés
nb.LessRatedMovies <- 12
order.LessRatedMovies <- order(stat.Movie$nb.Ratings, decreasing =FALSE)
cat("Les", nb.LessRatedMovies, "films ayant reçus le moins de notes : ")
for (movie in 1:nb.LessRatedMovies){
  cat("Le film d'ID", stat.Movie$movieID[order.LessRatedMovies[movie]], ",", 
      as.character(Data.Movies$title[order.LessRatedMovies[movie]]), "a reçu", 
      stat.Movie$nb.Ratings[order.LessRatedMovies[movie]], "note(s).")
  cat("\n")
}

## Films n'ayant reçu qu'une seule note
cat("Il y a", sum(stat.Movie$nb.Ratings == 1), "films qui n'ont reçu qu'une seule note.")

## Films ayant reçu moins de xxx votes
nbLimit.RatingsForMovie <- 10
cat("Il y a", sum(stat.Movie$nb.Ratings <= nbLimit.RatingsForMovie), "films qui ont reçu moins de", 
    nbLimit.RatingsForMovie, "votes.")

#7.2# SUR LES FILMS (PAR RAPPORT A LA MOYENNE DES NOTES)

## Films ayant les meilleurs notes
nb.BestMovies <- 25
order.BestMovies <- order(stat.Movie$avgRating, decreasing = TRUE)
cat("Les", nb.BestMovies, "meilleurs films : ")
for (movie in 1:nb.BestMovies){
  cat("Le film d'ID", stat.Movie$movieID[order.BestMovies[movie]], ",", 
      as.character(data.Movies$title[order.BestMovies[movie]]),
      "a une note moyenne de", stat.Movie$avgRating[order.BestMovies[movie]], "sur",
      stat.Movie$nb.Ratings[order.BestMovies[movie]], "vote(s).")
  cat("\n")
}

## Nombre de films ayant une note moyenne de 5
cat("Il y a", sum(stat.Movie$avgRating == 5), "films qui ont reçu une note moyenne de 5.")

## Films ayant les meilleurs notes et dépassant un seuil de visionnage
cat("Les", nb.BestMovies, "meilleurs films ayant reçu plus de", nbLimit.RatingsForMovie, "notes: ")
stat.MovieUppernbLimit.Ratings <- stat.Movie[stat.Movie$nb.Ratings >= nbLimit.RatingsForMovie,]
order.BestMoviesUppernbLimit.Ratings <- order(stat.MovieUppernbLimit.Ratings$avgRating, decreasing = TRUE)
for (movie in 1:nb.BestMovies){
  cat("Le film d'ID", stat.MovieUppernbLimit.Ratings$movieID[order.BestMoviesUppernbLimit.Ratings[movie]], ",",
      as.character(data.Movies$title[stat.MovieUppernbLimit.Ratings$movieID[order.BestMoviesUppernbLimit.Ratings[movie]]]),
      "a une note moyenne de", stat.MovieUppernbLimit.Ratings$avgRating[order.BestMoviesUppernbLimit.Ratings[movie]], "sur",
      stat.MovieUppernbLimit.Ratings$nb.Ratings[order.BestMoviesUppernbLimit.Ratings[movie]], "votes.")
  cat("\n")
}

## Films ayant les pires notes
nb.WorstMovies <- 20
order.WorstMovies <- order(stat.Movie$avgRating, decreasing = FALSE)
cat("Les", nb.WorstMovies, "pires films : ")
for (movie in 1:nb.WorstMovies){
  cat("Le film d'ID", stat.Movie$movieID[order.WorstMovies[movie]], ",", 
      as.character(data.Movies$title[stat.Movie$movieID[order.WorstMovies[movie]]]),
      ",a une note moyenne de", stat.Movie$avgRating[order.WorstMovies[movie]], "sur",
      stat.Movie$nb.Ratings[order.WorstMovies[movie]], "vote(s).")
  cat("\n")
}

## Films ayant une note moyenne de 1
cat("Il y a", sum(stat.Movie$avgRating == 1), "films qui ont reçu une note moyenne de 1.")

## Films ayant les pires notes et dépassant un seuil de visionnage
cat("Les", nb.WorstMovies, "pires films ayant reçu plus de", nbLimit.RatingsForMovie, "notes: ")
stat.MovieUppernbLimit.Ratings <- stat.Movie[stat.Movie$nb.Ratings >= nbLimit.RatingsForMovie,]
order.WorstMoviesUppernbLimit.RatingsForMovie <- order(stat.MovieUppernbLimit.Ratings$avgRating, decreasing = FALSE)
for (movie in 1:nb.WorstMovies){
  cat("Le film d'ID", stat.MovieUppernbLimit.Ratings$movieID[order.WorstMoviesUppernbLimit.RatingsForMovie[movie]], ",",
      as.character(data.Movies$title[stat.MovieUppernbLimit.Ratings$movieID[order.WorstMoviesUppernbLimit.RatingsForMovie[movie]]]),
      "a une note moyenne de", stat.MovieUppernbLimit.Ratings$avgRating[order.WorstMoviesUppernbLimit.RatingsForMovie[movie]], "sur",
      stat.MovieUppernbLimit.Ratings$nb.Ratings[order.WorstMoviesUppernbLimit.RatingsForMovie[movie]], "votes.")
  cat("\n")
}

#7.3# SUR LES UTILISATEURS (PAR RAPPORT AU NOMBRE DE NOTES DONNEES)

## Utilisateurs ayant le plus voté
nb.MostVotedUsers <- 10
order.MostVotedUsers <- order(stat.User$nb.Ratings, decreasing =TRUE)
cat("Les", nb.MostVotedUsers, "utilisateurs ayant notés le plus de films : ")
for (user in 1:nb.MostVotedUsers){
  cat("L'utilisateur d'ID", stat.User$userID[order.MostVotedUsers[user]], "a voté", 
      stat.User$nb.Ratings[order.MostVotedUsers[user]], "fois.")
  cat("\n")
}

## Utilisateurs ayant le moins voté
nb.LessVotedUsers <- 35
order.LessVotedUsers <- order(stat.User$nb.Ratings, decreasing =FALSE)
cat("Les", nb.LessVotedUsers, "utilisateurs ayant notés le moins de films : ")
for (user in 1:nb.LessVotedUsers){
  cat("L'utilisateur d'ID", stat.User$userID[order.LessVotedUsers[user]], "n'a voté que", 
      stat.User$nb.Ratings[order.LessVotedUsers[user]], "fois.")
  cat("\n")
}

## Utilisateurs n'ayant voté que 20 fois
cat("Il y a", sum(stat.User$nb.Ratings == 20), "utilisateurs qui n'ont voté que 20 fois.")

## Utilisateurs ayant voté moins de xxx fois
nbLimit.RatingsForUser <- 75
cat("Il y a", sum(stat.User$nb.Ratings <= nbLimit.RatingsForUser), "utilisateurs qui ont voté moins de", 
    nbLimit.RatingsForUser, "fois.")

#7.4# SUR LES UTILISATEURS (PAR RAPPORT A LA MOYENNE DES NOTES)

## Utilisateurs ayant donnés les plus hautes notes
nb.NicestUsers <- 20
order.NicestUsers <- order(stat.User$avgRating, decreasing = TRUE)
cat("Les", nb.NicestUsers, "utilisateurs les plus gentils : ")
for (user in 1:nb.NicestUsers){
  cat("L'utilisateur d'ID", stat.User$userID[order.NicestUsers[user]], "a une donné en moyenne une note de", 
      stat.User$avgRating[order.NicestUsers[user]], "pour", stat.User$nb.Ratings[order.NicestUsers[user]], "notes.")
  cat("\n")
}

## Utilisateurs ayant donné les meilleurs notes et dépassant un seuil de participation
cat("Les", nb.NicestUsers, "utilisateurs ayant données les meilleurs notes et ayant voté plus de", nbLimit.RatingsForUser, "fois: ")
stat.UserUppernbLimit.Ratings <- stat.User[stat.User$nb.Ratings >= nbLimit.RatingsForUser,]
order.NicestUsersUppernbLimit.Ratings <- order(stat.UserUppernbLimit.Ratings$avgRating, decreasing = TRUE)
for (user in 1:nb.NicestUsers){
  cat("L'utilisateur d'ID", stat.UserUppernbLimit.Ratings$userID[order.NicestUsersUppernbLimit.Ratings[user]], 
      "a donné en moyenne une note de", stat.UserUppernbLimit.Ratings$avgRating[order.NicestUsersUppernbLimit.Ratings[user]], 
      "pour", stat.UserUppernbLimit.Ratings$nb.Ratings[order.NicestUsersUppernbLimit.Ratings[user]], "notes.")
  cat("\n")
}

## Utilisateurs ayant donnés les plus faibles notes
nb.MeanestUsers <- 20
order.MeanestUsers <- order(stat.User$avgRating, decreasing = FALSE)
cat("Les", nb.MeanestUsers, "utilisateurs les plus méchants : ")
for (user in 1:nb.MeanestUsers){
  cat("L'utilisateur d'ID", stat.User$userID[order.MeanestUsers[user]], "a une donné en moyenne une note de", 
      stat.User$avgRating[order.MeanestUsers[user]], "pour", stat.User$nb.Ratings[order.MeanestUsers[user]], "notes.")
  cat("\n")
}

## Utilisateurs ayant donné les pires notes et dépassant un seuil de participation
cat("Les", nb.MeanestUsers, "utilisateurs ayant données les pires notes et ayant voté plus de", nbLimit.RatingsForUser, "fois: ")
stat.UserUppernbLimit.Ratings <- stat.User[stat.User$nb.Ratings >= nbLimit.RatingsForUser,]
order.MeanestUsersUppernbLimit.Ratings <- order(stat.UserUppernbLimit.Ratings$avgRating, decreasing = FALSE)
for (user in 1:nb.MeanestUsers){
  cat("L'utilisateur d'ID", stat.UserUppernbLimit.Ratings$userID[order.MeanestUsersUppernbLimit.Ratings[user]], 
      "a donné en moyenne une note de", stat.UserUppernbLimit.Ratings$avgRating[order.MeanestUsersUppernbLimit.Ratings[user]], 
      "pour", stat.UserUppernbLimit.Ratings$nb.Ratings[order.MeanestUsersUppernbLimit.Ratings[user]], "notes.")
  cat("\n")
}

# ======================================== 8.ETUDE PAR AGE =================================================

# ====================================== 9.ETUDE PAR SEXE =================================================

## Création des bases par sexe
data.MaleUsers <- Data.Users[Data.Users$gender == "M" ,]
data.FemaleUsers <- Data.Users[Data.Users$gender == "F" ,]

stat.MaleUser <- stat.User[stat.User$userID %in% data.MaleUsers$userID,]
stat.FemaleUser <- stat.User[stat.User$userID %in% data.FemaleUsers$userID,]

## Statistiques sur la répartion des notes par sexe
summary(stat.MaleUser)
summary(stat.FemaleUser)

# ====================================== 10.ETUDE PAR GENRE DE FILMS ========================================
