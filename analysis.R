# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : analysis.R
#       Description : analyses descriptives sur la base des notes, des utilisateurs et des films
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 

## Set working
getwd() 
#setwd("C:/Users/Maestro/Desktop/2A/Stat_App/Donnees") # Changer le repertoire de travail #
#setwd("~/ENSAE/2AD/Stat-App/Collaborative_Filtering_Project/Codes/Descriptive_analysis")

# ================================== 2.OUVERTURE DES FICHIERS =======================================

## Lecture du fichier des notes

data.Ratings = read.table(file=file.choose(),header=F,colClasses = c(V4 = "NULL"))
#data.Ratings <- read.csv("../../Data/ml-100k/u.data.", header = FALSE, sep='\t')
#data.Ratings$timestamp <- NULL
colnames(data.Ratings) = c("userID", "movieID", "rating")

## Lecture du fichier des utilisateurs

data.Users = read.table(file=file.choose(),header=F, sep='|', stringsAsFactors = TRUE)
#data.Users <- read.csv("../../Data/ml-100k/u.user", header = FALSE, sep='|', stringsAsFactors = TRUE)
colnames(data.Users) = c("userID", "age", "gender", "occupation", "zip.code")

## Lecture du fichier des films

data.Movies = read.table(file=file.choose(),header=F,sep="|", quote = "\"",colClasses = c(V4 = "NULL"))
#data.Movies <- read.csv("../../Data/ml-100k/u.item", header = FALSE, sep='|')
#data.Movies[4] <- NULL
vect.movieGenres = c("unknown", "action", "adventure", "animation", "children's", "comedy", 
                      "crime", "documentary", "drama", "fantasy", "film-noir", "horror", 
                      "musical", "mystery", "romance", "sci-fi", "thriller", "war", "western")

# TO ANSWER : A quoi ça sert ?
#MG=read.table(file=file.choose(),header=F,sep="|", quote = "\"") ##choisir u.genre
#MG=MG$V1
#nbG=length(MG)

nb.Genres = length(vect.movieGenres)
colnames(data.Movies) = c("movieID", "title", "date", "IMDbURL", vect.movieGenres)
#colnames(data.Movies)=c("movieID", "title", "date", "IMDbURL", MG) à modifier de tel sorte que MG soit une string


# ======================== 3.DETERMINATION DU VOLUME DE DONNEES DES BASES (controles) ============================

#3.1# A PARTIR DE LA BASE DES NOTES

## Nombre de notes

nb.Ratings = dim(data.Ratings)[1]
cat("La base de données contient exactement", nb.Ratings, "notes.")

## Nombre d'utilisateurs uniques

nb.Users = length(unique(data.Ratings$userID))
cat("La base de données contient exactement", nb.Users, "utilisateurs uniques.")

## Nombre de films uniques

nb.Movies = length(unique(data.Ratings$movieID))
cat("La base de données contient exactement", nb.Movies, "films uniques.")

#3.2# VERIFICATION AVEC LA BASE DES UTILISATEURS

## Nombre d'utilisateurs

if(nb.Users == dim(data.Users)[1]){
  cat("La base de données contient bien", nb.Users, "utilisateurs.")
} else{
  cat("Attention : il y a un conflit sur le nombre d'utilisateurs.")
  break
}

#3.3# VERIFICATION AVEC LA BASE DES FILMS

## Nombre de films

if (nb.Movies == dim(data.Movies)[1]){
  cat("La base de données contient bien", nb.Movies, "films.")
} else{
  cat("Attention : il y a un conflit sur le nombre de films.")
  break
}

# ====================================== 4.STATISTIQUE D'ORDRE 1 =========================================

#4.1# SUR LES NOTES

## Statistiques des notes

recap.Ratings = summary(data.Ratings$rating)
cat("La moyenne des", nb.Ratings, "notes est de", recap.Ratings['Mean'], "; l'écart-type est de", round(ave(data.Ratings$rating, FUN=sd)[1],2))
summary(data.Ratings$rating)
boxplot(data.Ratings$rating, main = "Notes discrètes attribuées aux films") #TO DO : remplacer par un diagramme en bâton

#4.2# SUR LES UTILISATEURS

## Age

stat.Age = summary(data.Users$age)
stat.Age
cat("La moyenne des âges est", stat.Age['Mean'], "ans avec un écart-type de",round(ave(data.Users$age, FUN=sd)[1],2), "ans.")

## Proportion homme/femme

nb.Men = sum(data.Users$gender == "M")
nb.Women = sum(data.Users$gender == "F")
cat("La base de données est composée de", nb.Men, "hommes, soit", round(100*nb.Men/nb.Users,2), 
    "% et", nb.Women, "femmes, soit" ,round(100*nb.Women/nb.Users,2),"% .")

## Proportions dans les métiers

as.data.frame(summary(data.Users$occupation))

## Proportions dans les codes postales

as.data.frame(summary(data.Users$zip.code))

cat("le code postal n'est pas du tout pertinent (699 other)")

#4.3# SUR LES FILMS

## Répartition par genres de films

vect.nb.MoviesPerGenre = matrix(0, nrow = nb.Genres, ncol = 1)
rownames(vect.nb.MoviesPerGenre) = vect.movieGenres
for (genre in 1:nb.Genres){
  vect.nb.MoviesPerGenre[genre] = sum(data.Movies[vect.movieGenres[genre]])
}
vect.nb.MoviesPerGenre
boxplot(vect.nb.MoviesPerGenre, main = "nombre de films par genre")

# ==================================== 5.STATISTIQUE D'ORDRE 2 =========================================

#5.1# LIEN ENTRE NOMBRE DE FILMS ET UTILISATEURS

## Répartition du nombre de films notés par utilisateur

vect.nb.RatingsPerUser = matrix(0, nrow = nb.Users, ncol = 2) #matrice comprenant l'ID de l'utilisateut et son nombre de films notés
for (user in 1:nb.Users) { 
  vect.nb.RatingsPerUser[user,] = c(user,sum(data.Ratings$userID == user))
}
vect.nb.RatingsPerUser = as.data.frame(vect.nb.RatingsPerUser)
colnames(vect.nb.RatingsPerUser) = c("userID", "nb.Ratings")

## Statistiques sur le nombre de films notés par utilisateur

summary(vect.nb.RatingsPerUser$nb.Ratings)
boxplot(vect.nb.RatingsPerUser$nb.Ratings, main = "Nombre de films notés par utilisateur")

#5.2# LIEN ENTRE MOYENNE DES NOTES ET UTILISATEUR

ratingsPerUser = matrix(0, nrow = nb.Users, ncol = 6) # matrice comprenant l'ID de l'utilisateur 
                                                     #                    la moyenne des notes
                                                     #                    l'écart-type des notes
                                                     #                    la note maximale 
                                                     #                    la note maximale 
                                                     #                    la mediane des notes

for (user in 1:nb.Users) { 
  x=data.Ratings$rating[data.Ratings$userID == user] # TO DO : donner un nom avec du sens pour x
  ratingsPerUser[user,] = c(user,round(mean(x),2),round(sd(x),2),max(x),min(x),median(x))
}

## Tableau récapitulatif

recap.Users = cbind(vect.nb.RatingsPerUser,data.Users$age,data.Users$gender,data.Users$occupation,ratingsPerUser)
recap.Users = recap.Users[ ,-6] # A optimiser
colnames(recap.Users) = c("userID","nb.movie","age","gender","occupation","avg", "sd", "max", "min", "median")
rm(x)

## Statistiques sur les notes moyennes données par utilisateur

summary(ratingsPerUser$avgRating)
boxplot(ratingsPerUser$avgRating, main = "Note moyenne donnée par utilisateur")

#5.3# LIEN ENTRE NOMBRE DE NOTES ET FILM

## Répartition du nombre de notes par film

nb.RatingsPerMovie =  matrix(0, nrow = nb.Movies, ncol = 2) #matrice comprenant l'ID du film et som nombre de notes reçues
for (movie in 1:nb.Movies) { 
  nb.RatingsPerMovie[movie,] = c(movie,sum(data.Ratings$movieID == movie))
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
  x=data.Ratings$rating[data.Ratings$movieID == movie]
  RatingPerMovie[movie,] = c(movie,round(mean(x),2),round(sd(x),2),max(x),min(x),median(x))
}
rm(x)

RatingPerMovie = as.data.frame(RatingPerMovie)
colnames(RatingPerMovie) = c("movieID","avgRating","SdRating", "MaxRating","MinRating","MedRating")


## Statistiques sur les notes moyennes reçues par film

summary(RatingPerMovie$avgRating)
boxplot(RatingPerMovie$avgRating, main = "Note moyenne des films")

## Nombre d'hommes et de femmes ayant vu un film donné (ainsi que la moyenne des notes attribuées)

RatingPerMoviePerGender = matrix(0, nrow = nb.Movies, ncol = 5) #matrice comprenant l'ID du film 
MoyWomen=matrix(0, nrow = nb.Movies, ncol = 2) 
for (movie in 1:nb.Movies) { 
  cond1=data.Ratings$movieID == movie
  cond2=(data.Users$gender[data.Ratings$userID]=="M")
  x=data.Ratings$rating[cond1&cond2]
  y=data.Ratings$rating[cond1&!(cond2)]
  RatingPerMoviePerGender[movie,] = c(movie,length(x),round(mean(x),2),length(y),round(mean(y),2))
  MoyWomen[movie,]=c(movie,(mean(data.Ratings$movieID == movie)-mean(x)*(length(x)/(length(x)+length(y))))*((length(x)+length(y))/length(y)))
}

RatingPerMoviePerGender = as.data.frame(RatingPerMoviePerGender)
colnames(RatingPerMoviePerGender ) = c("movieID", "nbMen","avgRatingMen","nbWomen","avgRatingWomen")

# on verifie que moyeWomen[i,2]=RatingPerMOviePerGender$avrRatingWomen[i] pour tout i

## Tableau récapitulatif

recap.Movies = cbind(nb.RatingsPerMovie,RatingPerMovie,RatingPerMoviePerGender)
recap.Movies = recap.Movies[ ,-3] # A optimiser
recap.Movies = recap.Movies[ ,-8] # A optimiser
#rm(RatingPerMovie,nb.RatingsPerMovie,MenPerMovie,MoyWomen,RatingPerMoviePerGender,
   #ratingsPerUser,vect.nb.MoviesPerGenre,vect.nb.RatingsPerUser,vect.movieGenres,cond1,cond2,x,y)

# ====================== 6.CORRELATION ENTRE LES FILMS ET LES UTILISATEURS ===============================

## Corrélation entre note des films et nombre de visionnage de ce film (normalement oui, et positive)

#recap.Movies = merge(nb.RatingsPerMovie, AvgRatingPerMovie, by.x = "movieID", by.y = "movieID")
plot(recap.Movies$nb.Ratings, recap.Movies$avgRating, 
     main = "Lien entre nombre de visionnage et note moyenne par film", 
     xlab = "nombre de visionnage", 
     ylab = "note moyenne")

## Corrélation entre note attribuée et nombre de films vus (pas nécessairement sauf les rageux et les floodeurs)
#recap.Users = merge(vect.nb.RatingsPerUser,AvgRatingPerUser, by.x = "userID", bu.y = "userID")
plot(recap.Users$nb.movie, recap.Users$avg, 
     main = "Lien entre nombre de films notés et note moyenne par utilisateur", 
     xlab = "nombre de films notés", 
     ylab = "note moyenne")

# ===================================== 7.STATISTIQUE D'ORDRE ===========================================

#7.1# SUR LES FILMS (PAR RAPPORT AUX NOMBRE DE NOTES)

## Films les plus notés

nb.MostRatedMovies = 25
order.MostRatedMovies = order(recap.Movies$nb.Ratings, decreasing =TRUE)
cat("Les", nb.MostRatedMovies, "films ayant reçu le plus de notes : ")
for (movie in 1:nb.MostRatedMovies){
  cat("Le film d'ID", recap.Movies$movieID[order.MostRatedMovies[movie]], ",", 
      as.character(data.Movies$title[order.MostRatedMovies[movie]]), ", a reçu", 
      recap.Movies$nb.Ratings[order.MostRatedMovies[movie]], "notes.")
  cat("\n")
}

## Films les moins notés

nb.LessRatedMovies = 12
order.LessRatedMovies = order(recap.Movies$nb.Ratings, decreasing =FALSE)
cat("Les", nb.LessRatedMovies, "films ayant reçus le moins de notes : ")
for (movie in 1:nb.LessRatedMovies){
  cat("Le film d'ID", recap.Movies$movieID[order.LessRatedMovies[movie]], ",", 
      as.character(data.Movies$title[order.LessRatedMovies[movie]]), "a reçu", 
      recap.Movies$nb.Ratings[order.LessRatedMovies[movie]], "note(s).")
  cat("\n")
}

## Films n'ayant reçu qu'une seule note

cat("Il y a", sum(recap.Movies$nb.Ratings == 1), "films qui n'ont reçu qu'une seule note.")

## Films ayant reçu moins de xxx votes

nbLimit.RatingsForMovie = 10
cat("Il y a", sum(recap.Movies$nb.Ratings <= nbLimit.RatingsForMovie), "films qui ont reçu moins de", 
    nbLimit.RatingsForMovie, "votes.")

#7.2# SUR LES FILMS (PAR RAPPORT A LA MOYENNE DES NOTES)

## Films ayant les meilleurs notes

nb.BestMovies = 25
order.BestMovies = order(recap.Movies$avgRating, decreasing = TRUE)
cat("Les", nb.BestMovies, "meilleurs films : ")
for (movie in 1:nb.BestMovies){
  cat("Le film d'ID", recap.Movies$movieID[order.BestMovies[movie]], ",", 
      as.character(data.Movies$title[order.BestMovies[movie]]),
      "a une note moyenne de", recap.Movies$avgRating[order.BestMovies[movie]], "sur",
      recap.Movies$nb.Ratings[order.BestMovies[movie]], "vote(s).")
  cat("\n")
}

## Nombre de films ayant une note moyenne de 5

cat("Il y a", sum(recap.Movies$avgRating == 5), "films qui ont reçu une note moyenne de 5.")

## Films ayant les meilleurs notes et dépassant un seuil de visionnage

cat("Les", nb.BestMovies, "meilleurs films ayant reçu plus de", nbLimit.RatingsForMovie, "notes: ")
recap.MovieUppernbLimit.Ratings = recap.Movies[recap.Movies$nb.Ratings >= nbLimit.RatingsForMovie,]
order.BestMoviesUppernbLimit.Ratings = order(recap.MovieUppernbLimit.Ratings$avgRating, decreasing = TRUE)
for (movie in 1:nb.BestMovies){
  cat("Le film d'ID", recap.MovieUppernbLimit.Ratings$movieID[order.BestMoviesUppernbLimit.Ratings[movie]], ",",
      as.character(data.Movies$title[recap.MovieUppernbLimit.Ratings$movieID[order.BestMoviesUppernbLimit.Ratings[movie]]]),
      "a une note moyenne de", recap.MovieUppernbLimit.Ratings$avgRating[order.BestMoviesUppernbLimit.Ratings[movie]], "sur",
      recap.MovieUppernbLimit.Ratings$nb.Ratings[order.BestMoviesUppernbLimit.Ratings[movie]], "votes.")
  cat("\n")
}

## Films ayant les pires notes

nb.WorstMovies = 20
order.WorstMovies <- order(recap.Movies$avgRating, decreasing = FALSE)
cat("Les", nb.WorstMovies, "pires films : ")
for (movie in 1:nb.WorstMovies){
  cat("Le film d'ID", recap.Movies$movieID[order.WorstMovies[movie]], ",", 
      as.character(data.Movies$title[recap.Movies$movieID[order.WorstMovies[movie]]]),
      ",a une note moyenne de", recap.Movies$avgRating[order.WorstMovies[movie]], "sur",
      recap.Movies$nb.Ratings[order.WorstMovies[movie]], "vote(s).")
  cat("\n")
}

## Films ayant une note moyenne de 1

cat("Il y a", sum(recap.Movies$avgRating == 1), "films qui ont reçu une note moyenne de 1.")

## Films ayant les pires notes et dépassant un seuil de visionnage

cat("Les", nb.WorstMovies, "pires films ayant reçu plus de", nbLimit.RatingsForMovie, "notes: ")
recap.MovieUppernbLimit.Ratings = recap.Movies[recap.Movies$nb.Ratings >= nbLimit.RatingsForMovie,]
order.WorstMoviesUppernbLimit.RatingsForMovie = order(recap.MovieUppernbLimit.Ratings$avgRating, decreasing = FALSE)
for (movie in 1:nb.WorstMovies){
  cat("Le film d'ID", recap.MovieUppernbLimit.Ratings$movieID[order.WorstMoviesUppernbLimit.RatingsForMovie[movie]], ",",
      as.character(data.Movies$title[recap.MovieUppernbLimit.Ratings$movieID[order.WorstMoviesUppernbLimit.RatingsForMovie[movie]]]),
      "a une note moyenne de", recap.MovieUppernbLimit.Ratings$avgRating[order.WorstMoviesUppernbLimit.RatingsForMovie[movie]], "sur",
      recap.MovieUppernbLimit.Ratings$nb.Ratings[order.WorstMoviesUppernbLimit.RatingsForMovie[movie]], "votes.")
  cat("\n")
}

#7.3# SUR LES UTILISATEURS (PAR RAPPORT AU NOMBRE DE NOTES DONNEES)

## Utilisateurs ayant le plus voté

nb.MostVotedUsers = 10
order.MostVotedUsers = order(recap.Users$nb.movie, decreasing =TRUE)
cat("Les", nb.MostVotedUsers, "utilisateurs ayant notés le plus de films : ")
for (user in 1:nb.MostVotedUsers){
  cat("L'utilisateur d'ID", recap.Users$userID[order.MostVotedUsers[user]], "a voté", 
      recap.Users$nb.movie[order.MostVotedUsers[user]], "fois.")
  cat("\n")
}

## Utilisateurs ayant le moins voté

nb.LessVotedUsers = 35
order.LessVotedUsers = order(recap.Users$nb.movie, decreasing =FALSE)
cat("Les", nb.LessVotedUsers, "utilisateurs ayant notés le moins de films : ")
for (user in 1:nb.LessVotedUsers){
  cat("L'utilisateur d'ID", recap.Users$userID[order.LessVotedUsers[user]], "n'a voté que", 
      recap.Users$nb.movie[order.LessVotedUsers[user]], "fois.")
  cat("\n")
}

## Utilisateurs n'ayant voté que 20 fois

cat("Il y a", sum(recap.Users$nb.movie == 20), "utilisateurs qui n'ont voté que 20 fois.")

## Utilisateurs ayant voté moins de xxx fois
nbLimit.RatingsForUser = 75
cat("Il y a", sum(recap.Users$nb.movie <= nbLimit.RatingsForUser), "utilisateurs qui ont voté moins de", 
    nbLimit.RatingsForUser, "fois.")

#7.4# SUR LES UTILISATEURS (PAR RAPPORT A LA MOYENNE DES NOTES)

## Utilisateurs ayant donnés les plus hautes notes

nb.NicestUsers = 20
order.NicestUsers <- order(recap.Users$avg, decreasing = TRUE)
cat("Les", nb.NicestUsers, "utilisateurs les plus gentils : ")
for (user in 1:nb.NicestUsers){
  cat("L'utilisateur d'ID", recap.Users$userID[order.NicestUsers[user]], "a une donné en moyenne une note de", 
      recap.Users$avg[order.NicestUsers[user]], "pour", recap.Users$nb.Ratings[order.NicestUsers[user]], "notes.")
  cat("\n")
}

## Utilisateurs ayant donné les meilleurs notes et dépassant un seuil de participation

cat("Les", nb.NicestUsers, "utilisateurs ayant données les meilleurs notes et ayant voté plus de", nbLimit.RatingsForUser, "fois: ")
recap.UserUppernbLimit.Ratings = recap.Users[recap.Users$nb.movie >= nbLimit.RatingsForUser,]
order.NicestUsersUppernbLimit.Ratings = order(recap.UserUppernbLimit.Ratings$avg, decreasing = TRUE)
for (user in 1:nb.NicestUsers){
  cat("L'utilisateur d'ID", recap.UserUppernbLimit.Ratings$userID[order.NicestUsersUppernbLimit.Ratings[user]], 
      "a donné en moyenne une note de", recap.UserUppernbLimit.Ratings$avg[order.NicestUsersUppernbLimit.Ratings[user]], 
      "pour", recap.UserUppernbLimit.Ratings$nb.movie[order.NicestUsersUppernbLimit.Ratings[user]], "notes.")
  cat("\n")
}

## Utilisateurs ayant donnés les plus faibles notes

nb.MeanestUsers = 20
order.MeanestUsers = order(recap.Users$avg, decreasing = FALSE)
cat("Les", nb.MeanestUsers, "utilisateurs les plus méchants : ")
for (user in 1:nb.MeanestUsers){
  cat("L'utilisateur d'ID", recap.Users$userID[order.MeanestUsers[user]], "a donné en moyenne une note de", 
      recap.Users$avg[order.MeanestUsers[user]], "pour", recap.Users$nb.movie[order.MeanestUsers[user]], "notes.")
  cat("\n")
}

## Utilisateurs ayant donné les pires notes et dépassant un seuil de participation

cat("Les", nb.MeanestUsers, "utilisateurs ayant données les pires notes et ayant voté plus de", nbLimit.RatingsForUser, "fois: ")
recap.UserUppernbLimit.Ratings = recap.Users[recap.Users$nb.movie >= nbLimit.RatingsForUser,]
order.MeanestUsersUppernbLimit.Ratings = order(recap.UserUppernbLimit.Ratings$avg, decreasing = FALSE)
for (user in 1:nb.MeanestUsers){
  cat("L'utilisateur d'ID", recap.UserUppernbLimit.Ratings$userID[order.MeanestUsersUppernbLimit.Ratings[user]], 
      "a donné en moyenne une note de", recap.UserUppernbLimit.Ratings$avg[order.MeanestUsersUppernbLimit.Ratings[user]], 
      "pour", recap.UserUppernbLimit.Ratings$nb.movie[order.MeanestUsersUppernbLimit.Ratings[user]], "notes.")
  cat("\n")
}

# ======================================== 8.ETUDE PAR AGE =================================================



# ====================================== 9.ETUDE PAR SEXE =================================================

## Création des bases par sexe

data.MaleUsers = data.Users[data.Users$gender == "M" ,]
data.FemaleUsers = data.Users[data.Users$gender == "F" ,]

recap.MaleUser = recap.Users[recap.Users$userID %in% data.MaleUsers$userID,]
recap.FemaleUser = recap.Users[recap.Users$userID %in% data.FemaleUsers$userID,]

## Statistiques sur la répartion des notes par sexe

summary(recap.MaleUser)
summary(recap.FemaleUser)

# ====================================== 10.ETUDE PAR GENRE DE FILMS ========================================
