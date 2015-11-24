# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah, Ulrich Mpeli Mpeli
#
#       Fichier : descriptions.R
#       Description : analyses descriptives sur la base des notes, des utilisateurs et des films
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls())
cat("\014")  

## Set working
setwd("~/ENSAE/2AD/Stat-App/Collaborative_Filtering_Project/Codes/Descriptive_analysis")

# ================================== 2.OUVERTURE DES FICHIERS =======================================

## Lecture du fichier des notes
ratingFID <- "../../Data/ml-100k/u.data."
fullData.Ratings <- read.csv(ratingFID, header = FALSE, sep='\t')
colnames(fullData.Ratings) <- c("userID", "movieID", "rating", "timestamp")
fullData.Ratings$timestamp <- NULL

## Lecture du fichier des utilisateurs
userFID <- "../../Data/ml-100k/u.user"
data.Users <- read.csv(userFID, header = FALSE, sep='|', stringsAsFactors = TRUE)
colnames(data.Users) <- c("userID", "age", "gender", "occupation", "zip.code")

## Lecture du fichier des films
movieFID <- "../../Data/ml-100k/u.item"
data.Movies <- read.csv(movieFID, header = FALSE, sep='|')
vect.movieGenres <- c("unknown", "action", "adventure", "animation", "children's", "comedy", 
                      "crime", "documentary", "drama", "fantasy", "film-noir", "horror", 
                      "musical", "mystery", "romance", "sci-fi", "thriller", "war", "western")
nb.Genres <- length(vect.movieGenres)
colnames(data.Movies) <- c("movieID", "title", "date", "trash", "IMDbURL", vect.movieGenres)
data.Movies$trash <- NULL

# ======================== 3.DETERMINATION DU VOLUME DE DONNEES DES BASES ============================

  #3.1# A PARTIR DE LA BASE DES NOTES

## Nombre de notes
nb.Ratings <- dim(fullData.Ratings)[1]
cat("La base de données contient exactement", nb.Ratings, "notes.")

## Nombre d'utilisateurs uniques
nb.Users <- length(unique(fullData.Ratings$userID))
cat("La base de données contient exactement", nb.Users, "utilisateurs uniques.")

## Nombre de films uniques
nb.Movies <- length(unique(fullData.Ratings$movieID))
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
avgRatings <- mean(fullData.Ratings$rating)
cat("La moyenne des", nb.Ratings, "notes est de", avgRatings, ".")
summary(fullData.Ratings$rating)
boxplot(fullData.Ratings$rating, main = "Notes discrètes attribuées aux films")

  #4.2# SUR LES UTILISATEURS

## Age
summary(data.Users$age)

## Proportion homme/femme
nb.Men = sum(data.Users$gender == "M")
nb.Women = sum(data.Users$gender == "F")
cat("La base de données est composée de", nb.Men, "hommes, soit", 100*nb.Men/nb.Users, 
    "% et", nb.Women, "femmes, soit" ,100*nb.Women/nb.Users,"% .")

## Proportions dans les métiers
summary(data.Users$occupation)

## Proportions dans les codes postales
summary(data.Users$zip.code)
#résultat : le code postal n'est pas du tout pertinent (699 other)

  #4.3# SUR LES FILMS

## Répartition par genres de films
nb.MoviePerGenre <- matrix(0, nrow = nb.Genres, ncol = 1)
rownames(nb.MoviePerGenre) <- vect.movieGenres
for (genre in 1:nb.Genres){
  nb.MoviePerGenre[genre] <- sum(data.Movies[vect.movieGenres[genre]])
}
nb.MoviePerGenre
boxplot(nb.MoviePerGenre, main = "nombre de films par genre")

# ==================================== 5.STATISTIQUE D'ORDRE 2 =========================================

  #5.1# LIEN ENTRE NOMBRE DE FILMS ET UTILISATEUR

## Répartition du nombre de films notés par utilisateur
vect.nb.RatingsPerUser <- matrix(0, nrow = nb.Users, ncol = 2) #matrice comprenant l'ID de l'utilisateut et son nombre de films notés
for (user in 1:nb.Users) { 
  vect.nb.RatingsPerUser[user,] <- c(user,sum(fullData.Ratings$userID == user))
}
vect.nb.RatingsPerUser <- as.data.frame(vect.nb.RatingsPerUser)
colnames(vect.nb.RatingsPerUser) <- c("userID", "nb.Ratings")

## Statistiques sur le nombre de films notés par utilisateur
summary(vect.nb.RatingsPerUser$nb.Ratings)
boxplot(vect.nb.RatingsPerUser$nb.Ratings, main = "Nombre de films notés par utilisateur")

  #5.2# LIEN ENTRE MOYENNE DES NOTES ET UTILISATEUR

## Note moyenne par utilisateur
vect.AvgRatingPerUser <- matrix(0, nrow = nb.Users, ncol = 2) #matrice comprenant l'ID de l'utilisateur et sa note moyenne
for (user in 1:nb.Users) { 
  vect.AvgRatingPerUser[user,] <- c(user,sum(fullData.Ratings$rating[fullData.Ratings$userID == user]) / vect.nb.RatingsPerUser[user,2])
}
vect.AvgRatingPerUser <- as.data.frame(vect.AvgRatingPerUser)
colnames(vect.AvgRatingPerUser) <- c("userID", "avgRating")

## Statistiques sur les notes moyennes données par utilisateur
summary(vect.AvgRatingPerUser$avgRating)
boxplot(vect.AvgRatingPerUser$avgRating, main = "Note moyenne donnée par utilisateur")

  #5.3# LIEN ENTRE NOMBRE DE NOTES ET FILM

## Répartition du nombre de notes par film
vect.nb.RatingsPerMovie <- matrix(0, nrow = nb.Movies, ncol = 2) #matrice comprenant l'ID du film et som nombre de notes reçues
for (movie in 1:nb.Movies) { 
  vect.nb.RatingsPerMovie[movie,] <- c(movie,sum(fullData.Ratings$movieID == movie))
}
vect.nb.RatingsPerMovie <- as.data.frame(vect.nb.RatingsPerMovie)
colnames(vect.nb.RatingsPerMovie) <- c("movieID", "nb.Ratings")

## Statistiques sur le nombre de notes par films
summary(vect.nb.RatingsPerMovie$nb.Ratings)
boxplot(vect.nb.RatingsPerMovie$nb.Ratings, main = "Nombre de notes par film")

  #5.4# LIEN ENTRE MOYENNE DES NOTES ET FILM

## Note moyenne reçue par film
vect.AvgRatingPerMovie <- matrix(0, nrow = nb.Movies, ncol = 2) #matrice comprenant l'ID du film et sa note moyenne
for (movie in 1:nb.Movies) { 
  vect.AvgRatingPerMovie[movie,] <- c(movie,sum(fullData.Ratings$rating[fullData.Ratings$movieID == movie]) / vect.nb.RatingsPerMovie[movie,2])
}
vect.AvgRatingPerMovie <- as.data.frame(vect.AvgRatingPerMovie)
colnames(vect.AvgRatingPerMovie) <- c("movieID", "avgRating")

## Statistiques sur les notes moyennes reçues par film
summary(vect.AvgRatingPerMovie$avgRating)
boxplot(vect.AvgRatingPerMovie$avgRating, main = "Note moyenne des films")

# ====================== 6.CORRELATION ENTRE LES FILMS ET LES UTILISATEURS ===============================

## Corrélation entre note des films et nombre de visionnage de ce film (normalement oui, et positive)
stat.Movie <- merge(vect.nb.RatingsPerMovie, vect.AvgRatingPerMovie, by.x = "movieID", by.y = "movieID")
plot(stat.Movie$nb.Ratings, stat.Movie$avgRating, 
     main = "Lien entre nombre de visionnage et note moyenne par film", 
     xlab = "nombre de visionnage", 
     ylab = "note moyenne")

## Corrélation entre note attribuée et nombre de films vus (pas nécessairement sauf les rageux et les floodeurs)
stat.User <- merge(vect.nb.RatingsPerUser,vect.AvgRatingPerUser, by.x = "userID", bu.y = "userID")
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
      as.character(data.Movies$title[order.MostRatedMovies[movie]]), ", a reçu", 
      stat.Movie$nb.Ratings[order.MostRatedMovies[movie]], "notes.")
  cat("\n")
}

## Films les moins notés
nb.LessRatedMovies <- 12
order.LessRatedMovies <- order(stat.Movie$nb.Ratings, decreasing =FALSE)
cat("Les", nb.LessRatedMovies, "films ayant reçus le moins de notes : ")
for (movie in 1:nb.LessRatedMovies){
  cat("Le film d'ID", stat.Movie$movieID[order.LessRatedMovies[movie]], ",", 
      as.character(data.Movies$title[order.LessRatedMovies[movie]]), "a reçu", 
      stat.Movie$nb.Ratings[order.LessRatedMovies[movie]], "note(s).")
  cat("\n")
}

## Films n'ayant reçu qu'une seule note
cat("Il y a", sum(stat.Movie$nb.Ratings == 1), "films qui n'ont reçu qu'une seule note.")

## Films ayant reçu moins de xxx votes
nbLimit.RatingsForMovie <- 100
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
data.MaleUsers <- data.Users[data.Users$gender == "M" ,]
data.FemaleUsers <- data.Users[data.Users$gender == "F" ,]

stat.MaleUser <- stat.User[stat.User$userID %in% data.MaleUsers$userID,]
stat.FemaleUser <- stat.User[stat.User$userID %in% data.FemaleUsers$userID,]

## Statistiques sur la répartion des notes par sexe
summary(stat.MaleUser)
summary(stat.FemaleUser)

# ====================================== 10.ETUDE PAR GENRE DE FILMS ========================================
