# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquee
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_analysis.R
#       Description : analyses descriptives sur la base des notes, des utilisateurs et des films
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 

# ======================================== 2.OUVERTURE DES FICHIERS =================================

source("open_file.R")
#open_file()

# ======================== 3.DETERMINATION DU VOLUME DE DONNEES DES BASES (controles) ============================

#3.1# A PARTIR DE LA BASE DES NOTES

## Nombre de notes

nb.Ratings = dim(data.Ratings)[1]
cat(sprintf("La base de donnees des notes contient exactement %.0f notes. \n", nb.Ratings))

## Nombre d'utilisateurs uniques

nb.Users = length(unique(data.Ratings$userID))
cat(sprintf("La base de donnees des notes contient exactement %.0f utilisateurs uniques. \n", nb.Users))

## Nombre de films uniques

nb.Movies = length(unique(data.Ratings$movieID))
cat(sprintf("La base de donnees contient exactement %.0f films uniques. \n", nb.Movies))

#3.2# VERIFICATION AVEC LA BASE DES UTILISATEURS

## Nombre d'utilisateurs

if(nb.Users == dim(data.Users)[1]){
  cat(sprintf("La base de donnees des utilisateurs contient bien %.0f utilisateurs. \n", nb.Users))
} else{
  cat(sprintf("Attention : il y a un conflit sur le nombre d'utilisateurs. \n"))
}

#3.3# VERIFICATION AVEC LA BASE DES FILMS

## Nombre de films

if (nb.Movies == dim(data.Movies)[1]){
  cat(sprintf("La base de donnees contient bien %.0f films.\n", nb.Movies))
} else{
  cat(sprintf("Attention : il y a un conflit sur le nombre de films.\n"))
  break
}

# ====================================== 4.STATISTIQUE D'ORDRE 1 =========================================

#4.1# SUR LES NOTES

## Statistiques des notes

cat(sprintf("La moyenne des %.0f notes est de %.2f ; l'ecart-type est de %.2f.\n", 
            nb.Ratings,mean(data.Ratings$rating), sd(data.Ratings$rating)
              ))
plot(table(data.Ratings$rating), 
     main = paste(sprintf("Distribution des %.0f notes", nb.Ratings)),
     xlab = "Notes", 
     ylab = "Nombre de notes",
     lwd = 5, 
     lend = "square") 
table(data.Ratings$rating)

#4.2# SUR LES UTILISATEURS

## Age

cat(sprintf("La moyenne des ages est %.2f ans avec un ecart-type de %.2f ans.\n", 
            mean(data.Users$age),sd(data.Users$age)
            ))

## Proportion homme/femme

nb.Men = sum(data.Users$gender == "M")
nb.Women = sum(data.Users$gender == "F")
cat(sprintf("La base de donnees est composee de %.0f hommes, soit %.2f%% et %.0f femmes, soit %.2f%%.\n", 
    nb.Men, 100*nb.Men/nb.Users, nb.Women, 100*nb.Women/nb.Users
    ))

# Histogramme des ages

par(lend="butt")
dst = density(data.Users$age, na.rm = TRUE)
dstM = density(data.Users$age[data.Users$gender == "M"], na.rm = TRUE)
dstF = density(data.Users$age[data.Users$gender == "F"], na.rm = TRUE)

hist(data.Users$age, 
     col = grey(0.9), 
     border = grey(0.8),
     main = paste("Age des votants"),
     xlab = "Age [ans]",
     ylab = "Frequence",
     proba = TRUE, 
     ylim = c(0, max(dst$y))
     )

lines(dstM$x, nb.Men/nb.Users*dstM$y, lwd = 3, lty = 2, col = "darkblue")
lines(dstF$x, nb.Women/nb.Users*dstF$y, lwd = 3, lty = 2, col = "darkred")
lines(dst$x, dst$y)

legend("topright", 
       inset = 0.01, 
       legend = c("Femmes", "Hommes","Total"), 
       col = c("darkred","darkblue","black"),
       lty = c(2, 2,1), 
       lwd = 2, 
       pt.cex = 2
       )
rm(dst,dstM,dstF)

## Proportions dans les metiers
as.data.frame(summary(data.Users$occupation))
#remarque : ces categories professionnelles sont etranges : ce n'est pas une categorisation CSP traditionnelle.

## Proportions dans les codes postales
as.data.frame(summary(data.Users$zip.code))
#remarque : le code postal n'est pas du tout pertinent (699 other)

#4.3# SUR LES FILMS

## Repartition par genres de films
vect.nb.MoviesPerGenre = as.matrix(apply(data.Movies[5:23],2, sum, na.rm = TRUE))
order.nb.MoviesPerGenre = order(vect.nb.MoviesPerGenre)
barplot(vect.nb.MoviesPerGenre[order.nb.MoviesPerGenre],
        main = "Nombre de films par genre",
        names.arg = rownames(vect.nb.MoviesPerGenre)[order.nb.MoviesPerGenre],
        las = 3
        )

# ==================================== 5.STATISTIQUE D'ORDRE 2 =========================================

#5.1# Statistiques par utilisateur

source("stat_Users.R")
stat.Users = stat_Users(data.Ratings)
recap.Users = merge(data.Users, stat.Users, by.x = "userID", by.y = "userID")
rm(stat.Users)

## Statistiques sur le nombre de films notees par utilisateur

summary(recap.Users$nb.Ratings)
plot(table(recap.Users$nb.Ratings), 
     main = "Nombre de films notes par utilisateur", 
     ylab = "nombre d'occurrences",
     xlab = "nombre de films notes",
     lwd = 5, 
     lend = "square")
#Je ne trouve pas ce graphique interessant

## Statistiques sur les notes moyennes donnees par utilisateur

summary(recap.Users$mean)
boxplot(recap.Users$mean, main = "Note moyenne donnee par utilisateur")

#5.2# Statistiques par film

source("stat_Movies.R")
stat.Movies = stat_Movies(data.Ratings)
recap.Movies = merge(data.Movies, stat.Movies, by.x = "movieID", by.y = "movieID")
rm(stat.Movies)

## Statistiques sur le nombre de notes par films

summary(recap.Movies$nb.Ratings)
boxplot(recap.Movies$nb.Ratings, main = "Nombre de notes par film")

## Statistiques sur les notes moyennes recues par film

summary(recap.Movies$mean)
boxplot(recap.Movies$mean, main = "Note moyenne des films")

## Nombre d'hommes et de femmes ayant vu un film donne (ainsi que la moyenne des notes attribuees)

RatingPerMoviePerGender = matrix(0, nrow = nb.Movies, ncol = 5) #matrice comprenant l'ID du film 
MoyWomen=matrix(0, nrow = nb.Movies, ncol = 2) 
cond2=(data.Users$gender[data.Ratings$userID]=="M")
for (movie in 1:nb.Movies) { 
  cond1=data.Ratings$movieID == movie
  x=data.Ratings$rating[cond1&cond2]
  y=data.Ratings$rating[cond1&!(cond2)]
  RatingPerMoviePerGender[movie,] = c(movie,length(x),round(mean(x),2),length(y),round(mean(y),2))
  MoyWomen[movie,]=c(movie,(mean(data.Ratings$movieID == movie)-mean(x)*(length(x)/(length(x)+length(y))))*((length(x)+length(y))/length(y)))
}

RatingPerMoviePerGender = as.data.frame(RatingPerMoviePerGender)
colnames(RatingPerMoviePerGender ) = c("movieID", "nbMen","avgRatingMen","nbWomen","avgRatingWomen")

# on verifie que moyeWomen[i,2]=RatingPerMOviePerGender$avrRatingWomen[i] pour tout i

# ====================== 6.CORRELATION ENTRE LES FILMS ET LES UTILISATEURS ===============================

## Correlation entre note des films et nombre de visionnage de ce film (normalement oui, et positive)

plot(recap.Movies$nb.Ratings, recap.Movies$mean, 
     main = "Lien entre nombre de visionnage et note moyenne par film", 
     xlab = "nombre de visionnage", 
     ylab = "note moyenne")

## Correlation entre note attribuee et nombre de films vus (pas necessairement sauf les rageux et les floodeurs)

plot(recap.Users$nb.Ratings, recap.Users$mean, 
     main = "Lien entre nombre de films notes et note moyenne par utilisateur", 
     xlab = "nombre de films notes", 
     ylab = "note moyenne")

## Correlation entre age et note moyenne (quelle génération est la plus sévère ?)

plot(recap.Users$age, recap.Users$mean, 
     main = "Lien entre age et note moyenne", 
     xlab = "age", 
     ylab = "note moyenne")

## Correlation entre age et nombre de films notes (quelle génération est la plus cinephile ?)

plot(recap.Users$age, recap.Users$nb.Ratings, 
     main = "Lien entre age et nombre de films notes", 
     xlab = "age", 
     ylab = "nombre de films notes")

# ===================================== 7.STATISTIQUE D'ORDRE ===========================================

#7.1# SUR LES FILMS (PAR RAPPORT AUX NOMBRE DE NOTES)

## Films les plus notes

nb.MostRatedMovies = 25
order.MostRatedMovies = order(recap.Movies$nb.Ratings, decreasing =TRUE)
cat(sprintf("Les %.0f films ayant recu le plus de notes : \n", nb.MostRatedMovies))
for (movie in 1:nb.MostRatedMovies){
  id.order = order.MostRatedMovies[movie]
  cat(sprintf("Le film d'ID %.0f, %s, a recu %.0f notes.\n", 
      recap.Movies$movieID[id.order], recap.Movies$title[id.order], recap.Movies$nb.Ratings[id.order]
      ))
}

#remarque : 25 films (soit 1.5% des films) recuillent à eux seuls 10% des notes.

## Films les moins notes

nb.LessRatedMovies = 25
order.LessRatedMovies = order(recap.Movies$nb.Ratings, decreasing =FALSE)
cat(sprintf("Les %.0f films ayant recus le moins de notes :\n",nb.LessRatedMovies))
for (movie in 1:nb.LessRatedMovies){
  id.order = order.LessRatedMovies[movie]
  cat(sprintf("Le film d'ID %.0f, %s, a recu %.0f note(s).\n",
      recap.Movies$movieID[id.order], recap.Movies$title[id.order], recap.Movies$nb.Ratings[id.order]
      ))
}

## Films n'ayant recu qu'une seule note

cat(sprintf("Il y a  %.0f films qui n'ont recu qu'une seule note.\n", sum(recap.Movies$nb.Ratings == 1)))

## Films ayant recu moins de 10 votes

nbLimit.RatingsForMovie = 10
cat(sprintf("Il y a %.0f films qui ont recu moins de %.0f votes.\n",
    sum(recap.Movies$nb.Ratings <= nbLimit.RatingsForMovie), nbLimit.RatingsForMovie
    ))

#7.2# SUR LES FILMS (PAR RAPPORT A LA MOYENNE DES NOTES)

## Films ayant les meilleurs notes

nb.BestMovies = 25
order.BestMovies = order(recap.Movies$mean, decreasing = TRUE)
cat(sprintf("Les %.0f meilleurs films :\n", nb.BestMovies))
for (movie in 1:nb.BestMovies){
  id.order = order.BestMovies[movie]
  cat(sprintf("Le film d'ID %.0f, %s, a une note moyenne de %.2f sur %.0f vote(s).\n", 
      recap.Movies$movieID[id.order], recap.Movies$title[id.order], recap.Movies$mean[id.order], recap.Movies$nb.Ratings[id.order]
      ))
}

## Nombre de films ayant une note moyenne de 5

cat(sprintf("Il y a %.0f films qui ont recu une note moyenne de 5.\n", sum(recap.Movies$mean == 5)))

## Films ayant les meilleurs notes et depassant un seuil de visionnage

cat(sprintf("Les %.0f meilleurs films ayant recu plus de %.0f notes:\n", nb.BestMovies, nbLimit.RatingsForMovie))
recap.MovieUppernbLimit.Ratings = recap.Movies[recap.Movies$nb.Ratings >= nbLimit.RatingsForMovie,]
order.BestMoviesUppernbLimit.Ratings = order(recap.MovieUppernbLimit.Ratings$mean, decreasing = TRUE)
for (movie in 1:nb.BestMovies){
  id.order = order.BestMoviesUppernbLimit.Ratings[movie]
  cat(sprintf("Le film d'ID %.0f, %s, a une note moyenne de %.2f sur %.0f votes.\n",
      recap.MovieUppernbLimit.Ratings$movieID[id.order], recap.MovieUppernbLimit.Ratings$title[id.order], recap.MovieUppernbLimit.Ratings$mean[id.order], recap.MovieUppernbLimit.Ratings$nb.Ratings[id.order]
      ))
}

## Films ayant les pires notes

nb.WorstMovies = 10
order.WorstMovies = order(recap.Movies$mean, decreasing = FALSE)
cat(sprintf("Les %.0f pires films :\n", nb.WorstMovies))
for (movie in 1:nb.WorstMovies){
  id.order = order.WorstMovies[movie]
  cat(sprintf("Le film d'ID %.0f, %s, a une note moyenne de %.2f sur %.0f vote(s).\n",
      recap.Movies$movieID[id.order], recap.Movies$title[id.order], recap.Movies$mean[id.order], recap.Movies$nb.Ratings[id.order]
      ))
}

## Films ayant une note moyenne de 1

cat(sprintf("Il y a %.0f films qui ont recu une note moyenne de 1.\n", sum(recap.Movies$mean == 1)))

## Films ayant les pires notes et depassant un seuil de visionnage

cat(sprintf("Les %.0f pires films ayant recu plus de %.0f notes:\n", nb.WorstMovies, nbLimit.RatingsForMovie))
recap.MovieUppernbLimit.Ratings = recap.Movies[recap.Movies$nb.Ratings >= nbLimit.RatingsForMovie,]
order.WorstMoviesUppernbLimit.RatingsForMovie = order(recap.MovieUppernbLimit.Ratings$mean, decreasing = FALSE)
for (movie in 1:nb.WorstMovies){
  id.order = order.WorstMoviesUppernbLimit.RatingsForMovie[movie]
  cat(sprintf("Le film d'ID %.0f, %s, a une note moyenne de %.2f sur %.0f votes.\n", 
      recap.MovieUppernbLimit.Ratings$movieID[id.order], recap.MovieUppernbLimit.Ratings$title[id.order], recap.MovieUppernbLimit.Ratings$mean[id.order], recap.MovieUppernbLimit.Ratings$nb.Ratings[id.order]
      ))
}

#7.3# SUR LES UTILISATEURS (PAR RAPPORT AU NOMBRE DE NOTES DONNEES)

## Utilisateurs ayant le plus vote

nb.MostVotedUsers = 10
order.MostVotedUsers = order(recap.Users$nb.Ratings, decreasing =TRUE)
cat(sprintf("Les %.0f utilisateurs ayant note le plus de films :\n", nb.MostVotedUsers))
for (user in 1:nb.MostVotedUsers){
  id.order = order.MostVotedUsers[user]
  cat(sprintf("L'utilisateur d'ID %.0f a vote %.0f fois.\n", 
      recap.Users$userID[id.order], recap.Users$nb.Ratings[id.order]
  ))
}

## Utilisateurs ayant le moins vote

nb.LessVotedUsers = 15
order.LessVotedUsers = order(recap.Users$nb.Ratings, decreasing =FALSE)
cat(sprintf("Les %.0f utilisateurs ayant note le moins de films:\n", nb.LessVotedUsers))
for (user in 1:nb.LessVotedUsers){
  id.order = order.LessVotedUsers[user]
  cat(sprintf("L'utilisateur d'ID %.0f n'a vote que %.0f fois.\n",
      recap.Users$userID[id.order], recap.Users$nb.Ratings[id.order]
  ))
}

## Utilisateurs n'ayant vote que 20 fois

cat(sprintf("Il y a %.0f utilisateurs qui n'ont vote que 20 fois.\n", sum(recap.Users$nb.Ratings == 20)))

## Utilisateurs ayant vote moins de xxx fois

nbLimit.RatingsForUser = 50
cat(sprintf("Il y a %.0f utilisateurs qui ont vote moins de %.0f fois.\n", 
    sum(recap.Users$nb.Ratings <= nbLimit.RatingsForUser), nbLimit.RatingsForUser
    ))

#7.4# SUR LES UTILISATEURS (PAR RAPPORT A LA MOYENNE DES NOTES)

## Utilisateurs ayant donne les plus hautes notes

nb.NicestUsers = 20
order.NicestUsers <- order(recap.Users$mean, decreasing = TRUE)
cat(sprintf("Les %.0f utilisateurs les plus gentils :\n", nb.NicestUsers))
for (user in 1:nb.NicestUsers){
  id.order = order.NicestUsers[user]
  cat(sprintf("L'utilisateur d'ID %.0f a donne en moyenne une note de %.2f pour %.0f notes.\n",
      recap.Users$userID[id.order], recap.Users$mean[id.order], recap.Users$nb.Ratings[id.order]
  ))
}

## Utilisateurs ayant donnes les meilleurs notes et depassant un seuil de participation

cat(sprintf("Les %.0f utilisateurs ayant donne les meilleurs notes et ayant vote plus de %.0f fois:\n", nb.NicestUsers, nbLimit.RatingsForUser))
recap.UserUppernbLimit.Ratings = recap.Users[recap.Users$nb.Ratings >= nbLimit.RatingsForUser,]
order.NicestUsersUppernbLimit.Ratings = order(recap.UserUppernbLimit.Ratings$mean, decreasing = TRUE)
for (user in 1:nb.NicestUsers){
  id.order = order.NicestUsersUppernbLimit.Ratings[user]
  cat(sprintf("L'utilisateur d'ID %.0f a donne en moyenne une note de %.2f pour %.0f notes.\n", 
      recap.UserUppernbLimit.Ratings$userID[id.order], recap.UserUppernbLimit.Ratings$mean[id.order], recap.UserUppernbLimit.Ratings$nb.Ratings[id.order]
      ))
}

## Utilisateurs ayant donnes les plus faibles notes

nb.MeanestUsers = 20
order.MeanestUsers = order(recap.Users$mean, decreasing = FALSE)
cat(sprintf("Les %.0f utilisateurs les plus severes :\n", nb.MeanestUsers))
for (user in 1:nb.MeanestUsers){
  id.order = order.MeanestUsers[user]
  cat(sprintf("L'utilisateur d'ID %.0f a donne en moyenne une note de %.2f pour %.0f notes.\n",
      recap.Users$userID[id.order], recap.Users$mean[id.order], recap.Users$nb.Ratings[id.order]
      ))
}

## Utilisateurs ayant donnes les pires notes et depassant un seuil de participation

cat(sprintf("Les %.0f utilisateurs ayant donne les pires notes et ayant vote plus de %.0f fois:\n", nb.MeanestUsers, nbLimit.RatingsForUser))
recap.UserUppernbLimit.Ratings = recap.Users[recap.Users$nb.Ratings >= nbLimit.RatingsForUser,]
order.MeanestUsersUppernbLimit.Ratings = order(recap.UserUppernbLimit.Ratings$mean, decreasing = FALSE)
for (user in 1:nb.MeanestUsers){
  id.order = order.MeanestUsersUppernbLimit.Ratings[user]
  cat(sprintf("L'utilisateur d'ID %.0f a donne en moyenne une note de %.2f pour %.0f notes.\n",
      recap.UserUppernbLimit.Ratings$userID[id.order], recap.UserUppernbLimit.Ratings$mean[id.order], recap.UserUppernbLimit.Ratings$nb.Ratings[id.order]
      ))
}

# ======================================== 8.ETUDE PAR AGE =================================================



# ====================================== 9.ETUDE PAR SEXE =================================================

## Creation des bases par sexe

data.MaleUsers = data.Users[data.Users$gender == "M" ,]
data.FemaleUsers = data.Users[data.Users$gender == "F" ,]

recap.MaleUser = recap.Users[recap.Users$userID %in% data.MaleUsers$userID,]
recap.FemaleUser = recap.Users[recap.Users$userID %in% data.FemaleUsers$userID,]

## Statistiques sur la repartion des notes par sexe

summary(recap.MaleUser)
summary(recap.FemaleUser)

#remarque : il y a peu de différence entre les hommes et les femmes

# ====================================== 10.ETUDE PAR GENRE DE FILMS ========================================
