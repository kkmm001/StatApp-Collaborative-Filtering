# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main.R
#       Description : fonction principal pour les recommandations
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ======================================== 1.PREAMBULE =============================================

## Clean up
rm(list=ls()) 
cat("\014") 

repository = readline(prompt="Choisissez un problème : ")

# ========================== 2.PREPARATION DE L'ALGORITHME NAIF ===========================

recap.Movies = read.table(file = paste0("./Results/", repository, "/recap.Movies.tsv"), header=T, sep='\t')
load(file = paste0("./Results/", repository, "/dejaVu.Rdata"))

# ======================= 3.RECOMMANDATION VIA ALGORITHME NAIF ===========================

source("./NaiveAlgorithms/recommandation_meanByMovie.R")
nb.recommandations = as.integer(readline(prompt="Choisissez un nombre de recommandation : "))
userID = as.integer(readline(prompt="Choisissez un utilisateur : "))

vect.RecommendedMovies = recommandation_meanByMovie(recap.Movies, dv, userID, nb.recommandations, threshold = 50)
print(vect.RecommendedMovies)

# ================== 4.PREPARATION DE L'ALGORITHME DES KNN USER-USER ===========================

# ================ 5.RECOMMANDATION VIA L'ALGORITHME des KNN USER-USER =======================

#nb.recommandations = readline(prompt="Choisissez un nombre de recommandation : ")
#userID = readline(prompt="Choisissez un utilisateur : ")
