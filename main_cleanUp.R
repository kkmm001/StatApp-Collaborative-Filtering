# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquÃ©e
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_cleanUp.R
#       Description : nettoyage des bases de données
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 

# ======================================== 2.OUVERTURE DES FICHIERS =================================

source("open_file.R")
#open_file(repository = ml-100k)

# ======================================== 2.NETTOYAGE DES FICHIERS =================================

data.SameMovies = data.Movies[duplicated(data.Movies$title),c("movieID", "title", "date")]

delete_Movie = function(movieID){
}

merge_Movie = function(movieID1, movieID2){
}


