# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_svd_predictionsTest.R
#       Description : résultats des tests par validation croisée
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 


# ====================== 2.CHARGEMENT DES TABLEAUX DE PREDICTION ====================================

source("./svd.R")
source("./svd_predictions.R")
source("./stat_Movies.R")
source("./stat_Users.R")

# ======================================== 3.PREDICTION =================================


