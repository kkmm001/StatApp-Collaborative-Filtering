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

## BASES D'APPRENTISSAGE ET DE TEST
nb.Tests = length(list.Datasets)
# liss.Datasets est une list de 5 éléments ou chaque élement
# est une liste comoposée d'un train.Ratings et d 'un test.Rating

## Création des matrices servant de paramètres
matUS=list()
matSV=list()
for(testID in 1:nb.Tests){
  train.Ratings=list.Datasets[testID]$train
  test.Ratings=list.Datasets[testID]$test
  matUS[testID]=svd(AvrRtg,train.Ratings,X)$US
  matSV[testID]=svd(AvrRtg,train.Ratings,X)$SV
}


# Prediction
AvrRTg="Item"
Pred=svd_prediction(AvrRtg,list.Datasets,matUS,matSV,data.Movies,data.Users,X)

# Cross Validation




