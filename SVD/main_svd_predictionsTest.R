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


# ======================================== 2.OUVERTURE DES FICHIERS =================================

repository = readline(prompt = "Choisissez un problème : ") #ml-100k

# ====================== 3.CHARGEMENT DES BASES D'APPRENTISSAGE ET DE TEST ==========================

nb.Tests = as.integer(readline(prompt = "Choisissez un nombre de sous-bases : "))

file_list.Datasets = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/list.Datasets.Rdata")
load(file = file_list.Datasets)

# ====================== 2.CHARGEMENT DES FONCTIONS A UTILISER ====================================

source("./SVD/svd2.R")
source("./SVD/svd_predictions.R")
source("./Util/stat_Movies.R")
source("./Util/stat_Users.R")

# ======================================== 3. PHASE DE PREPARATION =================================

## BASES D'APPRENTISSAGE ET DE TEST
# list.Datasets est une list de 5 data.frame

## Création des matrices servant de paramètres

X = as.numeric(readline(prompt = "Choisissez une proportion d'inertie à garder : "))
cat(sprintf("Les façons de remplis sont Item ou User \n"))
AvrRtg = readline(prompt = "Choisissez un mode de remplissage : ")

matUS=list()
matSV=list()
for(testID in 1:nb.Tests){
  dataset_to_keep = (1:nb.Tests)[(1:nb.Tests) != testID]
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  test.Ratings=list.Datasets[[testID]]
  mat.SVD=svd2(AvrRtg,train.Ratings)
  matUS[[testID]]=matUS_matSV(mat.SVD,X)$US
  matSV[[testID]]=matUS_matSV(mat.SVD,X)$SV
}
# ======================================== 4. PREDICTION =================================

# A CHARGER LES DEUX listes de MATRICES load(matSV)

# Prediction

pred=list()
for(testID in 1:nb.Tests){
  Pred[[testID]]=svd_prediction(AvrRtg,list.Datasets,testID,matUS,matSV,data.Movies,data.Users,X)
}


# Cross Validation
