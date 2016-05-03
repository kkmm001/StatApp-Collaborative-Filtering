# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquÃ©e
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_svd_predictionsTest.R
#       Description : rÃ©sultats des tests par validation croisÃ©e
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 


# ======================================== 2.OUVERTURE DES FICHIERS =================================

repository = readline(prompt = "Choisissez un problÃ¨me : ") #ml-100k

# ====================== 3.CHARGEMENT DES BASES D'APPRENTISSAGE ET DE TEST ==========================

nb.Tests = as.integer(readline(prompt = "Choisissez un nombre de sous-bases : "))

file_list.Datasets = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/list.Datasets.Rdata")
load(file = file_list.Datasets)

# ====================== 2.CHARGEMENT DES FONCTIONS A UTILISER ====================================

source("./SVD/svd2.R")
source("./SVD/svd_predictions.R")
source("./SVD/matUS_matSV.R")
source("./Util/stat_Movies.R")
source("./Util/stat_Users.R")

#install.packages("zoo")
#install.packages("hydroGOF")
#install.packages("expm")
library("hydroGOF")
library("zoo")
library("expm")

# ======================================== 3. PHASE DE PREPARATION =================================

## BASES D'APPRENTISSAGE ET DE TEST
# list.Datasets est une list de 5 data.frame

## CrÃ©ation des matrices servant de paramÃ¨tres

#X = as.numeric(readline(prompt = "Choisissez une proportion d'inertie Ã  garder (entre 0 et 1) : "))
cat(sprintf("Les mÃ©thodes proposÃ©es pour remplir la matrice des notes sont : Item ou User \n"))
AvrRtg = readline(prompt = "Choisissez une mÃ©thode de remplissage de la matrice : ") 

matUS=list()
matSV=list()

seq_X = seq(0.23, 0.29, by=0.005)
result_RMSE = matrix(0, nrow = length(seq_X), ncol = nb.Tests)

for(testID in 1:nb.Tests){
  cat(paste("BASE TEST N°: ", testID,"\n" ))
  dataset_to_keep = (1:nb.Tests)[(1:nb.Tests) != testID]
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  test.Ratings=list.Datasets[[testID]]
  mat.SVD=svd2(AvrRtg,train.Ratings)
  for(INDX in 1:length(seq_X)){
    X=seq_X[INDX]
    cat(paste("\n","\n"))
    cat(paste("X :=", X,"\n"))
    cat(paste("Creation de matUS \n"))
    matUS[[testID]]=matUS_matSV(mat.SVD,X)$US
    cat(paste("Creation de matSV \n"))
    matSV[[testID]]=matUS_matSV(mat.SVD,X)$SV
    pred=svd_predictions(list.Datasets,testID,matUS,matSV)
    write.csv2(pred, paste0("svd_", AvrRtg, X, "_pred_train", testID, ".csv"), col.names = NA)
    result_RMSE[INDX,testID] = rmse(pred$rating, pred$prating)
  }
}

result=as.data.frame(result_RMSE)
rownames(result)=seq_X
write.csv2(result,paste0("./Results/Results.RMSE.SVDNaif.",AvrRtg,".X3.csv"),col.names = NA,sep="\t")



seq_X = seq(0.23, 0.26, by=0.005)
result_RMSE = matrix(0, nrow = length(seq_X), ncol = 1)
train.Ratings = do.call("rbind", list.Datasets)
test.Ratings=data.Ratings.Vierge
matUS=list()
matSV=list()
list.Datasets.Test=list(test.Ratings,train.Ratings)
mat.SVD=svd2(AvrRtg,train.Ratings)
for(INDX in 1:length(seq_X)){
    X_opt=seq_X[INDX]
    cat(paste("\n","\n"))
    cat(paste("X :=", X_opt,"\n"))
    cat(paste("Creation de matUS \n"))
    Q=matUS_matSV(mat.SVD,X_opt)
    matUS[[1]]=Q$US
    cat(paste("Creation de matSV \n"))
    matSV[[1]]=Q$SV
    pred=svd_predictions(list.Datasets.Test,1,matUS,matSV)
    write.csv2(pred, paste0("svd_pred_sur_base_vierge", AvrRtg, X_opt,".csv"), col.names = NA)
    result_RMSE[INDX,1] = rmse(pred$rating, pred$prating)
  }
result=as.data.frame(result_RMSE)
rownames(result)=seq_X
write.csv2(result,paste0("./Results/Results.RMSE.SVDNaif.pred_sur_base_vierge.",AvrRtg,".",X_opt,".csv"),col.names = NA,sep="\t")

