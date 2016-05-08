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

cat(sprintf("Les problèmes proposés sont : ml-100k et ml-1m \n"))
repository = readline(prompt = "Choisissez un problème : ")

cat(sprintf("Les sous-bases proposés sont de taille : 5\n"))
nb.Datasets = as.integer(readline(prompt = "Choisissez un nombre de sous-bases : "))

file_list.Datasets = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/list.Datasets.Rdata")
load(file = file_list.Datasets)

# ====================== 3.CHARGEMENT DES FONCTIONS A UTILISER ====================================

#source("./SVD/svd_filledMatrix.R")
source("./SVD/svd_predictions.R")
source("./SVD/matUS_matSV.R")
source("./Util/get_limited_value.R")

#install.packages("zoo")
#install.packages("hydroGOF")
#install.packages("expm")
library("hydroGOF")
library("zoo")
library("expm")

# ======================================== 4. CHOIX DE PARAMETRES =================================

seq_tau = seq(5, 95, by = 10) # pour l'estimation du paramètre optimal
seq_tau_opt = seq(23, 26, by=0.5) # pour la base vierge
seq_howToFill = c("Item", "User")

# ================================= 5. PHASE DE PREDICTION POUR ESTIMER LE PARAMETRE ==============================

result_RMSE_User = as.data.frame(matrix(0, nrow = length(seq_tau), ncol = nb.Datasets))
result_RMSE_Item = as.data.frame(matrix(0, nrow = length(seq_tau), ncol = nb.Datasets))

for(test in 1:nb.Datasets){
  cat(sprintf("Calcul pour la sous-base : %.0f / %.0f \n", test, nb.Datasets))
  dataset_to_keep = (1:nb.Datasets)[(1:nb.Datasets) != test]
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  test.Ratings=list.Datasets[[test]]
  
  stat.Users = read.table(paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/train", test, "/stat.Users.tsv"), header = TRUE, sep='\t')
  
  #Prédiction pour howToFill = User
  if("User" %in% seq_howToFill){
    cat(sprintf(" Calcul SVD.User \n"))
    load(file = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/train", test, "/list.SVD_User.Rdata"))
    rownames(result_RMSE_User) = seq_tau
    
    for(tauIND in 1:length(seq_tau)){
      tau=seq_tau[tauIND]
      cat(sprintf("   Tau = %.0f %%\n", tau))      
      
      pred = svd_predictions(list.Datasets, test, tau, list.SVD_User, stat.Users)

      result_RMSE_User[tauIND,test] = rmse(pred$rating, pred$prating)
    }
    
  }
  
  #Prédiction pour howToFill = Item
  if("Item" %in% seq_howToFill){
    cat(sprintf(" Calcul SVD.Item \n"))
    load(file = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/train", test, "/list.SVD_Item.Rdata"))
    rownames(result_RMSE_Item) = seq_tau
    
    for(tauIND in 1:length(seq_tau)){
      tau=seq_tau[tauIND]
      cat(sprintf("   Tau = %.0f %% \n", tau))      
      
      pred = svd_predictions(list.Datasets, test, tau, list.SVD_Item, stat.Users)
      
      result_RMSE_Item[tauIND,test] = rmse(pred$rating, pred$prating)
    }
  }
}

if("User" %in% seq_howToFill){
  result_RMSE_User$moyenne = rowMeans(result_RMSE_User[, 1:nb.Datasets])
  write.table(result_RMSE_User, paste0("./Results/", repository, "/results_svdUser_PredictionTest.tsv"), col.names = NA, sep='\t')
}

if("Item" %in% seq_howToFill){
  result_RMSE_Item$moyenne = rowMeans(result_RMSE_Item[, 1:nb.Datasets])
  write.table(result_RMSE_Item, paste0("./Results/", repository, "/results_svdItem_PredictionTest.tsv"), col.names = NA, sep='\t')
}

# ================================= 6. PHASE DE PREDICTION POUR LA BASE VIERGE ==============================

cat(sprintf("\n Calcul pour la base vierge \n"))

#Adaptation du code pour prendre en compte la base vierge
file_data.Vierge = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/data.Ratings.Vierge.Rdata")
load(file = file_data.Vierge)
train.Ratings = do.call("rbind", list.Datasets)
test.Ratings = data.Ratings.Vierge
list.Datasets = list(test.Ratings,train.Ratings)

# Statitiques sur les utilisateurs
stat.Users = read.table(paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/vierge/stat.Users.tsv"), header = TRUE, sep='\t')

# Dimension du problème
nb.Tau = length(seq_tau)
nb.Tests = dim(test.Ratings)[1]

if("User" %in% seq_howToFill){
  cat(sprintf(" Calcul SVD.User \n"))
  
  load(file = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/vierge/list.SVD_User.Rdata"))
  result_RMSE_Vierge_User = as.data.frame(matrix(0, nrow = length(seq_tau_opt), ncol = 1))
  rownames(result_RMSE_Vierge_User) = seq_tau_opt
  pred.RatingsBySVDUser = test.Ratings
  
  for(tauIND in 1:length(seq_tau_opt)){
      tau = seq_tau_opt[tauIND]
      cat(sprintf("   Tau = %.1f %%\n", tau))  
      
      pred = svd_predictions(list.Datasets, 1, tau, list.SVD_User, stat.Users) # 1 désigne la base vierge
      pred.RatingsBySVDUser[,3+tauIND] = pred$rating
      
      result_RMSE_Vierge_User[tauIND,1] = rmse(pred$rating, pred$prating)
  }
  write.table(pred.RatingsBySVDUser, paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/vierge/prediction_svd.User.tsv"), row.names = FALSE, sep='\t')
}

if("Item" %in% seq_howToFill){
  cat(sprintf(" Calcul SVD.Item \n"))
  
  load(file = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/vierge/list.SVD_Item.Rdata"))
  result_RMSE_Vierge_Item = as.data.frame(matrix(0, nrow = length(seq_tau_opt), ncol = 1))
  rownames(result_RMSE_Vierge_Item) = seq_tau_opt
  pred.RatingsBySVDItem = test.Ratings
  
  for(tauIND in 1:length(seq_tau_opt)){
    tau = seq_tau_opt[tauIND]
    cat(sprintf("   Tau = %.1f %%\n", tau))  
    
    pred = svd_predictions(list.Datasets, 1, tau, list.SVD_Item, stat.Users) # 1 désigne la base vierge
    pred.RatingsBySVDItem[,3+tauIND] = pred$rating
    
    result_RMSE_Vierge_Item[tauIND,1] = rmse(pred$rating, pred$prating)
  }
  write.table(pred.RatingsBySVDItem, paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/vierge/prediction_svd.Item.tsv"), row.names = FALSE, sep='\t')
}

if("User" %in% seq_howToFill){
  write.table(result_RMSE_Vierge_User, paste0("./Results/", repository, "/results_svdUser_PredictionTest_vierge.tsv"), col.names = NA, sep='\t')
}

if("Item" %in% seq_howToFill){
  write.table(result_RMSE_Vierge_Item, paste0("./Results/", repository, "/results_svdItem_PredictionTest_vierge.tsv"), col.names = NA, sep='\t')
}
