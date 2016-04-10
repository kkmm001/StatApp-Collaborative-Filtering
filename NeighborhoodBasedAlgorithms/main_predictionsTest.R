# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_knn_user_predictionsTest.R
#       Description : résultats des tests par validation croisée
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 

# ======================================== 2.OUVERTURE DES FICHIERS =================================

#source("./Util/open_files.R", encoding = 'UTF-8')
repository = readline(prompt = "Choisissez un problème : ") #ml-100k

# ====================== 3.GENERATION DES BASES D'APPRENTISSAGE ET DE TEST ==========================

file_list.Datasets = paste0("./CrossValidation/", repository, "/list.Datasets.Rdata")
load(file = file_list.Datasets)
nb.Tests = length(list.Datasets)

# ============================== 4.CHOIX DE PARAMETRES =========================================
library("hydroGOF")

#source("./Util/error_function.R")
#error_names = c("RMSE")
#nb.Errors = length(error_names)

Qmax = 20

similarity_names = c("pearson") #TODO A BOUCLER
nb.Similarity = length(similarity_names)

list.nbMin.InCommon = c(0,2) #4,6,8)
nb.nbMin.InCommon = length(list.nbMin.InCommon)

method_names = c("mean") #, "pondered", "centred-pondered")
nb.method = length(method_names)

mat.names_predictors = expand.grid(similarity_names, list.nbMin.InCommon, method_names)
colnames(mat.names_predictors) = c("similarity", "nbMin.InCommon", "predicteur")
nb.Predictors.byQ = nb.Similarity * nb.nbMin.InCommon * nb.method

# ====================== 5.CHARGEMENT DES TABLEAUX DE PREDICTION ====================================

source("./NeighborhoodBasedAlgorithms/Q_nearest_neighbors.R")
source("./NeighborhoodBasedAlgorithms/knn_user_predictions.R")
source("./NeighborhoodBasedAlgorithms/knn_user_predicteur.R")

#for(error in error_names){
#  assign(paste0('result_',error),as.data.frame(matrix(0, nrow=nb.Predictors.byQ, ncol = n)))
#}

result_error = as.data.frame(matrix(0, nrow=nb.Predictors.byQ, ncol = Qmax))

# =================== 6.CALCUL DES TABLEAUX DE PREDICTION ================================

#TODO a changer : 1:n et non 1:1
for(train in 1:1){ # pour chaque couple train/test de la validation croisée
  
  # Chargement des listes déjà vus
  file_list.dejaVu = paste0("./CrossValidation/", repository, "/train", train, "/list.dejaVu.Rdata")
  load(file = file_list.dejaVu)
  
  # Statistiques sur les utilisateurs
  file_stat.Users = paste0("./CrossValidation/", repository, "/train", train, "/stat.Users.tsv")
  stat.Users = read.table(file = file_stat.Users, header=T, sep='\t')
  
  # Statistiques sur les films
  file_stat.Movies = paste0("./CrossValidation/", repository, "/train", train, "/stat.Movies.tsv")
  stat.Movies = read.table(file = file_stat.Movies, header=T, sep='\t')
  
  
  for(model in 1:nb.Predictors.byQ){
    similarity = mat.names_predictors$similarity[model]
    nbMin.InCommon = mat.names_predictors$nbMin.InCommon[model]
    method_pred = mat.names_predictors$predicteur[model]
    
    file_mat.sim = paste0("./CrossValidation/", repository, "/train", train, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv")
    mat.sim = as.matrix(read.table(file = file_mat.sim, header=T, sep='\t'))
      
    pred = knn_user_predictions(list.Datasets, train, Qmax, mat.sim, method_pred, list.dejaVu, stat.Users, stat.Movies)
    
    for(q in 1:Qmax){
      result_error[model,q] = rmse(pred$rating, pred[,q+3])
    }
  }
  write.table(result_error, paste0("./Results/", repository, "/results_knn_userPredictionTest_train", train, ".tsv"), col.names=NA, sep="\t")
}