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

cat(sprintf("Les problèmes proposés sont : ml-100k\n"))
repository = readline(prompt = "Choisissez un problème : ")

# ====================== 3.CHARGEMENT DES BASES D'APPRENTISSAGE ET DE TEST ==========================

cat(sprintf("Les sous-bases proposés sont de taille : 5\n"))
nb.Datasets = as.integer(readline(prompt = "Choisissez un nombre de sous-bases : "))

file_list.Datasets = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/list.Datasets.Rdata")
load(file = file_list.Datasets)

# ============================== 4.CHOIX DE PARAMETRES =========================================

library("zoo")
library("hydroGOF")

Qmax = 20

similarity_names = c("RFP") 
list.nbMin.InCommon = c(0)
method_names = c("weighted-centered&a", "mean")

# Calcul du nombre de prédicteurs
# Noms des familles de modèles : similarité_nbMin.InCommon_predicteur

nb.similarities = length(similarity_names)
nb.nbMin.InCommon = length(list.nbMin.InCommon)
nb.predicteurs = length(method_names)

mat.models = expand.grid(similarity_names, list.nbMin.InCommon, method_names, stringsAsFactors = FALSE)
colnames(mat.models) = c("similarity", "nbMin.InCommon", "predicteur")
mat.models$name = paste0(mat.models$similarity, "_", mat.models$nbMin.InCommon, "_", 
                                    mat.models$predicteur)

nb.Models.byQ = nb.similarities * nb.nbMin.InCommon * nb.predicteurs

# ====================== 5.CHARGEMENT DES TABLEAUX DE PREDICTION ====================================

source("./NeighborhoodBasedAlgorithms/Q_nearest_neighbors.R")
source("./NeighborhoodBasedAlgorithms/knn_user_predictions.R", encoding = 'UTF-8')
source("./NeighborhoodBasedAlgorithms/knn_user_predicteur.R")

result_error = as.data.frame(matrix(0, nrow=nb.Models.byQ, ncol = Qmax))

# =================== 6.CALCUL DES TABLEAUX DE PREDICTION ================================

#TODO a changer : 1:nb.Datasets et non 1:1
for(train in 1:1){ # pour chaque couple train/test de la validation croisée
  
  cat(sprintf("\n Calcul pour la sous-base : %0.f / %0.f \n", train, nb.Datasets))
  
  # Chargement des listes déjà vus
  file_list.dejaVu = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/train", train, "/list.dejaVu.Rdata")
  load(file = file_list.dejaVu)
  
  # Statistiques sur les utilisateurs
  file_stat.Users = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/train", train, "/stat.Users.tsv")
  stat.Users = read.table(file = file_stat.Users, header=T, sep='\t')
  
  # Statistiques sur les films
  file_stat.Movies = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/train", train, "/stat.Movies.tsv")
  stat.Movies = read.table(file = file_stat.Movies, header=T, sep='\t')
  
  for(modelIND in 1:nb.Models.byQ){
    
    cat(sprintf("\n Calcul pour le modèle : %s (%0.f / %0.f) \n",mat.models$name[modelIND], modelIND, nb.Models.byQ))
    
    similarity = mat.models$similarity[modelIND]
    nbMin.InCommon = mat.models$nbMin.InCommon[modelIND]
    predicteur = mat.models$predicteur[modelIND]
    
    file_mat.sim = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/train", train, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv")
    mat.sim = as.matrix(read.table(file = file_mat.sim, header=T, sep='\t'))
      
    pred = knn_user_predictions(list.Datasets, train, Qmax, mat.sim, predicteur, list.dejaVu, stat.Users, stat.Movies)
    
    write.table(pred, paste0("./Results/", repository, "/results_predictions_train", train, "_", similarity, "_", nbMin.InCommon, "_", predicteur, ".tsv"), col.names=NA, sep="\t")
    
    for(q in 1:Qmax){
      result_error[modelIND,q] = rmse(pred$rating, pred[,q+3])
    }
  }
  write.table(result_error, paste0("./Results/", repository, "/results_knn_userPredictionTest_train", train, ".tsv"), col.names=NA, row.names = mat.models$name, sep="\t")
}