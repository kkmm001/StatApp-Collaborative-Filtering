# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_predictionsTest.R
#       Description : résultats des tests par validation croisée
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 

# ======================================== 2.OUVERTURE DES FICHIERS =================================

source("./Util/open_files.R")

# ====================== 3.GENERATION DES BASES D'APPRENTISSAGE ET DE TEST ==========================

source("./Util/split_data.R")

# ====================== 4.GENERATION DES TABLEAUX DE PREDICTION ====================================

source("./Util/deja_Vu.R")
source("./Util/stat_Users.R")
source("./Util/stat_Movies.R")

source("./NeighborhoodBasedAlgorithms/proxi_Users.R")
source("./NeighborhoodBasedAlgorithms/proxi_Users_AllvsAll.R")
source("./NeighborhoodBasedAlgorithms/Q_nearest_neighbors.R")
source("./NeighborhoodBasedAlgorithms/knn_user_predictions.R")

source("./Util/error_function.R")

error_names = c("RMSE", "MAE", "01")
nb.Errors = length(error_names)

Qmax = 20
similarity_names = c("pearson") #TODO a rallonger
nb.Similarity = length(similarity_names)

predictor_names = paste0(similarity_names, "_Q_", 1:Qmax) #TODO a améliorer
nb.Predictors = length(predictor_names)

for(error in error_names){
  assign(paste0('result_',error),as.data.frame(matrix(0, nrow=nb.Predictors, ncol = n), row.names = predictor_names))
}

# =================== 5.CALCUL DES TABLEAUX DE PREDICTION ================================

#TODO a changer : 1:n et non 1:1
for(vc in 1:1){ # pour chaque couple train/test de la validation croisée
  pred = knn_user_predictions(get(paste0('TrainingU',vc)), get(paste0('TestU',vc)), Qmax)
  
  for(error in error_names){ #pour chaque erreur
    for(model in predictor_names){ #pour chaque predicteur
      assign(paste0("result_",error),'[<-' (get(paste0("result_",error)), model,vc, value = error_function(pred$rating, pred[,model], method=error)))
    }
  }
  
}

cross_validation = as.data.frame(matrix(0,nrow=nb.Predictors, ncol = nb.Errors)) #predicteur x metrique
colnames(cross_validation) = error_names
rownames(cross_validation) = predictor_names
for(error in error_names){
  for(model in predictor_names){
    cross_validation[model,error] = round(sum(get(paste0('result_',error))[model,])/n, digits = 3)
  }
}


# =================== 6.ENREGISTREMENT DES RESULTATS DE LA VC ================================

write.table(cross_validation, paste0("./Results/", repository, "/results_knn_userPredictionTest.tsv"), col.names=NA, sep="\t")
