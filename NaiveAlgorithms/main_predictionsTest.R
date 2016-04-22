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

cat(sprintf("Les problèmes proposés sont : ml-100k\n"))
repository = readline(prompt = "Choisissez un problème : ")

# ====================== 3.CHARGEMENT DES BASES D'APPRENTISSAGE ET DE TEST ==========================

cat(sprintf("Les sous-bases proposés sont de taille : 5\n"))
nb.Datasets = as.integer(readline(prompt = "Choisissez un nombre de sous-bases : "))

file_list.Datasets = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/list.Datasets.Rdata")
load(file = file_list.Datasets)

# =================== 4.GENERATION DES TABLEAUX DE PREDICTION ================================

source("./Util/stat_Users.R")
source("./Util/stat_Movies.R")
source("./NaiveAlgorithms/naive_predictions.R")
#source("./Util/error_function.R")

library("zoo")
library("hydroGOF")

#error_names = c("RMSE", "MAE", "01")
#nb.Errors = length(error_names)

predictor_names = c("random_unif", "random_samp", "meanOfMovies", "meanOfUsers", "mean", "meanByUser", "meanByMovie")
nb.Predictors = length(predictor_names)

#for(error in error_names){
#  assign(paste0('result_',error),as.data.frame(matrix(0, nrow=nb.Predictors, ncol = nb.Datasets), row.names = predictor_names))
#}

result_RMSE = as.data.frame(matrix(0, nrow=nb.Predictors, ncol = nb.Datasets), row.names = predictor_names)

# =================== 5.CALCUL DES TABLEAUX DE PREDICTION ================================

for(train in 1:nb.Datasets){ # pour chaque couple train/test de la validation croisée
  pred = naive_predictions(list.Datasets, train)
  
  #for(error in error_names){ #pour chaque erreur
    #for(model in predictor_names){ #pour chaque predicteur
      #assign(paste0("result_",error),'[<-' (get(paste0("result_",error)), model,vc, value = error_function(pred$rating, pred[,model], method=error)))
      #}
  #}
  for(model in predictor_names){
    result_RMSE[model, train] = rmse(pred$rating, pred[,model])
  }

}

cross_validation = as.data.frame(matrix(0,nrow=nb.Predictors, ncol = 1)) #predicteur x metrique
colnames(cross_validation) = "RMSE"
rownames(cross_validation) = predictor_names
#for(error in error_names){
  for(model in predictor_names){
    cross_validation[model,1] = round(sum(result_RMSE[model,])/nb.Datasets, digits = 3)
  }
#}

# =================== 6.ENREGISTREMENT DES RESULTATS DE LA VC ================================

#write.table(cross_validation, paste0("./Results/", repository, "/results_naivePredictionTest.tsv"), col.names=NA, sep="\t")
