# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_predictionsTest_naive.R
#       Description : résultats des tests par validation croisée
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 

# ======================================== 2.OUVERTURE DES FICHIERS =================================

cat(sprintf("Les problèmes proposés sont : ml-100k et ml-1m\n"))
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

library("zoo")
library("hydroGOF")

predictor_names = c("random_unif", "random_samp", "meanOfMovies", "meanOfUsers", "mean", "meanByUser", "meanByMovie")
nb.Predictors = length(predictor_names)

result_RMSE_cv = as.data.frame(matrix(0, nrow=nb.Predictors, ncol = nb.Datasets+1), row.names = predictor_names)
colnames(result_RMSE_cv) = c("Test.Base.1", "Test.Base.2", "Test.Base.3", "Test.Base.4", "Test.Base.5", "moyenne")
result_RMSE_vierge = as.data.frame(matrix(0, nrow=nb.Predictors, ncol = 1), row.names = predictor_names)

# =================== 5.CALCUL DES TABLEAUX DE PREDICTION ================================

for(train in 1:nb.Datasets){ # pour chaque couple train/test de la validation croisée
  cat(sprintf("\n Calcul pour la base d'apprentissage : %.0f / %.0f", train, nb.Datasets))
  
  pred = naive_predictions(list.Datasets, train)
  
  for(model in predictor_names){
    result_RMSE_cv[model, train] = rmse(pred$rating, pred[,model])
  }
}

result_RMSE_cv$moyenne = round(rowMeans(result_RMSE_cv[,1:nb.Datasets]), digits = 3)

write.table(result_RMSE_cv, paste0("./Results/", repository, "/results_naivePredictionTest.tsv"), col.names=NA, sep="\t")


# =================== 6.CALCUL DES TABLEAUX DE PREDICTION POUR LA BASE VIERGE ================================

cat(sprintf("\n Calcul pour la base vierge"))

#Adaptation du code pour prendre en compte la base vierge
file_data.Vierge = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/data.Ratings.Vierge.Rdata")
load(file = file_data.Vierge)
train.Ratings = do.call("rbind", list.Datasets)
test.Ratings=data.Ratings.Vierge
list.Datasets=list(test.Ratings,train.Ratings)

# Chargement des listes déjà vus
file_list.dejaVu = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/vierge/list.dejaVu.Rdata")
load(file = file_list.dejaVu)

# Statistiques sur les films
file_stat.Movies = paste0("./CrossValidation/", repository, "/CV", nb.Datasets, "/vierge/stat.Movies.tsv")
stat.Movies = read.table(file = file_stat.Movies, header=T, sep='\t')

pred = naive_predictions(list.Datasets, 1) #1 désigne la base vierge
write.table(pred, paste0("./Results/", repository, "/results_predictions_vierge_naive.tsv"), col.names=NA, sep="\t")
  
for(model in predictor_names){
  result_RMSE_vierge[model,1] = rmse(pred$rating, pred[,model])
}

write.table(result_RMSE_vierge, paste0("./Results/", repository, "/results_naivePredictionTest_vierge.tsv"), col.names=NA, row.names = predictor_names, sep="\t")
