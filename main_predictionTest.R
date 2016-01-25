# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_predictionTest.R
#       Description : résultats des tests par validation croisée
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 

# ======================================== 2.OUVERTURE DES FICHIERS =================================

source("open_file.R")
#open_file(repository = ml-100k)

# ====================== 3.GENERATION DES BASES D'APPRENTISSAGE ET DE TEST ==========================

source("decoupage_de_la_base.R")
#split_Ratings(n=5)

# =================== 4.GENERATION DES TABLEAUX DE PREDICTION ================================

source("stat_Users.R")
source("stat_Movies.R")
source("naive_prediction.R")
source("error_function.R")

result_RMSE = as.data.frame(matrix(0, nrow=n, ncol = 5))
colnames(result_RMSE) = c("random", "meanOfUsers", "meanOfMovies", "meanByUser", "meanByMovie")

result_MAE = as.data.frame(matrix(0, nrow=n, ncol = 5))
colnames(result_MAE) = c("random", "meanOfUsers", "meanOfMovies", "meanByUser", "meanByMovie")

result_01 = as.data.frame(matrix(0, nrow=n, ncol = 5))
colnames(result_01) = c("random", "meanOfUsers", "meanOfMovies", "meanByUser", "meanByMovie")

# =================== 5.CALCUL DES TABLEAUX DE PREDICTION ================================

for(vc in 1:n){ # pour chaque couple train/test de la validation croisée
  train = get(paste0('TrainingU',vc))
  test = get(paste0('TestU',vc))
  pred = naive_prediction(train, test)
  
  for(model in 1:5){ #pour chaque prédicteur
    result_RMSE[vc,model] = error_function(pred$rating, pred[,model+3], method = 'RMSE')
  }
  
  for(model in 1:5){ #pour chaque prédicteur
    result_MAE[vc,model] = error_function(pred$rating, pred[,model+3], method = 'MAE')
  }
  
  for(model in 1:5){ #pour chaque prédicteur
    result_01[vc,model] = error_function(pred$rating, pred[,model+3], method = '01')
  }

}
