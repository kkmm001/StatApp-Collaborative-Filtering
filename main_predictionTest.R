# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquÃ©e
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_predictionTest.R
#       Description : rÃ©sultats des tests par validation croisÃ©e
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

error_names = c("RMSE", "MAE", "01")
nb.Errors = length(error_names)
predictor_names = c("random", "meanOfUsers", "meanOfMovies", "meanByUser", "meanByMovie")
nb.Predictors = length(predictor_names)

for(error in error_names){
  assign(paste0('result_',error),as.data.frame(matrix(0, nrow=n, ncol = nb.Predictors), row.names = predictor_names))
}

# =================== 5.CALCUL DES TABLEAUX DE PREDICTION ================================

for(vc in 1:n){ # pour chaque couple train/test de la validation croisÃ©e
  pred = naive_prediction(get(paste0('TrainingU',vc)), get(paste0('TestU',vc)))
  
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
    cross_validation[model,error] = sum(get(paste0('result_',error))[model,])/n
  }
}
