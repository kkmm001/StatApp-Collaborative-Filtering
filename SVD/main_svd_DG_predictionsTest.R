# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquÃ©e
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_svd_predictionsTest_DG.R
#       Description : rÃ©sultats des tests par validation croisÃ©e
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

## Clean up
rm(list=ls()) 
cat("\014") 


# ======================================== 2.OUVERTURE DES FICHIERS =================================

repository = readline(prompt = "Choisissez un problÃ¨me : ") #ml-100k
data.Ratings = read.table(file = paste0("Data/", repository, "/data.Ratings.tsv"), header=T, sep='\t')

# ====================== 3.CHARGEMENT DES BASES D'APPRENTISSAGE ET DE TEST ==========================

nb.Tests = as.integer(readline(prompt = "Choisissez un nombre de sous-bases : "))

file_list.Datasets = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/list.Datasets.Rdata")
load(file = file_list.Datasets)

# ====================== 2.CHARGEMENT DES FONCTIONS A UTILISER ====================================

source("./Util/stat_Movies.R")
source("./Util/stat_Users.R")
source("./SVD/transform_Ratings.R")
source("./SVD/DescentG.R")

#install.packages("hydroGOF")
#library("Metrics")
library("zoo")
library("hydroGOF")


# ======================================== 3. PHASE DE PREPARATION =================================

iteration.times = as.integer(readline(prompt = "Choisissez un nombre d’iterations: "))

seq_lambda = seq(5,40, by=5)
result_RMSE = matrix(0, nrow = length(seq_lambda), ncol = nb.Tests)
vector.userID = sort(as.numeric(unique(data.Ratings$userID)))
vector.movieID = sort(as.numeric(unique(data.Ratings$movieID)))

for(testID in 2:5){
  cat(paste("BASE TEST N°: ", testID,"\n" ))
  dataset_to_keep = (1:nb.Tests)[(1:nb.Tests) != testID]
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  test.Ratings=list.Datasets[[testID]]
  matrix.training = transform.data.rating(train.Ratings, vector.userID, vector.movieID)
  for(INDL in 1:length(seq_lambda)){
    L=seq_lambda[INDL]
    cat(paste("\n","\n"))
    cat(paste("lambda :=", L,"\n"))
    cat(paste("Creation de mat avec la method DG \n"))
    #matrix.prevision = descentG(matrix.training, iteration.times, L)
    matrix.prevision = proximalG(matrix.training, iteration.times, L)
    
    pred = restablish.data.rating.col(matrix.prevision, vector.userID, vector.movieID)
    #write.csv2(pred, paste0("svd_DG.iteration.times.", iteration.times, ".L.", L , "_pred_train", testID, ".csv"), col.names = NA)
    result_RMSE[INDL,testID] = svd.error.test(pred, test.Ratings)
  }
}

result=as.data.frame(result_RMSE)
rownames(result)=seq_lambda
colnames(result)=1:nb.Tests
write.csv2(result, paste0("./Results/Results.RMSE.SVD.DG. iteration.times ", iteration.times ,".csv"))




###################TEST SUR base vierge ##################

seq_lambda = seq(6, 16, by=2)
result_RMSE2 = matrix(0, nrow = length(seq_lambda), ncol = 1)
vector.userID = sort(as.numeric(unique(data.Ratings$userID)))
vector.movieID = sort(as.numeric(unique(data.Ratings$movieID)))

data.Ratings.Vierge = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/data.Ratings.Vierge.Rdata")
load(file = data.Ratings.Vierge)

train.Ratings = do.call("rbind", list.Datasets)
test.Ratings=data.Ratings.Vierge
matrix.training = transform.data.rating(  train.Ratings, vector.userID, vector.movieID)




for(INDL in 1:length(seq_lambda)){
    L_opt=seq_lambda[INDL]
    cat(paste("\n","\n"))
    cat(paste("lambda :=", L_opt,"\n"))
    cat(paste("Creation de mat avec la method DG \n"))
    #matrix.prevision = descentG(matrix.training, iteration.times, L_opt)
    matrix.prevision = proximalG(matrix.training, iteration.times, L_opt)
    pred = restablish.data.rating.col(matrix.prevision, vector.userID, vector.movieID)
    #write.csv2(pred, paste0("svd.DG_pred_sur_base_vierge.iteration.times.", iteration.times, ".L.",L_opt,".csv"), col.names = NA)
    result_RMSE2[INDL,1] = svd.error.test(pred, test.Ratings)
}
Result2=as.data.frame(result_RMSE2)
rownames(Result2)=seq_lambda
write.csv2(Result2,paste0("./Results/Results.RMSE.SVD.DG.pred_sur_base_vierge.Iteration_times", iteration.times ,".csv"))

