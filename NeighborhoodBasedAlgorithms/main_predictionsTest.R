# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_predictionsTest2.R
#       Description : fonction principal pour les recommandations
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ======================================== 1.PREAMBULE =============================================

## Clean up
rm(list=ls()) 
cat("\014") 

source("./Util/open_files.R")
# ========================== 2.SOUS-PREPARATION DE L'ALGORITHME KNN USER USER ===========================

load(file = paste0("./Results/", repository, "/list.dejaVu.Rdata"))

similarity = "pearson"
if (similarity == "pearson"){
  betterIsHigh = TRUE
} else if(similarity %in% c("nrmse", "nmae")){
  betterIsHigh = FALSE
}

assign(paste0("mat.sim_", similarity), as.matrix(read.table(file = paste0("./Results/", repository, "/mat.sim_", similarity, ".tsv"), header=T, sep='\t')))

# ======================= 3.RECOMMANDATION VIA ALGORITHME KNN USER USER ===========================

source("./NeighborhoodBasedAlgorithms/Q_nearest_neighbors.R")
recap.Movies = read.table(file = paste0("./Results/", repository, "/recap.Movies.tsv"), header=T, sep='\t')

vect.Users = sort(unique(data.Ratings$userID))
nb.Users = 5 #length(vect.Users)

start.time <- Sys.time()

Qmax = 40

source("./Util/error_function.R")
mat.error = matrix(NA, nrow = nb.Users, ncol = Qmax)

for(userIND in 1:nb.Users){
  userID = vect.Users[userIND]
  vect.similarity = vect.Users[order(get(paste0("mat.sim_", similarity))[userIND,], decreasing = betterIsHigh)]
  nb.dejaVu = length(list.dejaVu[[userID]])
  
  mat.pronostic = matrix(NA, nrow = nb.dejaVu, ncol = Qmax)
  vect.realRating = matrix(NA, nrow = nb.dejaVu, ncol = 1)
  
  for (movieIND in 1:nb.dejaVu){
    
    movieID = list.dejaVu[[userID]][movieIND]
    qnn = Q_nearest_neighbors(list.dejaVu,vect.similarity,movieID,Qmax) #Attention aux NA !! => is.na dans le calcul de la moyenne
      
    # Pronostic
      
    vect.Ratings.byNN = matrix(NA, nrow = 1, ncol = Qmax)
    for(q in 1:Qmax){
      vect.Ratings.byNN[q] = data.Ratings$rating[(data.Ratings$userID == qnn[q]) & (data.Ratings$movieID == movieID)]
      mat.pronostic[movieIND,q] = mean(vect.Ratings.byNN[1:q], na.rm = TRUE)
    }
      
    vect.realRating[movieIND] = data.Ratings$rating[(data.Ratings$userID == userID) & (data.Ratings$movieID == movieID)] 
      
    plot(1:Qmax,mat.pronostic[movieIND,], type ="l", 
         ylim = c(0,5), 
         main = paste("pronostic pour l'utilisateur", userID, "et le film", movieID), 
         xlab = "nombre de plus proches voisins", 
         ylab = "pronostic"
    )
    abline(vect.realRating[movieIND], 0, col = "red")
  }
  
  for (q in 1:Qmax){
    mat.error[userIND,q] = error_function(mat.pronostic[,q], c(vect.realRating), "RMSE")
  }
  
  png(paste('./Results/ml-100k/KNN_', similarity, 'error_user', userID, '.png'),width = 600, height = 300)
  plot(1:Qmax,mat.error[userIND,],
       type ="l", 
       col="green", 
       main = paste("Erreur pour l'utilisateur", userID), 
       xlab = "nombre de plus proches voisins", 
       ylab = "error", 
       ylim = c(0,2)
  )
  dev.off()
}

vect.nb.Ratings = matrix(NA, nrow = nb.Users, ncol = 1)
for(userIND in 1:nb.Users){
  userID = vect.Users[userIND]
  vect.nb.Ratings[userIND] = length(list.dejaVu[[userID]])
}

vect.error = matrix(NA, nrow = 1, ncol = Qmax)
for (q in 1:Qmax){
  vect.error[q] = (mat.error[,q] %*% vect.nb.Ratings[1:nb.Users])/sum(vect.nb.Ratings[1:nb.Users])
}

png(paste('./Results/ml-100k/KNN _', similarity, '_ global error sur', nb.Users, 'utilisateurs .png'),width = 600, height = 300)
plot(1:Qmax,vect.error, 
     type ="s", 
     col="blue", 
     main = "Erreur globale", 
     xlab = "nombre de plus proches voisins", 
     ylab = "error"
     )
dev.off()

end.time <- Sys.time() 
time.taken <- end.time - start.time
time.taken
