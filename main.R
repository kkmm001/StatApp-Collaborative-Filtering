# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main.R
#       Description : fonction principal pour les recommandations
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

## Clean up
rm(list=ls()) 
cat("\014") 

# Choix du problème et de la méthode de recommandation
repository = readline(prompt = "Choisissez un problème : ") #ml-100k
method = readline(prompt = "Choisissez une méthode : ") #naive, knn_user

if(method == "naive"){

  # Chargement des données
  recap.Movies = read.table(file = paste0("./Results/", repository, "/recap.Movies.tsv"), header=T, sep='\t')
  load(file = paste0("./Results/", repository, "/list.dejaVu.Rdata"))
  source("./NaiveAlgorithms/recommandation_meanByMovie.R")

  # Choix de quelques paramètres
  nb.recommandations = as.integer(readline(prompt = "Choisissez un nombre de recommandations : "))
  seuil = as.integer(readline(prompt = "Choisissez un seuil de visionnage : "))
  userID = as.integer(readline(prompt = "Choisissez un utilisateur : "))

  # Recommandation
  vect.RecommendedMovies = recommandation_meanByMovie(recap.Movies, list.dejaVu, userID, nb.recommandations, threshold = seuil)

}

if(method == "knn_user"){
  
  # Chargement des données
  load(file = paste0("./Results/", repository, "/list.dejaVu.Rdata"))
  
  similarity = readline(prompt = "Choisissez une métrique pour la similarité : ") # "pearson", "nrmse" ou "nmae"
  if (similarity == "pearson"){
    betterIsHigh = TRUE
  } else if(similarity %in% c("nrmse", "nmae")){
    betterIsHigh = FALSE
  }
  assign(paste0("mat.sim_", similarity), as.matrix(read.table(file = paste0("./Results/", repository, "/mat.sim_", similarity, ".tsv"), header=T, sep='\t')))
  
  recap.Movies = read.table(file = paste0("./Results/", repository, "/recap.Movies.tsv"), header=T, sep='\t')
  recap.Users = read.table(file = paste0("./Results/", repository, "/recap.Users.tsv"), header=T, sep='\t')
  
  data.Ratings = read.table(file = paste0("Data/", repository, "/data.Ratings.tsv"), header=T, sep='\t')
  
  # Choix de quelques paramètres
  Q = as.integer(readline(prompt = "Choisissez le nombre de plus proches voisins : "))
  nb.recommandations = as.integer(readline(prompt = "Choisissez un nombre de recommandations : "))
  userID = as.integer(readline(prompt = "Choisissez un utilisateur : "))

  vect.Users = sort(unique(recap.Users$userID))
  vect.Movies = sort(unique(recap.Movies$movieID[recap.Movies$nb.Ratings >= Q]))
  
  #Recommandation
  source("./NeighborhoodBasedAlgorithms/Q_nearest_neighbors.R")
  source("./NeighborhoodBasedAlgorithms/recommendation_knn_user.R")
  
  userIND = which(vect.Users == userID)
  vect.similarity = vect.Users[order(get(paste0("mat.sim_", similarity))[userIND,], decreasing = betterIsHigh)]
  
  vect.RecommendedMovies = recommendation_knn_user(userID, vect.Users, vect.Movies, data.Ratings, vect.similarity, list.dejaVu)
}

cat(sprintf("Les %.0f films recommandés pour vous : \n", nb.recommandations))
for (recom in 1:nb.recommandations){
  cat(sprintf("%.0f \t %-40s \t noté %.2f/5\n", 
              recom, 
              recap.Movies$title[recap.Movies$movieID == vect.RecommendedMovies[recom]], 
              recap.Movies$mean[recap.Movies$movieID == vect.RecommendedMovies[recom]]
  )
  )
}  
