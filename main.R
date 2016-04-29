# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main.R
#       Description : fonction principal pour les recommandations
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ============================== 1.PREAMBULE ==================================================================

# Clean up
rm(list=ls()) 
cat("\014") 

# Choix du problème et de la méthode de recommandation
cat(sprintf("Les problèmes proposés sont : ml-100k\n"))
repository = readline(prompt = "Choisissez un problème : ")
cat(sprintf("Les méthodes proposées sont : naive, knn_user et svd\n"))
method = readline(prompt = "Choisissez une méthode : ")

# ============================== 2.ALGORITHME NAIF ==================================================================

if(method == "naive"){

  # Chargement de la base et des statistiques des films
  recap.Movies = read.table(file = paste0("./Results/", repository, "/recap.Movies.tsv") , header=T, sep='\t')
  
  # Chargement de la liste des films déjà notés par individu
  load(file = paste0("./Results/", repository, "/list.dejaVu.Rdata"))

  # Choix de quelques paramètres pour l'utilisateur final
  nb.recommandations = as.integer(readline(prompt = "Choisissez un nombre de recommandations : "))
  nbMin.Ratings = as.integer(readline(prompt = "Choisissez un seuil de visionnage : "))
  userID = as.integer(readline(prompt = "Choisissez un utilisateur : "))

  # Recommandation
  source("./NaiveAlgorithms/recommandation_meanByMovie.R")
  mat.RecommendedMovies = recommandation_meanByMovie(recap.Movies, list.dejaVu, userID, nb.recommandations, nbMin.Ratings)
}

# ============================== 3.ALGORITHME DES PLUS PROCHES VOISINS AU SENS UTILISATEUR ============================

if(method == "knn_user"){
  
# CHARGEMENT DES DONNEES BASIQUES
  
  # Chargement de la base des notes
  data.Ratings = read.table(file = paste0("Data/", repository, "/data.Ratings.tsv"), header=T, sep='\t')
  
  # Chargement de la base et des statistiques des films
  recap.Movies = read.table(file = paste0("./Results/", repository, "/recap.Movies.tsv"), header=T, sep='\t')
  
  # Chargement de la base et des statistiques des utilisateurs
  recap.Users = read.table(file = paste0("./Results/", repository, "/recap.Users.tsv"), header=T, sep='\t')
  
  # Chargement de la liste des films déjà notés
  load(file = paste0("./Results/", repository, "/list.dejaVu.Rdata"))
  
# CHARGEMENT DES PARAMETRES CHOISIS PAR L'OPERATEUR
  
  # choix de la métrique de similarité
  cat(sprintf("Les métriques proposées sont : pearson, nrmse, nmae et RFP (ratings-frequency pearson) \n"))
  similarity = readline(prompt = "Choisissez une métrique pour la similarité : ") 
    
  # Choix du seuil de voisinage
  cat(sprintf("Les seuils de voisinage sont 0, 2, 4, 6, 8 et 10  \n"))
  nbMin.InCommon = readline(prompt = "Choisissez un seuil de voisinage : ")
  
  # Chargement de la matrice de similarité (dépend de la métrique et du seuil)
  mat.sim = as.matrix(read.table(file = paste0("./Results/", repository, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv"), header=T, sep='\t'))
  
  # Choix du nombre de plus proches voisins
  Q = as.integer(readline(prompt = "Choisissez le nombre de plus proches voisins : "))
  
  # Choix du nombre minimal de visionnage
  nbMin.Ratings = as.integer(readline(prompt = "Choisissez un seuil de visionnage : "))
  
  # Choix du prédicteur
  cat(sprintf("Les prédicteurs proposés sont : mean, weighted, weighted-centered, weighted&a, weighted-centered&a \n"))
  predicteur = readline(prompt = "Choisissez le prédicteur : ")
  
# CHARGEMENT DES PARAMETRES CHOISIS PAR L'UTILISATEUR FINAL
  
  # Choix du nombre de recommandations
  nb.recommandations = as.integer(readline(prompt = "Choisissez un nombre de recommandations : "))
  
  # Choix de l'identifiant de l'utilisateur
  userID = as.integer(readline(prompt = "Choisissez un utilisateur : "))
  
# RECOMMANDATION
  source("./NeighborhoodBasedAlgorithms/Q_nearest_neighbors.R")
  source("./NeighborhoodBasedAlgorithms/knn_user_predicteur.R")
  source("./NeighborhoodBasedAlgorithms/knn_user_recommendation.R")
  
  mat.RecommendedMovies = knn_user_recommendation(userID, recap.Users, recap.Movies, data.Ratings, mat.sim, list.dejaVu, Q, nb.recommandations, predicteur, nbMin.Ratings)
  }

# ============================== 4.ALGORITHME PAR REDUCTION SVD ======= ============================

if(method == "svd"){
  print("Not available")
}

# ============================== 5.AFFICHAGE DES RECOMMANDATIONS ==================================================================

source("./Util/display_recommendations.R", encoding = 'UTF8')
source("./Util/genres_of_movie.R")

display_recommendations(mat.RecommendedMovies, nb.recommandations, recap.Movies)