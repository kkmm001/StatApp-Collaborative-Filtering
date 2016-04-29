# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_test_robust.R
#       Description : test la robustesse des recommandations
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ============================== 1.PREAMBULE ==================================================================

# Clean up
rm(list=ls()) 
cat("\014") 

# Choix du problème et chargement de data.Ratings, data.Movies et data.Users 
source("./Util/open_files.R", encoding = 'UTF-8')

# Choix de la méthode de recommandation
cat(sprintf("Les méthodes proposées sont : naive et knn_user\n"))
method = readline(prompt = "Choisissez une méthode : ")

# ============================== 2.ALGORITHME NAIF ==================================================================

if(method == "naive"){
  
  # Chargement de la base et des statistiques des films
  recap.Movies = read.table(file = paste0("./Results/", repository, "/recap.Movies.tsv") , header=T, sep='\t')
  
  # Chargement de la liste des films déjà notés par individu
  load(file = paste0("./Results/", repository, "/list.dejaVu.Rdata"))
  
  # Nombre de films recommandables
  nb.recommandations = dim(recap.Movies)[1]- length(list.dejaVu[[userID]])
  
  # Choix de quelques paramètres pour l'utilisateur final
  nbMin.Ratings = as.integer(readline(prompt = "Choisissez un seuil de visionnage : "))
  userID = as.integer(readline(prompt = "Choisissez un utilisateur : "))
  
  # Recommandation
  source("./NaiveAlgorithms/recommandation_meanByMovie.R")
  mat.RecommendedMovies1 = recommandation_meanByMovie(recap.Movies, list.dejaVu, userID, nb.recommandations, nbMin.Ratings)
  
  # Changement de quelques notes
  taux = as.integer(readline(prompt = "Choisissez un taux de changement (entre 0 et 100) : "))
  
  source("./Tests/modify_data.Ratings.R")
  data.Ratings_modified = modify_data.Ratings(data.Ratings, userID, taux, list.dejaVu)
  
  source("./Util/stat_Movies.R")
  stat.Movies_modified = stat_Movies(data.Ratings_modified)
  recap.Movies_modified = merge(data.Movies, stat.Movies_modified, by.x = "movieID", by.y = "movieID")

  mat.RecommendedMovies2 = recommandation_meanByMovie(recap.Movies_modified, list.dejaVu, userID, nb.recommandations, nbMin.Ratings)
}

# ============================== 3.ALGORITHME DES PLUS PROCHES VOISINS AU SENS UTILISATEUR ============================

if(method == "knn_user"){
  
  # Chargement de la base et des statistiques des films
  recap.Movies = read.table(file = paste0("./Results/", repository, "/recap.Movies.tsv"), header=T, sep='\t')
  
  # Chargement de la base et des statistiques des utilisateurs
  recap.Users = read.table(file = paste0("./Results/", repository, "/recap.Users.tsv"), header=T, sep='\t')
  
  # Chargement de la liste des films déjà notés
  load(file = paste0("./Results/", repository, "/list.dejaVu.Rdata"))
  
  # CHARGEMENT DES PARAMETRES CHOISIS PAR L'OPERATEUR
  
  # choix de la métrique de similarité
  cat(sprintf("Les métriques proposées sont : pearson, nrmse, nmae et RFP (ratings-frequency pearson) \n"))
  similarity = readline(prompt = "Choisissez une métrique pour la similarité : ") # "pearson", "nrmse" ou "nmae"
  
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
  
  # Choix de l'identifiant de l'utilisateur
  userID = as.integer(readline(prompt = "Choisissez un utilisateur : "))
  
  # Nombre de films recommandables
  nb.recommandations = sum(!(recap.Movies$movieID %in% list.dejaVu[[userID]]) & recap.Movies$nb.Ratings >= nbMin.Ratings)
  
  # RECOMMANDATION
  source("./NeighborhoodBasedAlgorithms/Q_nearest_neighbors.R")
  source("./NeighborhoodBasedAlgorithms/knn_user_predicteur.R")
  source("./NeighborhoodBasedAlgorithms/knn_user_recommendation.R")
  
  mat.RecommendedMovies1 = knn_user_recommendation(userID, recap.Users, recap.Movies, data.Ratings, mat.sim, list.dejaVu, Q, nb.recommandations, predicteur, nbMin.Ratings)
  
  # Changement de quelques notes
  taux = as.integer(readline(prompt = "Choisissez un taux de changement (entre 0 et 100) : "))
  
  source("./Tests/modify_data.Ratings.R")
  data.Ratings_modified = modify_data.Ratings(data.Ratings, userID, taux, list.dejaVu)
  
  source("./Util/stat_Users.R")
  stat.Users_modified = stat_Users(data.Ratings_modified)
  recap.Users_modified = as.data.frame(merge(data.Users, stat.Users_modified, by.x = "userID", by.y = "userID"))
  
  source("./Util/stat_Movies.R")
  stat.Movies_modified = stat_Movies(data.Ratings_modified)
  recap.Movies_modified = merge(data.Movies, stat.Movies_modified, by.x = "movieID", by.y = "movieID")
  
  source("./Tests/modify_mat.sim.R")
  source("./NeighborhoodBasedAlgorithms/proxi_Users.R")
  source("./NeighborhoodBasedAlgorithms/proxi_Users_AllvsAll.R")
  source("./NeighborhoodBasedAlgorithms/filtrer_similarite.R")
  mat.InCommon = as.matrix(read.table(file = paste0("./Results/", repository, "/mat.InCommon.tsv"), header=T, sep='\t'))
  mat.sim_modified = modify_mat.sim(mat.sim, data.Ratings_modified, userID, similarity, mat.InCommon, nbMin.InCommon)
  
  mat.RecommendedMovies2 = knn_user_recommendation(userID, recap.Users_modified, recap.Movies_modified, data.Ratings_modified, mat.sim_modified, list.dejaVu, Q, nb.recommandations, predicteur, nbMin.Ratings)
  
}

# ============================== 4.AFFICHAGE DES RECOMMANDATIONS ==================================================================

#taux de corrélation entre les deux recommandations (corrélation de pearson entre les rangs)
rankOfMovies1 = rank(mat.RecommendedMovies1[order(mat.RecommendedMovies1$movieID),"prating"])
rankOfMovies2 = rank(mat.RecommendedMovies2[order(mat.RecommendedMovies2$movieID),"prating"])
correlation = cor(rankOfMovies1, rankOfMovies2, method = "pearson")

source("./Util/display_recommendations.R", encoding = 'UTF8')

display_recommendations(mat.RecommendedMovies1[1:10,], 10, recap.Movies)
display_recommendations(mat.RecommendedMovies2[1:10,], 10, recap.Movies)

cat(sprintf("Le taux de corrélation est de %.2f", correlation))
