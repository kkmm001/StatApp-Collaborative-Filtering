# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_test_correlation.R
#       Description : test la correlation des recommandations entre les méthodes
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ============================== 1.PREAMBULE ==================================================================

# Clean up
rm(list=ls()) 
cat("\014") 

# Choix du problème et chargement de data.Ratings, data.Movies et data.Users 
source("./Util/open_files.R", encoding = 'UTF-8')

# Choix de l'utilisateur
userID = as.integer(readline(prompt = "Choisissez un utilisateur : "))

# Choix de quelques paramètres pour l'utilisateur final
nbMin.Ratings = as.integer(readline(prompt = "Choisissez un seuil de visionnage : "))

# ============================== 2.CHARGEMENT DES DONNEES BASIQUES ==================================================================

# Chargement de la base et des statistiques des films
recap.Movies = read.table(file = paste0("./Results/", repository, "/recap.Movies.tsv") , header=T, sep='\t')

# Chargement de la base et des statistiques des utilisateurs
recap.Users = read.table(file = paste0("./Results/", repository, "/recap.Users.tsv"), header=T, sep='\t')

# Chargement de la liste des films déjà notés par individu
load(file = paste0("./Results/", repository, "/list.dejaVu.Rdata"))

# Nombre de films recommandables
nb.recommandations = dim(recap.Movies)[1]- length(list.dejaVu[[userID]]) #tous les films

# ============================== 2.ALGORITHME NAIF ==================================================================

cat(sprintf("Algorithme naïf \n"))
source("./NaiveAlgorithms/recommandation_meanByMovie.R")
mat.RecommendedMovies_naif = recommandation_meanByMovie(recap.Movies, list.dejaVu, userID, nb.recommandations, nbMin.Ratings)

# ============================== 3.ALGORITHME DES PLUS PROCHES VOISINS AU SENS UTILISATEUR ============================

cat(sprintf("Algorithme des plus proches voisins \n"))

# choix de la métrique de similarité
cat(sprintf("Les métriques proposées sont : pearson, nrmse, nmae et RFP (ratings-frequency pearson) \n"))
similarity = readline(prompt = "Choisissez une métrique pour la similarité : ") # "pearson", "nrmse" ou "nmae"
  
# Choix du seuil de voisinage
cat(sprintf("Les seuils de voisinage sont 0, 2, 4, 6, 8 et 10  \n"))
nbMin.InCommon = readline(prompt = "Choisissez un seuil de voisinage : ")
  
# Chargement de la matrice de similarité (dépend de la métrique et du seuil)
mat.sim = as.matrix(read.table(file = paste0("./Results/", repository, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv"), header=T, sep='\t'))
  
# Choix du nombre de plus proches voisins
K = as.integer(readline(prompt = "Choisissez le nombre de plus proches voisins : "))
  
# Choix du prédicteur
cat(sprintf("Les prédicteurs proposés sont : mean, weighted, weighted-centered, weighted&a, weighted-centered&a \n"))
predicteur = readline(prompt = "Choisissez le prédicteur : ")
  
source("./NeighborhoodBasedAlgorithms/K_nearest_neighbors.R")
source("./NeighborhoodBasedAlgorithms/knn_user_predicteur.R")
source("./NeighborhoodBasedAlgorithms/knn_user_recommendation.R")
source("./Util/get_limited_value.R")
  
mat.RecommendedMovies_knn = knn_user_recommendation(userID, recap.Users, recap.Movies, data.Ratings, mat.sim, list.dejaVu, K, nb.recommandations, predicteur, nbMin.Ratings)

# ============================== 3.ALGORITHME PAR DECOMPOSITION EN FAIBLE RANG ============================

# choix de la méthode de remplissage de la matrice des notes
cat(sprintf("\n Les méthodes proposées pour remplir la matrice des notes sont : Item ou User \n"))
howToFill = readline(prompt = "Choisissez une méthode de remplissage de la matrice : ")

# Chargement de la décomposition de la matrice des notes après remplissage
load(paste0("./Results/", repository, "/list.SVD_User.Rdata"))
load(paste0("./Results/", repository, "/list.SVD_Item.Rdata"))

#  Choix de la proportion d'inertie à garder
tau = as.numeric(readline(prompt = "Choisissez une proportion d'inertie à garder (entre 0 et 100) : "))

source("./SVD/matUS_matSV.R")
source("./SVD/svd_recommendation.R")

library("expm")

mat.RecommendedMovies_svd = svd_recommendation(userID, recap.Users, recap.Movies, data.Ratings, list.SVD_Item, list.SVD_User, tau,  nb.recommandations, nbMin.Ratings, howToFill, list.dejaVu)

# ============================== 4.AFFICHAGE DES RECOMMANDATIONS ==================================================================

#taux de corrélation entre les deux recommandations (corrélation de pearson entre les rangs)
rankOfMovies_naif = rank(mat.RecommendedMovies_naif[order(mat.RecommendedMovies_naif$movieID),"prating"])
rankOfMovies_knn  = rank(mat.RecommendedMovies_knn[order(mat.RecommendedMovies_knn$movieID),"prating"])
rankOfMovies_svd  = rank(mat.RecommendedMovies_svd[order(mat.RecommendedMovies_svd$movieID),"prating"])

correlation_naif_knn = cor(rankOfMovies_naif, rankOfMovies_knn, method = "pearson")
correlation_naif_svd = cor(rankOfMovies_naif, rankOfMovies_svd, method = "pearson")
correlation_knn_svd  = cor(rankOfMovies_knn, rankOfMovies_svd, method = "pearson")

source("./Util/genres_of_movie.R")
source("./Util/display_recommendations.R", encoding = 'UTF8')

cat(sprintf("Recommandation par la méthode naïve\n"))
display_recommendations(mat.RecommendedMovies_naif[1:10,], 10, recap.Movies)

cat(sprintf("Recommandation par la méthode des plus proches voisins\n"))
display_recommendations(mat.RecommendedMovies_knn[1:10,], 10, recap.Movies)

cat(sprintf("Recommandation par la décomposition en faible rang\n"))
display_recommendations(mat.RecommendedMovies_svd[1:10,], 10, recap.Movies)

cat(sprintf("\n Taux de corrélation entre les méthodes\n"))

cat(sprintf("Le taux de corrélation entre naif et knn est de %.2f\n", correlation_naif_knn))
cat(sprintf("Le taux de corrélation entre naif et svd est de %.2f\n", correlation_naif_svd))
cat(sprintf("Le taux de corrélation entre knn et svd est de %.2f\n", correlation_knn_svd))
