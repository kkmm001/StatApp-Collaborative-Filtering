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
seq_userID = seq(1, 943, by=15)

# Choix de quelques paramètres pour l'utilisateur final
nbMin.Ratings = 25

# choix de la métrique de similarité
similarity = "RFP"

# Choix du seuil de voisinage
nbMin.InCommon = 0

# Chargement de la matrice de similarité (dépend de la métrique et du seuil)
mat.sim = as.matrix(read.table(file = paste0("./Results/", repository, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv"), header=T, sep='\t'))

# Choix du nombre de plus proches voisins
K = 36

# Choix du prédicteur
predicteur = "weighted-centered&a"

# choix de la méthode de remplissage de la matrice des notes
howToFill = "Item"

#  Choix de la proportion d'inertie à garder
tau = 24.5

# ============================== 2.CHARGEMENT DES DONNEES BASIQUES ==================================================================

# Chargement de la base et des statistiques des films
recap.Movies = read.table(file = paste0("./Results/", repository, "/recap.Movies.tsv") , header=T, sep='\t')

# Chargement de la base et des statistiques des utilisateurs
recap.Users = read.table(file = paste0("./Results/", repository, "/recap.Users.tsv"), header=T, sep='\t')

# Chargement de la liste des films déjà notés par individu
load(file = paste0("./Results/", repository, "/list.dejaVu.Rdata"))

# ============================== 3.MATRICE DES CORRELATIONS ==================================================================

mat.Correlations = as.data.frame(matrix(NA, nrow = length(seq_userID), ncol = 7))
rownames(mat.Correlations) = seq_userID
colnames(mat.Correlations) = c("userID", "corr_naif_knn", "corr_naif_svdN", "corr_naif_svdDG", 
                               "corr_knn_svdN", "corr_knn_svdDG", "corr_svdN_svdDG")

for(user in 1:length(seq_userID)){
  userID = seq_userID[user]
  # Filtre des films ayant dépasssé un certain seuil
  vect.Recommandable = sort(unique(recap.Movies$movieID[recap.Movies$nb.Ratings >= nbMin.Ratings]))
  
  # Ensemble des films qui sont susceptibles d'être recommandés à userID
  vect.Recommandable = vect.Recommandable[!(vect.Recommandable %in% list.dejaVu[[userID]])]
  
  # Nombre de films recommandables
  nb.recommandations = length(vect.Recommandable) #tous les films
  
  # ALGORITHME NAIF
  
  cat(sprintf("\nAlgorithme naïf"))
  source("./NaiveAlgorithms/recommandation_meanByMovie.R")
  mat.RecommendedMovies_naif = recommandation_meanByMovie(recap.Movies, list.dejaVu, userID, nb.recommandations, nbMin.Ratings)
  
  # ALGORITHME DES PLUS PROCHES VOISINS AU SENS UTILISATEUR 
  
  cat(sprintf("\nAlgorithme des plus proches voisins \n"))
    
  source("./NeighborhoodBasedAlgorithms/K_nearest_neighbors.R")
  source("./NeighborhoodBasedAlgorithms/knn_user_predicteur.R")
  source("./NeighborhoodBasedAlgorithms/knn_user_recommendation.R")
  source("./Util/get_limited_value.R")
    
  mat.RecommendedMovies_knn = knn_user_recommendation(userID, recap.Users, recap.Movies, data.Ratings, mat.sim, list.dejaVu, K, nb.recommandations, predicteur, nbMin.Ratings)
  
  # ALGORITHME PAR DECOMPOSITION EN FAIBLE RANG - NAIF 
  
  cat(sprintf("\nAlgorithme par décomposition en faible rang - naif"))
  
  # Chargement de la décomposition de la matrice des notes après remplissage
  load(paste0("./Results/", repository, "/list.SVD_User.Rdata"))
  load(paste0("./Results/", repository, "/list.SVD_Item.Rdata"))
  
  source("./SVD/matUS_matSV.R")
  source("./SVD/svd_recommendation.R")
  source("./Util/get_limited_value.R")
  
  library("expm")
  
  mat.RecommendedMovies_svdN = svd_recommendation(userID, recap.Users, recap.Movies, data.Ratings, list.SVD_Item, list.SVD_User, tau,  nb.recommandations, nbMin.Ratings, howToFill, list.dejaVu)
  
  # ALGORITHME DECOMPOSITION EN FAIBLE RANG - DESCENTE DE GRADIENT 
  
  cat(sprintf("\nAlgorithme par décomposition en faible rang - descente de gradient\n"))
   
  source("./SVD/transform_Ratings.R")
  source("./SVD/DescentG.R")
  source("./SVD/svd_DG_recommendation.R")
  mat.RecommendedMovies_svdDG = svd_DG_recommendation(userID, recap.Users, recap.Movies, data.Ratings, nb.recommandations, nbMin.Ratings)
  
  # AFFICHAGE DES RECOMMANDATIONS 
  
  # rankOfMovies contient le rang de chaque film recommandés  
  rankOfMovies_naif = rank(mat.RecommendedMovies_naif[order(mat.RecommendedMovies_naif$movieID),"prating"])
  rankOfMovies_knn  = rank(mat.RecommendedMovies_knn[order(mat.RecommendedMovies_knn$movieID),"prating"])
  rankOfMovies_svdN  = rank(mat.RecommendedMovies_svdN[order(mat.RecommendedMovies_svdN$movieID),"prating"])
  rankOfMovies_svdDG  = rank(mat.RecommendedMovies_svdDG[order(mat.RecommendedMovies_svdDG$movieID),"prating"])
  
  #taux de corrélation entre les deux recommandations (corrélation de pearson entre les rangs)
  correlation_naif_knn = cor(rankOfMovies_naif, rankOfMovies_knn, method = "pearson")
  correlation_naif_svdN = cor(rankOfMovies_naif, rankOfMovies_svdN, method = "pearson")
  correlation_naif_svdDG = cor(rankOfMovies_naif, rankOfMovies_svdDG, method = "pearson")
  correlation_knn_svdN  = cor(rankOfMovies_knn, rankOfMovies_svdN, method = "pearson")
  correlation_knn_svdDG  = cor(rankOfMovies_knn, rankOfMovies_svdDG, method = "pearson")
  correlation_svdN_svdDG  = cor(rankOfMovies_svdN, rankOfMovies_svdDG, method = "pearson")
  
  source("./Util/genres_of_movie.R")
  #source("./Util/display_recommendations.R", encoding = 'UTF8')
  
  #cat(sprintf("Recommandation par la méthode naïve\n"))
  #display_recommendations(mat.RecommendedMovies_naif[1:10,], 10, recap.Movies)
  
  #cat(sprintf("Recommandation par la méthode des plus proches voisins\n"))
  #display_recommendations(mat.RecommendedMovies_knn[1:10,], 10, recap.Movies)
  
  #cat(sprintf("Recommandation par la décomposition en faible rang - naif\n"))
  #display_recommendations(mat.RecommendedMovies_svdN[1:10,], 10, recap.Movies)
  
  #cat(sprintf("Recommandation par la décomposition en faible rang - descente de gradient\n"))
  #display_recommendations(mat.RecommendedMovies_svdDG[1:10,], 10, recap.Movies)
  
  cat(sprintf("\n Taux de corrélation entre les méthodes pour l'utilisateur %.0f \n", userID))
  
  cat(sprintf("Le taux de corrélation entre naif et knn est de %.2f\n", correlation_naif_knn))
  cat(sprintf("Le taux de corrélation entre naif et svdN est de %.2f\n", correlation_naif_svdN))
  cat(sprintf("Le taux de corrélation entre naif et svdDG est de %.2f\n", correlation_naif_svdDG))
  cat(sprintf("Le taux de corrélation entre knn et svdN est de %.2f\n", correlation_knn_svdN))
  cat(sprintf("Le taux de corrélation entre knn et svdDG est de %.2f\n", correlation_knn_svdDG))
  cat(sprintf("Le taux de corrélation entre svdN et svdDG est de %.2f\n", correlation_svdN_svdDG))

  # Remplissage de la matrice de corrélation
  
  mat.Correlations$userID[user] = userID
  mat.Correlations$corr_naif_knn[user] = correlation_naif_knn
  mat.Correlations$corr_naif_svdN[user] = correlation_naif_svdN
  mat.Correlations$corr_naif_svdDG[user] = correlation_naif_svdDG
  mat.Correlations$corr_knn_svdN[user] = correlation_knn_svdN
  mat.Correlations$corr_knn_svdDG[user] = correlation_knn_svdDG
  mat.Correlations$corr_svdN_svdDG[user] = correlation_svdN_svdDG
    
}

write.table(mat.Correlations, "./Tests/mat.correlation.tsv", col.names = NA, sep='\t')
