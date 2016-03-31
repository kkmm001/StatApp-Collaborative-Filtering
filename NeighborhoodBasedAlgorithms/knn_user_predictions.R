# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =#
#       Fichier : knn_user_predictions.R                                                               #
#       Description : Fonction de predictions de plus proches voisins sur les bases                                   #
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =#

knn_user_predictions = function(train.Ratings, test.Ratings, Qmax){
  
  ## Génération des outils sur la base d'apprentissage
  # Base de données des utilisateurs et leurs statistiques
  
  cat(sprintf("Création de la base recap.Users en cours \n"))
  stat.Users = stat_Users(train.Ratings)
  recap.Users = as.data.frame(merge(data.Users, stat.Users, by.x = "userID", by.y = "userID"))
  vect.Users = sort(unique(train.Ratings$userID))
  
  # Base de données des films et leurs statistiques
  cat(sprintf("Création de la base recap.Movies en cours \n"))
  stat.Movies = stat_Movies(train.Ratings)
  recap.Movies = merge(data.Movies, stat.Movies, by.x = "movieID", by.y = "movieID")

  # Listes des films notés par utilisateur
  cat(sprintf("Création de la base list.dejaVu en cours \n"))
  list.dejaVu = deja_Vu(train.Ratings)

  # Matrice des similarités (pearson, nrmse, nmae)
  cat(sprintf("Création de la matrice de similarité en cours \n"))
  mat.sim_pearson <<- proxi_Users_AllvsAll(train.Ratings, "pearson") #variable globale 
  
  #similarity = "pearson"
  #assign(paste0("mat.sim_", similarity), as.matrix(read.table(file = paste0("./Results/", repository, "/mat.sim_", similarity, ".tsv"), header=T, sep='\t')), envir = globalenv())
  
  #mat.sim_nrmse = proxi_Users_AllvsAll(train.Ratings, "nrmse")
  #mat.sim_nmae = proxi_Users_AllvsAll(train.Ratings, "nmae")

  # Paramétres du test
  cat(sprintf("Début du test \n"))
  nb.Tests = dim(test.Ratings)[1]
  resultTest = test.Ratings
  
  cat(paste0("|", nb.Tests, "|"))
  # Prédiction
  for(test in 1:nb.Tests){
    cat(paste0("|", test, "|"))
    userID = test.Ratings$userID[test]
    movieID = test.Ratings$movieID[test]
    similarity = "pearson" #TODO boucle
    qnn = Q_nearest_neighbors(userID, movieID, Qmax, list.dejaVu, vect.Users, similarity)
    
    vect.Ratings.byNN = matrix(NA, nrow = 1, ncol = Qmax)
    nb.Ratings = recap.Movies$nb.Ratings[recap.Movies$movieID == movieID]
    for(q in 1:min(Qmax,nb.Ratings)){
      vect.Ratings.byNN[q] = train.Ratings$rating[(train.Ratings$userID == qnn[q]) & (train.Ratings$movieID == movieID)]
      
      resultTest[test, paste0(similarity, "_Q_", q)] = mean(vect.Ratings.byNN[1:q], na.rm = TRUE)
    }
    
  }
  
  return(resultTest)
  
}

