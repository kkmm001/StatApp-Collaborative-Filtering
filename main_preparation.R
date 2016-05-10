# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_preparation.R
#       Description : fonction principal pour les générer les bases de travail et de test
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ======================================== 1.PREAMBULE ============================================================

## Clean up
rm(list=ls()) 
cat("\014") 

source("./Util/open_files.R", encoding = 'UTF-8')
source("./Util/stat_Users.R")
source("./Util/stat_Movies.R")
source("./Util/deja_Vu.R")

source("./NeighborhoodBasedAlgorithms/nb_MoviesInCommon.R")
source("./NeighborhoodBasedAlgorithms/proxi_Users.R")
source("./NeighborhoodBasedAlgorithms/proxi_Users_AllvsAll.R")
source("./NeighborhoodBasedAlgorithms/filtrer_similarite.R")

source("./SVD/svd_filledMatrix.R")

# ================================== 2.CHOIX DES PARAMETRES ===========================================================

# Pour la partie recommandation et validation-croisée
list.Similarities = c("pearson", "RFP")
list.nbMin.InCommon = c()

# Pour la partie validation-croisée (il faut que la base de validation-croisée soit créée)
nb.Tests = 5

# ================================== 2.PREPARATION DES FICHIERS POUR LES RECOMMANDATIONS ==============================

cat("Préparation des fichiers pour les recommandations \n")

# Base de données des utilisateurs et leurs statistiques
cat(" \t Création des bases de données des utilisateurs et leurs statistiques \n")
stat.Users = stat_Users(data.Ratings)
recap.Users = as.data.frame(merge(data.Users, stat.Users, by.x = "userID", by.y = "userID"))
write.table(recap.Users, paste0("./Results/", repository, "/recap.Users.tsv"), row.names = FALSE, sep="\t")

# Base de données des films et leurs statistiques
cat(" \t Création des bases de données des films et leurs statistiques \n")
stat.Movies = stat_Movies(data.Ratings)
recap.Movies = merge(data.Movies, stat.Movies, by.x = "movieID", by.y = "movieID")
write.table(recap.Movies, paste0("./Results/", repository, "/recap.Movies.tsv"), row.names = FALSE, sep="\t")

# Listes des films notés par utilisateur
cat(" \t Création des listes des films notés par utilisateur \n")
list.dejaVu = deja_Vu(data.Ratings)
save(list.dejaVu, file = paste0("Results/", repository, "/list.dejaVu.Rdata"))

# Matrice du nombre de films notés en commun
#cat("\t Création de la matrice du nombre de films notés en commun \n")
#mat.InCommon = nb_MoviesInCommon(data.Ratings)
#write.table(mat.InCommon, paste0("./Results/", repository, "/mat.InCommon.tsv"), row.names = FALSE, sep="\t")

# Matrice des similarités
#cat(" \n \t Création des matrices des similarités \n")
#for(similarity in list.Similarities){
#  cat(sprintf("\n \t \t Création de la matrice %s sans filtre\n", similarity))
#  mat.sim0 = proxi_Users_AllvsAll(data.Ratings, similarity)
#  write.table(mat.sim0, paste0("./Results/", repository, "/mat.sim_", similarity, "_0.tsv"), row.names = FALSE, sep="\t")
#  for(nbMin.InCommon in list.nbMin.InCommon){
#    cat(sprintf("\n \t \t Création de la matrice %s avec un seuil à %0.f \n", similarity, nbMin.InCommon))
#    mat.sim_filtre = filtrer_similarite(mat.sim0, mat.InCommon, nbMin.InCommon)
#    write.table(mat.sim_filtre, paste0("./Results/", repository, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv"), row.names = FALSE, sep="\t")
#  } 
#}

# Décomposition de la matrice des notes remplie de façon naïve
cat("\t Décomposition de la matrice des notes remplies en fonction de la moyenne des utilisateurs ou des films \n")
list.SVD_Item = svd_filledMatrix("Item",data.Ratings, recap.Users, recap.Movies)
list.SVD_User = svd_filledMatrix("User",data.Ratings, recap.Users, recap.Movies)

save(list.SVD_User, file = paste0("./Results/", repository, "/list.SVD_User.Rdata"))
save(list.SVD_Item, file = paste0("./Results/", repository, "/list.SVD_Item.Rdata"))

# =============================== 3.PREPARATION DES FICHIERS POUR LA VALIDATION CROISEE ===========================

cat("Préparation des fichiers pour la validation croisée \n")

file_list.Datasets = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/list.Datasets.Rdata")
load(file = file_list.Datasets)

for(train in 1:nb.Tests){
    
  cat(sprintf("Début de la préparation : %0.f / %0.f \n", train, nb.Tests))
  
  dataset_to_keep = (1:nb.Tests)[(1:nb.Tests) != train]
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])

  cat("\t Création des bases de données des utilisateurs et leurs statistiques \n")
  stat.Users = stat_Users(train.Ratings)
  write.table(stat.Users, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/stat.Users.tsv"), row.names = FALSE, sep="\t")
  
  cat(" \t Création des bases de données des films et leurs statistiques \n")
  stat.Movies = stat_Movies(train.Ratings)
  write.table(stat.Movies, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/stat.Movies.tsv"), row.names = FALSE, sep="\t")
  
  cat(" \t Création des listes des films notés par utilisateur \n")
  list.dejaVu = deja_Vu(train.Ratings)
  save(list.dejaVu, file = paste0("CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/list.dejaVu.Rdata"))
  
#  cat("\t Création de la matrice du nombre de films notés en commun \n")
#  mat.InCommon = nb_MoviesInCommon(train.Ratings)
#  write.table(mat.InCommon, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/mat.InCommon.tsv"), row.names = FALSE, sep="\t")

#  cat(" \n \t Création des matrices des similarités \n")
#  for(similarity in list.Similarities){
#    cat(sprintf("\n \t \t Création de la matrice %s sans filtre\n", similarity))
#    mat.sim0 = proxi_Users_AllvsAll(train.Ratings, similarity)
#    write.table(mat.sim0, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/mat.sim_", similarity, "_0.tsv"), row.names = FALSE, sep="\t")
    
#    for(nbMin.InCommon in list.nbMin.InCommon){
#      cat(sprintf("\n \t \t Création de la matrice %s avec un seuil à %0.f \n", similarity, nbMin.InCommon))
#      mat.sim_filtre = filtrer_similarite(mat.sim0, mat.InCommon, nbMin.InCommon)
#      write.table(mat.sim_filtre, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv"), row.names = FALSE, sep="\t")
#    } 
#  }
  
  cat(" \t Décomposition de la matrice des notes remplie en fonction de la moyenne des utilisateurs ou des films \n")
  list.SVD_Item = svd_filledMatrix("Item",train.Ratings, stat.Users, stat.Movies)
  list.SVD_User = svd_filledMatrix("User",train.Ratings, stat.Users, stat.Movies)
  
  save(list.SVD_User, file = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/list.SVD_User.Rdata"))
  save(list.SVD_Item, file = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/list.SVD_Item.Rdata"))
  
}

# =============================== 4.PREPARATION DES FICHIERS POUR LE BASE VIERGE ===========================

cat("Préparation des fichiers pour la base vierge\n")

file_list.Datasets = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/list.Datasets.Rdata")
load(file = file_list.Datasets)

train.Ratings = do.call("rbind", list.Datasets[1:nb.Tests])
  
cat("\t Création des bases de données des utilisateurs et leurs statistiques \n")
stat.Users = stat_Users(train.Ratings)
write.table(stat.Users, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/vierge/stat.Users.tsv"), row.names = FALSE, sep="\t")
  
cat(" \t Création des bases de données des films et leurs statistiques \n")
stat.Movies = stat_Movies(train.Ratings)
write.table(stat.Movies, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/vierge/stat.Movies.tsv"), row.names = FALSE, sep="\t")
  
cat(" \t Création des listes des films notés par utilisateur \n")
list.dejaVu = deja_Vu(train.Ratings)
save(list.dejaVu, file = paste0("CrossValidation/", repository, "/CV", nb.Tests, "/vierge/list.dejaVu.Rdata"))
  
#cat("\t Création de la matrice du nombre de films notés en commun \n")
#mat.InCommon = nb_MoviesInCommon(train.Ratings)
#write.table(mat.InCommon, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/vierge/mat.InCommon.tsv"), #row.names = FALSE, sep="\t")
  
#cat(" \n \t Création des matrices des similarités \n")
#for(similarity in list.Similarities){
#  cat(sprintf("\n \t \t Création de la matrice %s sans filtre\n", similarity))
#  mat.sim0 = proxi_Users_AllvsAll(train.Ratings, similarity)
#  write.table(mat.sim0, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/vierge/mat.sim_", similarity, "_0.tsv"), row.names = FALSE, sep="\t")
    
#  for(nbMin.InCommon in list.nbMin.InCommon){
#    cat(sprintf("\n \t \t Création de la matrice %s avec un seuil à %0.f \n", similarity, nbMin.InCommon))
#    mat.sim_filtre = filtrer_similarite(mat.sim0, mat.InCommon, nbMin.InCommon)
#    write.table(mat.sim_filtre, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/vierge/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv"), row.names = FALSE, sep="\t")
#  } 
#}

cat("\t Décomposition de la matrice des notes remplie en fonction de la moyenne des utilisateurs ou des films \n")
list.SVD_Item = svd_filledMatrix("Item",train.Ratings, stat.Users, stat.Movies)
list.SVD_User = svd_filledMatrix("User",train.Ratings, stat.Users, stat.Movies)

save(list.SVD_User, file = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/vierge/list.SVD_User.Rdata"))
save(list.SVD_Item, file = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/vierge/list.SVD_Item.Rdata"))

