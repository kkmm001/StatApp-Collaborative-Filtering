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

# ================================== 2.CHOIX DES PARAMETRES ===========================================================

# Pour la partie recommandation et validation-croisée
list.Similarities = c("pearson", "RFP", "nrmse", "nmae")
list.nbMin.InCommon = c(2,4,6,8,10)

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
cat("\t Création de la matrice du nombre de films notés en commun \n")
mat.InCommon = nb_MoviesInCommon(data.Ratings)
write.table(mat.InCommon, paste0("./Results/", repository, "/mat.InCommon.tsv"), row.names = FALSE, sep="\t")

# Matrice des similarités
cat(" \n \t Création des matrices des similarités \n")
for(similarity in list.Similarities){
  cat(sprintf("\n \t \t Création de la matrice %s sans filtre\n", similarity))
  mat.sim0 = proxi_Users_AllvsAll(data.Ratings, similarity)
  write.table(mat.sim0, paste0("./Results/", repository, "/mat.sim_", similarity, "_0.tsv"), row.names = FALSE, sep="\t")
  for(nbMin.InCommon in list.nbMin.InCommon){
    cat(sprintf("\n \t \t Création de la matrice %s avec un seuil à %0.f \n", similarity, nbMin.InCommon))
    mat.sim_filtre = filtrer_similarite(mat.sim0, mat.InCommon, nbMin.InCommon)
    write.table(mat.sim_filtre, paste0("./Results/", repository, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv"), row.names = FALSE, sep="\t")
  } 
}

# =============================== 3.PREPARATION DES FICHIERS POUR LA VALIDATION CROISEE ===========================

cat("Préparation des fichiers pour la validation croisée \n")

file_list.Datasets = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/list.Datasets.Rdata")
load(file = file_list.Datasets)

for(train in 1:1){ #TODO for(train in 1:nb.Tests){
    
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
  
  cat("\t Création de la matrice du nombre de films notés en commun \n")
  mat.InCommon = nb_MoviesInCommon(train.Ratings)
  write.table(mat.InCommon, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/mat.InCommon.tsv"), row.names = FALSE, sep="\t")

  cat(" \n \t Création des matrices des similarités \n")
  for(similarity in list.Similarities){
    cat(sprintf("\n \t \t Création de la matrice %s sans filtre\n", similarity))
    mat.sim0 = proxi_Users_AllvsAll(train.Ratings, similarity)
    write.table(mat.sim0, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/mat.sim_", similarity, "_0.tsv"), row.names = FALSE, sep="\t")
    
    for(nbMin.InCommon in list.nbMin.InCommon){
      cat(sprintf("\n \t \t Création de la matrice %s avec un seuil à %0.f \n", similarity, nbMin.InCommon))
      mat.sim_filtre = filtrer_similarite(mat.sim0, mat.InCommon, nbMin.InCommon)
      write.table(mat.sim_filtre, paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/train", train, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv"), row.names = FALSE, sep="\t")
    } 
  }
  
}
