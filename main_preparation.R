# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_preparation.R
#       Description : fonction principal pour les générer les bases de travail et de test
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ======================================== 1.PREAMBULE =============================================

## Clean up
rm(list=ls()) 
cat("\014") 

source("./Util/open_files.R", encoding = 'UTF-8')
source("./Util/stat_Users.R")
source("./Util/stat_Movies.R")
source("./Util/deja_Vu.R")

source("./NeighborhoodBasedAlgorithms/proxi_Users.R")
source("./NeighborhoodBasedAlgorithms/proxi_Users_AllvsAll.R")

list.Similarities = c("pearson")
list.nbMin.InCommon = c(2,4,6,8,10)

source("./NeighborhoodBasedAlgorithms/nb_MoviesInCommon.R")

source("./Util/split_data.R")

# ======================================== 2.FONCTION DE FILTRAGE =============================================

filtrer = function(mat.sim, mat.InCommon, nbMin.InCommon){
  filtre = mat.sim
  filtre[mat.InCommon < nbMin.InCommon] = NA
  return(filtre)
}

# ================================== 3.PREPARATION DES FICHIERS POUR LES RECOMMANDATIONS ==============================

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

# Matrice des similarités (pearson, nrmse, nmae et RFP)
cat(" \t Création des matrices des similarités \n")
for(similarity in list.Similarities){
  cat(sprintf("\n \t \t Création de la matrice %s\n", similarity))
  mat.sim = proxi_Users_AllvsAll(data.Ratings, similarity)
  write.table(mat.sim, paste0("./Results/", repository, "/mat.sim_", similarity, "_0.tsv"), row.names = FALSE, sep="\t")
  for(nbMin.InCommon in list.nbMin.InCommon){
    cat(sprintf("\n \t \t Création de la matrice %s avec %0.f pour seuil \n", similarity, nbMin.InCommon))
    mat.sim_filtre = filtrer(mat.sim, mat.InCommon, nbMin.InCommon)
    write.table(mat.sim_filtre, paste0("./Results/", repository, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv"), row.names = FALSE, sep="\t")
  } 
}

# =============================== 3.PREPARATION DES FICHIERS POUR LA VALIDATION CROISEE ===========================

cat("Préparation des fichiers pour la validation croisée \n")

cat("Création des sous-bases \n")
nb.Tests = 5
list.Datasets = split_data(data.Ratings, nb.Tests)
save(list.Datasets, file = paste0("./CrossValidation/", repository, "/list.Datasets.Rdata"))

for(vc in 1:nb.Tests){
  
  cat(sprintf("Début de la préparation : %0.f / %0.f \n", vc, nb.Tests))
  
  dataset_to_keep = (1:nb.Tests)[(1:nb.Tests) != vc]
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  
  cat("\t Création des bases de données des utilisateurs et leurs statistiques \n")
  stat.Users = stat_Users(train.Ratings)
  write.table(stat.Users, paste0("./CrossValidation/", repository, "/train", vc, "/stat.Users.tsv"), row.names = FALSE, sep="\t")
  
  cat(" \t Création des bases de données des films et leurs statistiques \n")
  stat.Movies = stat_Movies(train.Ratings)
  write.table(stat.Movies, paste0("./CrossValidation/", repository, "/train", vc, "/stat.Movies.tsv"), row.names = FALSE, sep="\t")
  
  cat(" \t Création des listes des films notés par utilisateur \n")
  list.dejaVu = deja_Vu(train.Ratings)
  save(list.dejaVu, file = paste0("CrossValidation/", repository, "/train", vc, "/list.dejaVu.Rdata"))
  
  # Matrice du nombre de films notés en commun
  cat("\t Création de la matrice du nombre de films notés en commun \n")
  mat.InCommon = nb_MoviesInCommon(train.Ratings)
  write.table(mat.InCommon, paste0("./CrossValidation/", repository, "/train", vc, "/mat.InCommon.tsv"), row.names = FALSE, sep="\t")
  
  cat(" \t Création des matrices des similarités \n")
  for(similarity in list.Similarities){
    cat(sprintf("\n \t \t Création de la matrice %s\n", similarity))
    mat.sim = proxi_Users_AllvsAll(train.Ratings, similarity)
    write.table(mat.sim, paste0("./CrossValidation/", repository, "/train", vc, "/mat.sim_", similarity, "_0.tsv"), row.names = FALSE, sep="\t")
    for(nbMin.InCommon in list.nbMin.InCommon){
      cat(sprintf("\n \t \t Création de la matrice %s avec %0.f pour seuil \n", similarity, nbMin.InCommon))
      mat.sim = filtrer(mat.sim, mat.InCommon, nbMin.InCommon)
      write.table(mat.sim, paste0("./CrossValidation/", repository, "/train", vc, "/mat.sim_", similarity, "_", nbMin.InCommon, ".tsv"), row.names = FALSE, sep="\t")
    } 
  }
  
}
