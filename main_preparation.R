# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_preparation.R
#       Description : fonction principal pour les générer les bases de travail
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ======================================== 1.PREAMBULE =============================================

## Clean up
rm(list=ls()) 
cat("\014") 

source("./Util/open_files.R")

# ================================== 2.PREPARATION DES FICHIERS ==============================

# Base de données des utilisateurs et leurs statistiques
source("./Util/stat_Users.R")
stat.Users = stat_Users(data.Ratings)
recap.Users = as.data.frame(merge(data.Users, stat.Users, by.x = "userID", by.y = "userID"))
write.table(recap.Users, paste0("./Results/", repository, "/recap.Users.tsv"), row.names = FALSE, sep="\t")

# Base de données des films et leurs statistiques
source("./Util/stat_Movies.R")
stat.Movies = stat_Movies(data.Ratings)
recap.Movies = merge(data.Movies, stat.Movies, by.x = "movieID", by.y = "movieID")
write.table(recap.Movies, paste0("./Results/", repository, "/recap.Movies.tsv"), row.names = FALSE, sep="\t")

# Listes des films notés par utilisateur
source("./Util/deja_Vu.R")
list.dejaVu = deja_Vu(data.Ratings)
save(list.dejaVu, file = paste0("Results/", repository, "/list.dejaVu.Rdata"))

# Matrice des similarités (pearson, nrmse, nmae)
source("./NeighborhoodBasedAlgorithms/proxi_Users.R")
source("./NeighborhoodBasedAlgorithms/proxi_Users_AllvsAll.R")

mat.sim_pearson = proxi_Users_AllvsAll(data.Ratings, "pearson")
write.table(mat.sim_pearson, paste0("./Results/", repository, "/mat.sim_pearson.tsv"), row.names = FALSE, sep="\t")

mat.sim_nrmse = proxi_Users_AllvsAll(data.Ratings, "nrmse")
write.table(mat.sim_nrmse, paste0("./Results/", repository, "/mat.sim_nrmse.tsv"), row.names = FALSE, sep="\t")

mat.sim_nmae = proxi_Users_AllvsAll(data.Ratings, "nmae")
write.table(mat.sim_nmae, paste0("./Results/", repository, "/mat.sim_nmae.tsv"), row.names = FALSE, sep="\t")

