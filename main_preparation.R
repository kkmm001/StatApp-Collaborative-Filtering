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

# ================================== 2.PREPARATION DE L'ALGORITHME NAIF ==============================

source("./Util/open_files.R")

source("./Util/stat_Users.R")
stat.Users = stat_Users(data.Ratings)
recap.Users = as.data.frame(merge(data.Users, stat.Users, by.x = "userID", by.y = "userID"))
write.table(recap.Users, paste0("./Results/", repository, "/recap.Users.tsv"), row.names = FALSE, sep="\t")

source("./Util/stat_Movies.R")
stat.Movies = stat_Movies(data.Ratings)
recap.Movies = merge(data.Movies, stat.Movies, by.x = "movieID", by.y = "movieID")
write.table(recap.Movies, paste0("./Results/", repository, "/recap.Movies.tsv"), row.names = FALSE, sep="\t")

source("./Util/deja_Vu.R")
list.dejaVu = deja_Vu(data.Ratings)
save(list.dejaVu, file = paste0("Results/", repository, "/list.dejaVu.Rdata"))
