# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : main_split.R
#       Description : fonction principal pour générer les sous-bases de travail des tests
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ======================================== 1.PREAMBULE =============================================

## Clean up
rm(list=ls()) 
cat("\014") 

#Need to change working directory

source("./Util/open_files.R", encoding = 'UTF-8')

# ======================================== 2.DECOUPAGE DE LA BASE DES NOTES =============================================

source("./Util/split_data.R")

nb.Tests = as.integer(readline(prompt = "Choisissez le nombre de sous-bases : "))

list.Datasets = split_data(data.Ratings, nb.Tests)
#save(list.Datasets, file = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/list.Datasets.Rdata"))
