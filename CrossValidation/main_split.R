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



set.seed(42)

# Génération d'un vecteur aléatoire
alea = runif(nrow(data.Ratings))

# Création de la base Vierge
data.Ratings.Vierge = subset(data.Ratings, (alea<quantile(alea,0.05)))

#Création de list.dataset servant à la cross validation

data.Ratings.Autre=subset(data.Ratings, (alea>=quantile(alea,0.05)))

list.Datasets = split_data(data.Ratings.Autre, nb.Tests)
save(list.Datasets, file = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/list.Datasets.Rdata"))
save(data.Ratings.Vierge, file = paste0("./CrossValidation/", repository, "/CV", nb.Tests, "/data.Ratings.Vierge.Rdata"))
