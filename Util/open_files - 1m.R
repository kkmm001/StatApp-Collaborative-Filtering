# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : open_files.R
#       Description : charge les fichiers une fois le dossier sélectionné pour 1m
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

## Clean up
rm(list=ls()) 
cat("\014") 

repository <- "ml-1m" #readline(prompt="Choisissez un dossier : ")

filepath.movies = paste0("Data/", repository, "/data.Movies.dat")

type.film = c("unknown", "Action", "Adventure", "Animation", "Children's", "Comedy", 
              "Crime", "Documentary", "Drama", "Fantasy", "Film-noir", "Horror", 
              "Musical", "Mystery", "Romance", "Sci-fi", "Thriller", "War", "Western")

list.col.names = c("id", "Name", type.film)

data <- read.delim(filepath.movies, header = F, sep="\t", col.names=list.col.names, stringsAsFactors = FALSE) 

matrix.movies=gsub("::", "|" , as.matrix(data))
data = read.table(text=matrix.movies, sep="|", fill=T, quote=NULL,allowEscapes = F, col.names=list.col.names, nrows = dim(matrix.movies)[1])


matrix.movies = as.matrix(data)
res = type.match(matrix.movies, type.film)
class(res[,1])<-"numeric"
class(res[,3:dim(res)[2]])<-"numeric"
res = as.data.frame(res,stringsAsFactors=FALSE)

