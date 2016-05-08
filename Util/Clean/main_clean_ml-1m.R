# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : Pre-.R
#       Description : charge les fichiers une fois le dossier sélectionné pour 1m
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

## Clean up
rm(list=ls()) 
cat("\014") 
repository <- "ml-1m" 

source("Util/Clean/read_processing-1m.R")

############# 1. Movies Base importation  ##################


filepath.movies = paste0("Data/", repository, "/data.Movies.dat")

type.film = c("unknown", "Action", "Adventure", "Animation", "Children.s", "Comedy", 
              "Crime", "Documentary", "Drama", "Fantasy", "Film.noir", "Horror", 
              "Musical", "Mystery", "Romance", "Sci.fi", "Thriller", "War", "Western")

list.col.names = c("movieID", "title", type.film)

data <- read.delim(filepath.movies, header = F, sep="\t", col.names=list.col.names, stringsAsFactors = FALSE) 

matrix.movies=gsub("::", "|" , as.matrix(data))
data = read.table(text=matrix.movies, sep="|", fill=T, quote=NULL, allowEscapes = F, col.names=list.col.names, nrows = dim(matrix.movies)[1])

matrix.movies = as.matrix(data)

data.Movies = type.match(matrix.movies, type.film)

data.Movies = as.data.frame(data.Movies,stringsAsFactors=FALSE)

class(data.Movies[,1])<-"numeric"

for(i in 3:dim(data.Movies)[2])
{
  class(data.Movies[,i])<-"numeric"
}

############# 2. User Base importation  ##################

filepath.user = paste0("Data/", repository, "/data.Users.dat")
col.names.user = c("userID",	"age",	"sex",	"occupation")

data = read.delim(filepath.user, header = F, sep="\t", stringsAsFactors = FALSE)
matrix.user=gsub("::", "," , as.matrix(data))
write.table(matrix.user, file = "Data/ml-1m/middle",row.names = FALSE, col.names = FALSE,  quote=FALSE)

filepath.user = paste0("Data/", repository, "/middle")
data.Users = read.table(filepath.user, sep=",",header = FALSE, fill=T, quote=NULL, allowEscapes = F,  stringsAsFactors = FALSE)  
data.Users = data.Users[,-5]
colnames(data.Users)=col.names.user

############# 3. Rating Base importation  ##################

filepath.ratings = paste0("Data/", repository, "/data.Ratings.dat")
col.names.ratings = c("userID",	"movieID",	"rating")

data = read.delim(filepath.ratings, header = F, sep="\t", stringsAsFactors = FALSE)
matrix.ratings=gsub("::", "," , as.matrix(data))
write.table(matrix.ratings, file = "Data/ml-1m/middle",row.names = FALSE, col.names = FALSE,  quote=FALSE)

filepath.ratings = paste0("Data/", repository, "/middle")
data.Ratings = read.table(filepath.ratings, sep=",",header = FALSE, fill=T, quote=NULL, allowEscapes = F,  stringsAsFactors = FALSE)  
data.Ratings = data.Ratings[,-4]
colnames(data.Ratings)=col.names.ratings


unique.users = unique(data.Ratings$userID)
unique.movies = unique(data.Ratings$movieID)

data.Movies = data.Movies[is.element(data.Movies$movieID, unique.movies),]
data.Users = data.Users[is.element(data.Users$userID, unique.users),]

write.table(data.Movies, file='Data/ml-1m/data.Movies1.tsv', quote=FALSE, sep='\t', col.names = list.col.names, row.names=FALSE)
write.table(data.Users, file='Data/ml-1m/data.Users1.tsv', quote=FALSE, sep='\t', col.names = col.names.user, row.names=FALSE)
write.table(data.Ratings, file='Data/ml-1m/data.Ratings1.tsv', quote=FALSE, sep='\t', col.names = col.names.ratings, row.names=FALSE)

rm(list=ls()) 

