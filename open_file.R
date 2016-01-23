# ===================================== 1.PREAMBULE ===============================================

## Clean up

rm(list=ls()) 
cat("\014") 


# ================================== 2.OUVERTURE DES FICHIERS =======================================

## Lecture du fichier des notes
u.data = file.choose()
cat(u.data)
#u.data=c(fileRepo,"\u.data")

u.user=paste(substr(u.data, 1, nchar(u.data)-4), "user", sep="")
u.item=paste(substr(u.data, 1, nchar(u.data)-4), "item", sep="")
cat(u.item)

data.Ratings = read.table(file=u.data,header=F,colClasses = c(V4 = "NULL"))
#data.Ratings = read.csv("../../Data/ml-100k/u.data.", header = FALSE, sep='\t')
colnames(data.Ratings) = c("userID", "movieID", "rating")
#data.Ratings$timestamp = NULL


## Lecture du fichier des utilisateurs

data.Users = read.table(file=u.user,header=F, sep='|', stringsAsFactors = TRUE)
#data.Users = read.csv("../../Data/ml-100k/u.user", header = FALSE, sep='|', stringsAsFactors = TRUE)
colnames(data.Users) = c("userID", "age", "gender", "occupation", "zip.code")

## Lecture du fichier des films

data.Movies = read.table(file=u.item,header=F,sep="|", quote = "\"",colClasses = c(V4 = "NULL"))
#data.Movies = read.csv("../../Data/ml-100k/u.item", header = FALSE, sep='|')
#data.Movies[4] = NULL
vect.MovieGenres = c("unknown", "action", "adventure", "animation", "children's", "comedy", 
                     "crime", "documentary", "drama", "fantasy", "film-noir", "horror", 
                     "musical", "mystery", "romance", "sci-fi", "thriller", "war", "western")

nb.Genres = length(vect.MovieGenres)
colnames(data.Movies) = c("movieID", "title", "date", "IMDbURL", vect.MovieGenres)
