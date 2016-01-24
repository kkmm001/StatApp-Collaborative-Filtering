# Notes de Mehdi : il faudrait une fonction indiquant le dossier contenant les données
# du type open_file(repository = ml-100k) ou open_file(repository = ml-1m).
# ATTENTION : cette fonction devra être compatible avec les autres bases de données.
# En effet, tous ne contiennent pas de fichiers 'data.Users' et les formats sont différents (.dat ou .csv).


## Lecture du fichier des notes
u.data = file.choose()
cat(u.data)
#u.data=c(fileRepo,"\u.data")

u.user=paste(substr(u.data, 1, nchar(u.data)-4), "user", sep="")
u.item=paste(substr(u.data, 1, nchar(u.data)-4), "item", sep="")
cat(u.item)

data.Ratings = read.table(file=u.data,header=F,colClasses = c(V4 = "NULL"))
colnames(data.Ratings) = c("userID", "movieID", "rating")


## Lecture du fichier des utilisateurs

data.Users = read.table(file=u.user,header=F, sep='|', stringsAsFactors = TRUE)
colnames(data.Users) = c("userID", "age", "gender", "occupation", "zip.code")

## Lecture du fichier des films

data.Movies = read.table(file=u.item,header=F,sep="|", quote = "\"",colClasses = c(V4 = "NULL"))

vect.MovieGenres = c("unknown", "action", "adventure", "animation", "children's", "comedy", 
                     "crime", "documentary", "drama", "fantasy", "film-noir", "horror", 
                     "musical", "mystery", "romance", "sci-fi", "thriller", "war", "western")

nb.Genres = length(vect.MovieGenres)
colnames(data.Movies) = c("movieID", "title", "date", "IMDbURL", vect.MovieGenres)
