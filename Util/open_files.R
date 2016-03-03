# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquee
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : open_files.R
#       Description : charge les fichiers une fois le dossier sélectionné
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

repository <- readline(prompt="Choisissez un dossier : ")

data.Ratings = read.csv(file = paste0("Data/", repository, "/data.Ratings.csv"), header=T, sep=',')
data.Movies = read.csv(file = paste0("Data/", repository, "/data.Movies.csv"), header=T, sep=',')
data.Users = read.csv(file = paste0("Data/", repository, "/data.Users.csv"), header=T, sep=',')

if(repository == "ml-100k"){
  
  vect.MovieGenres = c("unknown", "Action", "Adventure", "Animation", "Children's", "Comedy", 
                       "Crime", "Documentary", "Drama", "Fantasy", "Film-noir", "Horror", 
                       "Musical", "Mystery", "Romance", "Sci-fi", "Thriller", "War", "Western")
  
  nb.Genres = length(vect.MovieGenres)
  
}
