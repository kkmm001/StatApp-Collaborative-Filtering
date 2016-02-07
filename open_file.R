# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquee
#    Sujet : Filtrage collaborative
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : open_file.R
#       Description : charge les fichiers une fois le dossier sélectionné
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

repository <- readline(prompt="Choisissez un dossier : ")

data.Ratings = read.csv2(file = paste0("./", repository, "/data.Ratings.csv"), header=T)
data.Movies = read.csv2(file = paste0("./", repository, "/data.Movies.csv"), header=T)
data.Users = read.csv2(file = paste0("./", repository, "/data.Users.csv"), header=T)

if(repository == "ml-100k"){
  
  vect.MovieGenres = c("unknown", "Action", "Adventure", "Animation", "Children's", "Comedy", 
                       "Crime", "Documentary", "Drama", "Fantasy", "Film-noir", "Horror", 
                       "Musical", "Mystery", "Romance", "Sci-fi", "Thriller", "War", "Western")
  
  nb.Genres = length(vect.MovieGenres)
  
}
