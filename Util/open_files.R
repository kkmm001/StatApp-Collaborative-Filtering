# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 2AD - Groupe de statistique appliquée
#    Sujet : Filtrage collaboratif
#       Encadrants : Vincent Cottet et Mehdi Sebbar
#       Etudiants : Biwei Cui, Claudia Delgado, Mehdi Miah et Ulrich Mpeli Mpeli
#
#       Fichier : open_files.R
#       Description : charge les fichiers du problème sélectionné 
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

cat(sprintf("Les problèmes proposés sont : ml-100k et ml-1m\n"))
repository <- readline(prompt= "Choisissez un problème : ")

data.Ratings = read.table(file = paste0("Data/", repository, "/data.Ratings.tsv"), header=T, sep='\t')
data.Movies = read.table(file = paste0("Data/", repository, "/data.Movies.tsv"), header=T, quote="", sep='\t')
data.Users = read.table(file = paste0("Data/", repository, "/data.Users.tsv"), header=T, sep='\t')

vect.MovieGenres = c("unknown", "Action", "Adventure", "Animation", "Children.s", "Comedy", 
                       "Crime", "Documentary", "Drama", "Fantasy", "Film.noir", "Horror", 
                       "Musical", "Mystery", "Romance", "Sci.fi", "Thriller", "War", "Western")
  
nb.Genres = length(vect.MovieGenres)
