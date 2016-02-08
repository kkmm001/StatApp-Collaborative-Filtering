# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#       Fichier : recommandation_naive.R
#       Description : 
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#
# Suite aux résultats obtenus grâce aux fonctions de prédictions naives (results_naivePredictionTest.csv), 
# on choisit de prendre comme recommandation pour les différents utilisateurs les films ayant obtenus les plus hautes moyennes
# car le prédicteur meanByMovie possède les erreurs minimales
#
#
recommandation_naive = function (train.Ratings, IdUSer,nb.movie)
{
  #generation des statistiques sur les donnees de l'apprentissage
  stat.Users = stat_Users(train.Ratings)
  stat.Movies = stat_Movies(train.Ratings)
  
  #nb.Tests = dim(test.Ratings)[1]
  #resultTest = test.Ratings
  
  meanOfMovies=round(mean(stat.Movies$mean,na.rm=T),2)
  VectTri=order(meanOfMovies)
  movie=1
  result=c()
  while (movie<=nb.movie)
  {
    if(train.Ratings[userID==IdUser movieID==VectTri[i]])
    
    train.Ratings$rating[train.Ratings$movieID == movie & userID==IdUser m]
    {
      result=c(result,VectTri[i])
      i=i+1
    }
  }
}

# Remarques de Mehdi : Est-ce que ça marche ? 
# Le i à la ligne 31 n'est pas déclaré (c'est pas movie ?)
# Il manque une accolade ouvrante pour le if à la ligne 26
# stat.Users est totalement inutile ici
# Il faut faire la recherche des films non pas dans l'ensemble de films mais dans l'ensemble des films que l'utilisateur n'a pas encore vus
# Il faut afficher les titres des films pour une vérification logique + l'historique des films de l'utiliasteur (en bonus)
# Il faut travailler sur data.Ratings et non train.Ratings
