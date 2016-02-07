# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#       Fichier : recommandation_naive.R
#       Description : 
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#
# Suite aux résultats obtenus grâce aux fonctions de prédictions naives (results_naivePredictionTest.csv), 
# on choisit de prendre comme recommandation pour les différenst utilisateur les films ayant obtenus les plus hautes moyennes
# car  meanByMovie possède les erreurs minimales
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
