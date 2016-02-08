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

  recommandation_naive = function (data.Ratings, id.user,nb.movie)
  {
    # generation des statistiques sur les donnees de l'apprentissage
    stat.Movies = stat_Movies(data.Ratings)
    # generation du vecteur de films qu'id.user a deja vu
    dejavu=data.Ratings$movieID[data.Ratings$userID==id.user]
    # on retire les films deja visioné de stat.Movies
    for(ind_other in 1:length(dejavu))
    {
      otherID = dejavu[ind_other]
      stat.Movies = stat.Movies[stat.Movies$movieID != otherID,]
    }
    # on tri stat.Movies par odre décroissant en se base sur la moyenne de chaque film       
    VectTri=stat.Movies.non.vu[order(stat.Movies.non.vu$mean,decreasing=TRUE),]
    # on recommande les  nb.movie films ayant les meilleurs moyennes  
    return(VectTri$movieID[1:nb.movie])
  }
