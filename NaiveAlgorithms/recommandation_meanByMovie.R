#
# Suite aux résultats obtenus grâce aux fonctions de prédictions naives (results_naivePredictionTest.tsv), 
# on choisit de prendre comme recommandation pour les différents utilisateurs les films non vus ayant obtenus 
# les plus hautes moyennes car le prédicteur meanByMovie possède les erreurs minimales.
# On recommande les films qui ont dépassé une certaine limite de visionnage nbMin.Ratings.
#

recommandation_meanByMovie = function (recap.Movies, list.dejaVu, userID, nb.Recommandations, nbMin.Ratings = 1){
  #INPUT  recap.Movies          : la base des films avec les statistiques
  #       list.dejaVu           : la liste des films notés par utilisateur
  #       userID                : l'identifiant de l'utilisateur à qui l'on recommande
  #       nb.Recommandations    : nombre de films recommandés
  #       nbMin.Ratings         : seuil de visionnage à partir duquel les films pourront être recommandés
  #OUTPUT mat.Recommendations  : matrice contenant les identifiants de films recommandés et la note prédite

  # Génération du vecteur de films qu'userID a deja vu
  vect.DejaVu=list.dejaVu[[userID]]
  
  # Suppression des films deja visionnés de stat.Movies
  recap.Movies = recap.Movies[!(recap.Movies$movieID %in% vect.DejaVu),]

  # Suppression des films n'ayant pas dépassé le seuil de visionnage
  recap.Movies = recap.Movies[recap.Movies$nb.Ratings >= nbMin.Ratings,]

  # Tri de stat.Movies par ordre décroissant vis-à-vis de la moyenne de chaque film        
  mat.Recommendations=recap.Movies[order(recap.Movies$mean,decreasing=TRUE), c("movieID","mean")]
  colnames(mat.Recommendations) = c("movieID", "prating")
  
  # Recommandation des nb.Recommandations films ayant les meilleurs moyennes  
  return(mat.Recommendations[1:nb.Recommandations,])
  
}
