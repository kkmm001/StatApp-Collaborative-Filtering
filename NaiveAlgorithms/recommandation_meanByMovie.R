#
# Suite aux résultats obtenus grâce aux fonctions de prédictions naives (results_naivePredictionTest.csv), 
# on choisit de prendre comme recommandation pour les différents utilisateurs les films ayant obtenus 
# les plus hautes moyennes car le prédicteur meanByMovie possède les erreurs minimales.
# On recommande les films qui ont dépassé une certaine limite de visionnage threshold.
#

recommandation_meanByMovie = function (recap.Movies, list.DejaVu, userID, nb.Movies, threshold=1){
  #INPUT  recap.Movies  : la base des films avec les statistiques
  #       list.DejaVu   : la liste des films notés par utilisateur
  #       userID        : l'identifiant de l'utilisateur à qui l'on recommande
  #       nb.Movies     : nombre de films recommandés
  #       threshold     : seuil de visionnage à partir duquel les films pourront être recommandés
  #OUTPUT               : vecteur contenant les nb.Moviess identifiants de films recommandés

  # Génération du vecteur de films qu'userID a deja vu
  vect.DejaVu=list.DejaVu[[userID]]
  
  # Suppression des films deja visionnés de stat.Movies
  for(movieIND in 1:length(vect.DejaVu)){
    movieID = vect.DejaVu[movieIND]
    recap.Movies = recap.Movies[recap.Movies$movieID != movieID,]
  }
  
  # Suppression des films n'ayant pas dépassé le seuil de visionnage
  if(threshold != 1){
    recap.Movies = recap.Movies[recap.Movies$nb.Ratings>=threshold,]
  }
  
  # Tri de stat.Movies par ordre décroissant vis-à-vis de la moyenne de chaque film        
  vect.Recommendations=recap.Movies[order(recap.Movies$mean,decreasing=TRUE),"movieID"]
  
  # Recommandation des nb.Movies films ayant les meilleurs moyennes  
  return(vect.Recommendations[1:nb.Movies])
  
}
