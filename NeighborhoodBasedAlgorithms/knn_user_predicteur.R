knn_user_predicteur = function(Q, vect.Similarity.byNN, vect.Ratings.byNN, stat.Users, userID, predicteur, qnn){
  if(predicteur == "weighted-centered"){
    meanOfUser = as.numeric(stat.Users$mean[stat.Users$userID == userID])
    meanOfNeighbors = sapply(qnn, function(x) stat.Users$mean[stat.Users$userID == x])
  }
  switch(predicteur, 
         'mean' = mean(vect.Ratings.byNN[1:Q], na.rm = TRUE),
         'weighted' = sum(vect.Ratings.byNN[1:Q] * vect.Similarity.byNN[1:Q], na.rm = TRUE)/ sum(vect.Similarity.byNN[1:Q]),
         'weighted-centered' = meanOfUser + sum((vect.Ratings.byNN[1:Q]-meanOfNeighbors[1:Q]) * vect.Similarity.byNN[1:Q], na.rm = TRUE)/ sum(vect.Similarity.byNN[1:Q])
         )
}