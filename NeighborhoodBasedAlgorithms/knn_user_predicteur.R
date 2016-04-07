knn_user_predicteur = function(Q, vect.Similarity.byNN, vect.Ratings.byNN, stat.Users, userID, predicteur){
  switch(predicteur, 
         'mean' = mean(vect.Ratings.byNN[1:Q], na.rm = TRUE),
         'weighted' = 0,
         'weighted-centered' = 0
         )
}