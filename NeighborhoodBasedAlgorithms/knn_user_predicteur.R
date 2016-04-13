knn_user_predicteur = function(Q, vect.Similarity.byNN, vect.Ratings.byNN, stat.Users, userID, predicteur, qnn){
  # ============#
  get_meanRatings = function(userID){
    if(is.na(userID)){
      return(NA)
    }
    else{
      res = stat.Users$mean[stat.Users$userID == userID]
      return(res)
    }
  }
  
  # ============#
  
  limited_value = function(value){
    if(is.na(value)){
      return(NA)
    }
    else if(value > 5){
      return(5)
    }
    else if(value < 1){
      return(1)
    }
    else{
      return(value)
    }
  }
  
  # ============#
  
  meanOfUser = as.numeric(stat.Users$mean[stat.Users$userID == userID])
  meanOfNeighbors = sapply(qnn,get_meanRatings)
  switch(predicteur, 
         'mean' = mean(vect.Ratings.byNN[1:Q], na.rm = TRUE),
         'weighted&b' = limited_value(sum(vect.Ratings.byNN[1:Q] * vect.Similarity.byNN[1:Q], na.rm = TRUE)/ sum(vect.Similarity.byNN[1:Q], na.rm = TRUE)),
         'weighted-centered&b' = limited_value(meanOfUser + sum((vect.Ratings.byNN[1:Q]-meanOfNeighbors[1:Q]) * vect.Similarity.byNN[1:Q], na.rm = TRUE)/ sum(vect.Similarity.byNN[1:Q], na.rm = TRUE)),
         'weighted&a' = sum(vect.Ratings.byNN[1:Q] * vect.Similarity.byNN[1:Q], na.rm = TRUE)/ sum(abs(vect.Similarity.byNN[1:Q]), na.rm = TRUE),
         'weighted-centered&a' = meanOfUser + sum((vect.Ratings.byNN[1:Q]-meanOfNeighbors[1:Q]) * vect.Similarity.byNN[1:Q], na.rm = TRUE)/ sum(abs(vect.Similarity.byNN[1:Q]), na.rm = TRUE),
         'weighted&ab' = limited_value(sum(vect.Ratings.byNN[1:Q] * vect.Similarity.byNN[1:Q], na.rm = TRUE)/ sum(abs(vect.Similarity.byNN[1:Q]), na.rm = TRUE)),
         'weighted-centered&ab' = limited_value(meanOfUser + sum((vect.Ratings.byNN[1:Q]-meanOfNeighbors[1:Q]) * vect.Similarity.byNN[1:Q], na.rm = TRUE)/ sum(abs(vect.Similarity.byNN[1:Q]), na.rm = TRUE))
         
         )
}