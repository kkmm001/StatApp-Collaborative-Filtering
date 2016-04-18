transform.data.rating = function(data.Ratings){
  
  matrix.ratings = matrix(data=0, nrow = max(data.Users$userID), ncol=max(data.Movies$movieID))
  colnames(matrix.ratings)=1:max(data.Movies$movieID)
  rownames(matrix.ratings)=1:max(data.Users$userID)
  
  
  for(i in 1:nrow(data.Ratings)){
    
    id.user = data.Ratings[i,1]
    id.movies = data.Ratings[i,2]
    matrix.ratings[id.user, id.movies]=data.Ratings[i,3]
  }
  
  return(matrix.ratings)
}