transform.data.rating = function(data.Training, data.Ratings){
  
  matrix.ratings = matrix(data=0, nrow = max(data.Ratings$userID), ncol=max(data.Ratings$movieID))
  colnames(matrix.ratings)=1:max(data.Ratings$movieID)   #sort(unique(data.Training$movieID))
  rownames(matrix.ratings)=1:max(data.Ratings$userID)   #sort(unique(data.Training$userID))
  
  
  for(i in 1:nrow(data.Training)){
    
    id.user = data.Training$userID[i]
    id.movies = data.Training$movieID[i]
    
    #matrix.ratings[match(rownames(matrix.ratings), id.user), match(colnames(matrix.ratings), id.movies)] = data.Training$rating[i]
    matrix.ratings[id.user, id.movies] = data.Training$rating[i]
    
  }
  matrix.new = matrix.ratings[sort(unique(data.Training$userID)), sort(unique(data.Training$movieID))]
  
  return(matrix.new)
}