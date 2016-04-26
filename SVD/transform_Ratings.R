transform.data.rating = function(data.Training, vec.userID , vec.movieID){
  
  matrix.ratings = matrix(data=0, nrow = max(vec.userID), ncol=max(vec.movieID))
  colnames(matrix.ratings)=1:max(vec.movieID)   #sort(unique(data.Training$movieID))
  rownames(matrix.ratings)=1:max(vec.userID)   #sort(unique(data.Training$userID))
  
  
  for(i in 1:nrow(data.Training)){
    
    id.user = data.Training$userID[i]
    id.movies = data.Training$movieID[i]
    
    matrix.ratings[id.user, id.movies] = data.Training$rating[i]
    
  }
  matrix.new = matrix.ratings[sort(unique(data.Training$userID)), sort(unique(data.Training$movieID))]
  
  return(matrix.new)
}

restablish.data.rating.col = function(prevision, userID , movieID){

  
  matrix.recovery <- matrix(0, nrow = max(userID), ncol = max(movieID))
  
  vec.col.prevision = as.numeric(colnames(prevision))
  vec.row.prevision = as.numeric(rownames(prevision))
  
  vec.col.recovery = 1:max(movieID)
  vec.row.recovery = 1:max(userID)
  
  begin = sum(vec.col.recovery == vec.col.prevision)
  matrix.recovery[,1:begin] = prevision[,1:begin]
  
  end = max(movieID)
  j=begin + 1
  
  for(i in (begin+1):end){
    
    if(i==vec.col.prevision[j]){
      matrix.recovery[,i] = prevision[,j]
      j = j+1
    }
    
  }
  
  return(matrix.recovery)
}