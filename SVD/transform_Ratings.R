transform.data.rating = function(data.Training, vec.userID , vec.movieID){
  
  matrix.ratings = matrix(data=0, nrow = max(vec.userID), ncol=max(vec.movieID))
  colnames(matrix.ratings)=1:max(vec.movieID)   #sort(unique(data.Training$movieID))
  rownames(matrix.ratings)=1:max(vec.userID)   #sort(unique(data.Training$userID))
  
  
  for(i in 1:nrow(data.Training)){
    
    id.user = data.Training$userID[i]
    id.movies = data.Training$movieID[i]
    
    matrix.ratings[id.user, id.movies] = data.Training$rating[i]  #Pour eviter les recherches dans colnames/rownames, il faut garder vec.user/movies dans l'entree
    
  }
  matrix.new = matrix.ratings[sort(unique(data.Training$userID)), sort(unique(data.Training$movieID))]
  
  return(matrix.new)
}


######### restablish original dimension by adding zero vector in prevision base
restablish.data.rating.col = function(prevision, vec.userID , vec.movieID){ #prevision n'inclut que les donnees training

  matrix.recovery <- matrix(NA, nrow = max(vec.userID), ncol = max(vec.movieID))
  
  vec.col.prevision = as.numeric(colnames(prevision))
  vec.row.prevision = as.numeric(rownames(prevision))
  
  vec.col.recovery = 1:max(vec.movieID)
  vec.row.recovery = 1:max(vec.userID)
  
  begin = sum(vec.col.recovery == vec.col.prevision)
  matrix.recovery[,1:begin] = prevision[,1:begin]
  
  end = max(vec.movieID)
  j=begin + 1
  
  for(i in (begin+1):end){
    
    if(j<=length(vec.col.prevision)){
      if(i==vec.col.prevision[j]){
        matrix.recovery[,i] = prevision[,j]
        j = j+1
      }
    }
  }
  
  return(matrix.recovery)
}