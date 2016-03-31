type.match=function(matrix.Movies, type.film)
{ 
  len = dim(matrix.Movies)[1]
  width = dim(matrix.Movies)[2]
  type.beginIndex = grep("unknown", colnames(matrix.Movies))
  for(j in 1:len)
  {
    i<-type.beginIndex
    vector.index <- numeric(0)
    
    while (!(is.null(matrix.Movies[j,i])|is.na(matrix.Movies[j,i])|(nchar(matrix.Movies[j,i])==0))){
      index = match(as.character(matrix.Movies[j,i]), type.film)+type.beginIndex-1
      vector.index = c(vector.index, index)
      i=i+1
    }
    
    matrix.Movies[j, type.beginIndex:width]=0
    if(length(vector.index)==0){
      matrix.Movies[j, type.beginIndex]=1
      
    }else{
      for(i in 1:length(vector.index))
      {
        matrix.Movies[j, vector.index[i]]=1
      }
    }
  }
  return(matrix.Movies)
}

genresOfMovie = function(movieID, res)
{
  len = dim(res)[1]
  width = dim(res)[2]
  type.beginIndex = grep("unknown", colnames(res))
  return(res[res$id==movieID, type.beginIndex:width]) 
}
