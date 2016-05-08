#########     Matrix Completion with Nuclear Norm Minimization    ##############################"
#
#       X is the original ratings matrix to approach
#
#       we try to iterate M to minimize the F(M)=||M-X||²+lambda*||M|| (the last one is nuclear norm)
#
#       we define 2 gradients: (i)gradient_norm_M_X for ||M-X||²    (ii)gradient_norm_M for ||M||
#
#       we monitor the F(M) value by track_F_M, easy to show evolution of F(M)
#
#       mu denotes the iteration step size known as 1/sqrt(i), which decreases with index.
#
#       M_t denotes actual M and M_t_1 denotes next M, descent gradient method is employed to work out the optimization problem
#############################################################################################################

###################### R package softImpute should be available to work it out ##############################
#   http://math.stackexchange.com/questions/701062/derivative-of-nuclear-norm
#   The above url explain how to compute nuclear norm gradient, seeming different from vincent's method #####


descentG = function(matrix.training, iteration.times=10, lambda=15)
{
  X = matrix.training
  
  # M is a randomly sampled matrix restrained from 1 to 5.
  M_t = matrix(sample(1:5,nrow(X)*ncol(X),TRUE),nrow = nrow(X), ncol = ncol(X)) #matrice initiale
 
  #M_t = M   #affecter M a M_t pour iterer
  
  #track_F_M <-NULL
  
  for(i in 1:iteration.times){
    
    mu = 2/sqrt(i)    #longeur de pas
    
    gradient_norm_M_X = (M_t-X)
    
    gradient_norm_M_X[X==0]=0 
    
    svd_M_t = svd(M_t) 
    
    gradient_norm_M = svd_M_t$u%*%t(svd_M_t$v)
    
    gradient_F_M = gradient_norm_M_X+lambda*gradient_norm_M
    
    M_t = M_t - mu*gradient_F_M
    
    #variation.M = X-M_t_1
    #variation.M[X==0]=0 
    
    #F_M = 0.5*norm(variation.M, "f")**2+lambda*sum(svd_M_t$d[1:min(dim(M_t))])
    
    #track_F_M = c(track_F_M, F_M)
    
    #M_t = M_t_1
  }
  
  colnames(M_t)=colnames(matrix.training)
  rownames(M_t)=rownames(matrix.training)
  
  M_t[M_t>5]=5
  M_t[M_t<0]=0
  
  return(M_t)
} 


proximalG = function(matrix.training, iteration.times=10, lambda=15)
{
  X = matrix.training
  
  # M is a randomly sampled matrix restrained from 1 to 5.
  M_t = matrix(sample(1:5,nrow(X)*ncol(X),TRUE),nrow = nrow(X), ncol = ncol(X)) #matrice initiale
  
  for(i in 1:iteration.times){
    
    mu = 3/sqrt(i)    #longeur de pas
    
    gradient_norm_M_X = (M_t-X)
    
    gradient_norm_M_X[X==0]=0 
    
    M_t.modified = M_t - mu*gradient_norm_M_X
    
    
    svd_M_t = svd(M_t.modified) 
    
    
    sigma.vec = pmax(svd_M_t$d-lambda, 0)
    
    index = match(0, sigma.vec)-1
    
    if(is.na(index)){
      sigma = diag(sigma.vec)
      M_t = svd_M_t$u%*%sigma%*%t(svd_M_t$v)
    }
    else if(index>=2){
      sigma = diag(sigma.vec[1:index])
      M_t = svd_M_t$u[,1:index]%*%sigma%*%t(svd_M_t$v[,1:index])
    }else if(index == 1){
      sigma = sigma.vec[1]
      M_t = svd_M_t$u[,1]%*%t(svd_M_t$v[,1])*sigma
    }
    
    
    #F_M = 0.5*norm(variation.M, "f")**2+lambda*sum(svd_M_t$d[1:min(dim(M_t))])
    
    #track_F_M = c(track_F_M, F_M)
   
    
    #M_t = M_t_1
  }
  
  colnames(M_t)=colnames(matrix.training)
  rownames(M_t)=rownames(matrix.training)
  
  M_t[M_t>5]=5
  M_t[M_t<0]=0
  
  return(M_t)
  
}  
 
  
  
svd.error.test = function(prevision, data.test)  {

  result <-NULL
  
  for(i in 1:nrow(data.test))
  {
    userID = data.test$userID[i]
    movieID = data.test$movieID[i]
    result[i] = prevision[userID, movieID]
  }
  
  error = rmse(result, data.test$rating)
  
  return(error)
  
}

