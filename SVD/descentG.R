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


source("./CrossValidation/main_split.R")
source("./Util/transform_Ratings.R")
#install.packages("Metrics")
library("Metrics")

nb.subbase=4

data.training = list.Datasets[[1]]
for(i in 2:nb.subbase){
  
  data.training = rbind(list.Datasets[[i]])
  
}



data.check = list.Datasets[[5]]

vector.userID = sort(as.numeric(unique(data.Ratings$userID)))
vector.movieID = sort(as.numeric(unique(data.Ratings$movieID)))



X = transform.data.rating(data.training, vector.userID, vector.movieID)
#Y = transform.data.rating(data.Ratings, vector.userID, vector.movieID)

# M_t is a randomly sampled matrix restrained from 1 to 5.
M = matrix(sample(1:5,nrow(X)*ncol(X),TRUE),nrow = nrow(X), ncol = ncol(X)) #matrice initiale

#lambda=15

iteration.times = 20


track_error <- NULL


#track_F_M <- NULL
for(lambda in 15)#c(5, 10,15, 20))
{
  M_t = M
  
  for(i in 1:iteration.times){
    
    mu = 2/sqrt(i)
    
    gradient_norm_M_X = 2*(M_t-X)
    
    gradient_norm_M_X[X==0]=0
    
    svd_M_t = svd(M_t) 
    
    gradient_norm_M = svd_M_t$u%*%t(svd_M_t$v)
    
    gradient_F_M = gradient_norm_M_X+lambda*gradient_norm_M
    
    M_t_1 = M_t - mu*gradient_F_M
    
    #F_M = norm(X-M_t_1, "f")**2+lambda*sum(svd_M_t$d[1:min(dim(M_t))])
    
    #track_F_M = c(track_F_M, F_M)
    
    M_t = M_t_1
  }

  # retablissement de la matrice
  
  matrix.prevision = restablish.data.rating.col(M_t_1, vector.userID, vector.movieID)
  colnames(matrix.prevision) = 1:max(vector.movieID)
  
  result <-NULL
  
  for(i in 1:nrow(data.check))
  {
    userID = data.check$userID[i]
    movieID = data.check$movieID[i]
    result[i] = matrix.prevision[userID, movieID]
  }
  data.check = cbind(data.check, result)
  
  error = rmse(result, data.check$rating)
  
  track_error = cbind(track_error,c(lambda, error, iteration.times))
  
}

rownames(track_error)<-c("lambda","error", "times")