
source("./CrossValidation/main_split.R")
source("./SVD/transform_Ratings.R")
source("./SVD/DescentG.R")
#install.packages("hydroGOF")
#library("Metrics")
library("zoo")
library("hydroGOF")


vector.userID = sort(as.numeric(unique(data.Ratings$userID)))
vector.movieID = sort(as.numeric(unique(data.Ratings$movieID)))

track_error <- NULL
###### divise and scatter data.ratings into 2 parts(training, test) 
no.base.test=1

data.test = list.Datasets[[no.base.test]]

data.training <- NULL

for(i in 1:5){
  if(i!=no.base.test)
  {
    data.training = rbind(data.training, list.Datasets[[i]])
  }
  
}

matrix.training = transform.data.rating(data.training, vector.userID, vector.movieID)


lambda.set = c(5, 10, 15, 20)

iteration.times=5

for(lambda in lambda.set){
  
  matrix.prevision = descentG(matrix.training, iteration.times, lambda)
  #matrix.prevision = proximalG(matrix.training, iteration.times, lambda)
  
  data.prevision = restablish.data.rating.col(matrix.prevision, vector.userID, vector.movieID)
  
  error = svd.error.test(data.prevision, data.test)
  
  track_error = cbind(track_error,c(lambda, error, iteration.times))

}

rownames(track_error)<-c("lambda","error", "times")


