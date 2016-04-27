
source("./CrossValidation/main_split.R")
source("./SVD/transform_Ratings.R")
source("./SVD/DescentG.R")
#install.packages("hydroGOF")
#library("Metrics")
library("zoo")
library("hydroGOF")

###### divise and scatter data.ratings into 2 parts(training, test) 
nb.subbase=4

data.test = list.Datasets[[1]]

data.training <- NULL

for(i in 2:nb.subbase){
  
  data.training = rbind(list.Datasets[[i]])
  
}

vector.userID = sort(as.numeric(unique(data.Ratings$userID)))
vector.movieID = sort(as.numeric(unique(data.Ratings$movieID)))

training.reduced = transform.data.rating(data.training, vector.userID, vector.movieID)

track_error <- NULL

lambda.set = c(20)

iteration.times=50

for(lambda in lambda.set){
  
  prevision.reduced = descentG(training.reduced, iteration.times, lambda)
  
  data.prevision = restablish.data.rating.col(prevision.reduced, vector.userID, vector.movieID)
  
  error = svd.error.test(data.prevision, data.test)
  
  track_error = cbind(track_error,c(lambda, error, iteration.times))

}

rownames(track_error)<-c("lambda","error", "times")


