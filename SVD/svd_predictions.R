svd_predictions=function(AvrRtg,list.Datasets,matUS,matSV,data.Movies,data.Users,X){
  # INPUT
  # OUTPUT  : prédit les notes pour les couples (train, test)
  
  ## BASES D'APPRENTISSAGE ET DE TEST
  nb.Tests = length(list.Datasets)
  # liss.Datasets est une list de 5 éléments ou chaque élement
  # est une liste comoposée d'un train.Ratings et d 'un test.Rating
  
  ## PREDICTION
  res=list()
  for(testID in 1:nb.Tests){
    train.Ratings=list.Datasets[testID]$train
    test.Ratings=list.Datasets[testID]$test
    US=matUS[testID]
    SV=matSV[testID]
    res[testID]=test.Ratings
    n=length(test.Ratings$rating)
    stat.Users = stat_Users(train.Ratings)
    for(i in 1:n){
      userID1=res[testID]$userID[i]
      movieID1=res[testID]$movieID[i]
      res[testID]$rating[i]=as.numeric(stat.Users$mean[stat.Users$userID==userID1]+US[userID1,]%*%SV[,movieID1])
    }

  }
  return(res)
}
  
