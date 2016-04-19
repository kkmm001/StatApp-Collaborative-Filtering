svd_predictions=function(list.Datasets,train,matUS,matSV,,X){
  # INPUT
  # OUTPUT  : prédit les notes pour les couples (train, test)
  
  ## BASES D'APPRENTISSAGE ET DE TEST
  nb.Tests = length(list.Datasets)
  # liss.Datasets est une list de 5 éléments ou chaque élement
  # est une liste comoposée d'un train.Ratings et d 'un test.Rating
  
  ## PREDICTION
  dataset_to_keep = (1:nb.Tests)[(1:nb.Tests) != train]
  train.Ratings = do.call("rbind", list.Datasets[dataset_to_keep])
  test.Ratings=list.Datasets[[train]]
  US=matUS[[train]]
  SV=matSV[[train]]
  res=test.Ratings
  n=length(test.Ratings$rating)
  stat.Users = stat_Users(train.Ratings)
  for(i in 1:n){
    userID1=res$userID[i]
    movieID1=res$movieID[i]
    res$rating[i]=as.numeric(stat.Users$mean[stat.Users$userID==userID1]+US[userID1,]%*%SV[,movieID1])
  }


  return(res)
}
  
