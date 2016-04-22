svd_predictions=function(list.Datasets,train,matUS,matSV){
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
  
  vect.MoviesInTrain = sort(unique(train.Ratings$movieID))
  vect.UsersInTrain = sort(unique(train.Ratings$userID))
  
  US=matUS[[train]]
  SV=matSV[[train]]
  res=test.Ratings
  n=length(test.Ratings$rating)
  stat.Users = stat_Users(train.Ratings)

  for(i in 1:n){
    cat(paste0("|", i))
    userID=res$userID[i]
    movieID=res$movieID[i]
    movieIND = which(vect.MoviesInTrain == movieID)    
    userIND = which(vect.UsersInTrain == userID)

    if(movieID %in% vect.MoviesInTrain & userID %in% vect.UsersInTrain){
      res$prating[i]=as.numeric(stat.Users$mean[stat.Users$userID==userID]+US[userIND,]%*%SV[,movieIND])
    }
    else{
      res$prating[i] = NA
    }
  }


  return(res)
}
  
