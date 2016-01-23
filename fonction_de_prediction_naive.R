# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =#
#       Fichier : Fonction_prediction_naive.R    y                                                  #
#       Description : Fonction de prédiction naive sur les bases                                   #
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =#

prediction_naive=function(stat_Users,stat_Movies, matrixTest){
  
    D = dim(matrixTest)[1]
    ResultTest = matrixTest
    ResultTest$Prediction1=runif(D,1,5)
    
    #par la note moyenne de USER
    ResultTest$Prediction2=mean(stat_Users$mean,na.rm=T)
    
    ResultTest$Prediction3=mean(stat_Movies$mean,na.rm=T) # on doit d'abord enlever les films qui n'ont pas été noté ie qui ont une moyenne = a Nan avant de prendre la moyenne generale
    
    for (rowIndex in 1:D) { 
      flag = stat_Users$userID==ResultTest$userID[rowIndex]
      if (sum(flag)){
        ResultTest$Prediction4[rowIndex]=stat_Users$mean[flag]
      }
    }
    
    for (rowIndex in 1:D) { 
      flag = stat_Movies$movieID==ResultTest$movieID[rowIndex]
      if (sum(flag)){
        ResultTest$Prediction5[rowIndex]=stat_Movies$mean[flag]
      }
    }
    
   
    
    return(ResultTest)
  #}
}






