prediction=function(RU1,RM1,userID,movieID)
{
  X=RU1$rating[(RU1$userID==userID)&(RU1$movieID==movieID)]
  if (length(X)>0)
  {
    cat("L'utilisateur", userID, " a dÃ©jÃ  notÃ© le film",movieID,"il lui a donnÃ© la note de",x1,x2,x3,x4)
  }
  else
  {
    pred=rep(0,5)
    pred[1]=runif(1,1,5)
    pred[2]=mean(RU1$mean,na.rm=T)
    pred[3]=mean(RM1$mean,na.rm=T) # on doit d'abord enlever les films qui n'ont pas Ã©tÃ© notÃ© ie qui ont une moyenne = a Nan avant de prendre la moyenne generale
    pred[4]=RU1$mean[userID==userID]
    pred[5]=RM1$mean[movieID==movieID]
    pred=round(pred,4)
    cat("On peut prÃ©dire la note par :")
    cat("\n")
    cat("-  Une note complÃ©tement alÃ©atoire  :",pred[1])
    cat("\n")
    cat("-  La moyenne de toutes les notes prÃ©sentes dans la base modÃ¨le :",pred[2])
    cat("\n")
    cat("-  La moyenne des moyennes des films (sans pondÃ©ration):",pred[3])
    cat("\n")
    cat("-	La moyenne des notes que l'utilisateur i a donnÃ© aux autres films :",pred[4])
    cat("\n")
    cat("-	La moyenne des notes donnÃ©es par tous les autres utilisateurs au film ",movieID,":",pred[5])
    cat("\n")
    return(pred)
  }
}
