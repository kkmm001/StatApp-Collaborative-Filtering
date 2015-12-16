prediction=function(U1,U2,U3,U4,userID,movieID,data.Movies,data.Users)
{
  x1=U1$rating[(U1$userID==userID)&(U1$movieID==movieID)]
  x2=U2$rating[(U2$userID==userID)&(U2$movieID==movieID)]
  x3=U3$rating[(U3$userID==userID)&(U3$movieID==movieID)]
  x4=U4$rating[(U4$userID==userID)&(U4$movieID==movieID)]
  m=max(length(x1),length(x2),length(x3),length(x4))
  if (m>0)
  {
    cat("L'utilisateur", userID, " a dÃ©jÃ  notÃ© le film",movieID,"il lui a donnÃ© la note de",x1,x2,x3,x4)
  }
  else
  {
    Users=rbind(U1,U2,U3,U4)
    RM=recap_Movies(U1,U2,U3,U4,data.Movies,data.Users) # IL FAUT CREER CETTE FONctiON LES AMIS
    RU=recap_Users(U1,U2,U3,U4,data.Movies,data.Users)  # IL FAUT CREER CETTE FONctiON LES AMIS
    pred=rep(0,5)
    pred[1]=runif(1,1,5)
    pred[2]=mean(data.Users$rating,na.rm=T)
    pred[3]=mean(RM$mean,na.rm=T) # on doit d'abord enlever les films qui n'ont pas été noté ie qui ont une moyenne = a Nan avant de prendre la moyenne generale
    pred[4]=RU$mean[userID=userID]
    pred[5]=RM$mean[movieID=movieID]
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
