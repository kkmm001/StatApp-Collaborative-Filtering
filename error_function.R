error_function = function(vect1,vect2,method){
  # INPUT   vect1   : premier vecteur
  #         vect2   : second vecteur
  #         method  : méthode employée ('RMSE', 'MAD' ou '0-1') 
  # OUTPUT          : erreur entre les deux vecteurs
  
  if (length(vect1) != length(vect2)){
    warning("Erreur : les tailles des deux vecteurs ne correspondent pas")
  }
  
  else{
    switch(method,
           'RMSE' = sqrt(sum((vect1-vect2)^2)/length(vect1)), #erreur quadratique moyenne
           'MAD'  = sum(abs(vect1-vect2))/length(vect1),      #erreur absolue moyenne
           '0-1'  = sum(vect1-vect2 != 0)/length(vect1)       #erreur 0-1
    )
  }
}
