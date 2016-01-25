error_function = function(vect1,vect2,method){
  # INPUT   vect1   : premier vecteur
  #         vect2   : second vecteur
  #         method  : methode employée ('RMSE', 'MAE' ou '01') 
  # OUTPUT          : erreur entre les deux vecteurs
  
  if (length(vect1) != length(vect2)){
    warning("Erreur : les tailles des deux vecteurs ne correspondent pas")
  }
  
  else{
    switch(method,
           'RMSE' = sqrt(sum((vect1-vect2)^2)/length(vect1)),  #racine de l'erreur quadratique moyenne
           'MAE'  = sum(abs(vect1-vect2))/length(vect1),       #erreur absolue moyenne
           '01'   = sum(vect1-vect2 != 0)/length(vect1)        #erreur 01 (rajout d'un intervalle ?)
    )
  }
}
