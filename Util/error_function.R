error_function = function(vect1,vect2,method){
  # INPUT   vect1   : premier vecteur
  #         vect2   : second vecteur
  #         method  : methode employée ('RMSE', 'MAE' ou '01') 
  # OUTPUT          : erreur entre les deux vecteurs
  
  library(hydroGOF)
  
  if (length(vect1) != length(vect2)){
    warning("Erreur : les tailles des deux vecteurs ne correspondent pas")
  }
  
  else{
    switch(method,
           'RMSE' = rmse(vect1, vect2),  #racine de l'erreur quadratique moyenne
           'MAE'  = mae(vect1, vect2),       #erreur absolue moyenne
           '01'   = sum(vect1-(round(vect2,0)) != 0, na.rm = TRUE)/length(vect1)  #approximation     #erreur 01 (rajout d'un intervalle ?)
    )
  }
}
