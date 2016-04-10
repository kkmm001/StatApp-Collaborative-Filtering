error_function = function(vect1, vect2, method){
  # INPUT   vect1   : premier vecteur
  #         vect2   : second vecteur
  #         method  : méthode employée ('RMSE', 'MAE' ou '01') 
  # OUTPUT          : erreur entre les deux vecteurs
  
  # TODO : la métrique '01' est bugguée, elle retourne 'les mauvaises valeurs'NA' si un des vecteur contient des NA
  # tests unitaires à vérifier : 
  # error_function(c(2,3,5), c(2,3,5), '01') == 0
  # error_function(c(2,3.3,5.2), c(2.2,3,4.8), '01') == 0
  # error_function(c(2,3,6), c(2,3,5), '01') == 1/3
  # error_function(c(2.7,3,5), c(2,3,6), '01') == 2/3
  # error_function(c(2,3,NA), c(2,3,5), '01') == 0
  # error_function(c(2,NA,5), c(2,3,6), '01') == 1/2
  # error_function(c(2,NA,5), c(2,NA,5), '01') == 0
  
  library(hydroGOF)
  
  if (length(vect1) != length(vect2)){
    warning("Erreur : les tailles des deux vecteurs ne correspondent pas")
  }
  
  else{
    switch(method,
           'RMSE' = rmse(vect1, vect2),  # racine de l'erreur quadratique moyenne
           'MAE'  = mae(vect1, vect2),   # erreur absolue moyenne
           '01'   = sum( round(vect1,0) != round(vect2,0) ), na.rm = TRUE)/(length(vect1)  # approximation de l'erreur 01
    )
  }
}
