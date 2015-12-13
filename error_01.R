error_01 = function(vect1, vect2){
  # error_01 calcule l'erreur 0-1 entre les vecteurs vect1 et vect2 lorsqu'ils ont la même dimension.
  
  if (length(vect1) == length(vect2)){
    return(sum(vect1-vect2 != 0)/length(vect1))
  }
  else{
    return(cat("Erreur : les tailles des deux vecteurs ne correspondent pas"))
  }
}
