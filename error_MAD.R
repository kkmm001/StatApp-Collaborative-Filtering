error_MAD = function(vect1, vect2){
  # error_MAD calcule l'erreur en valeur absolue entre les vecteurs vect1 et vect2 lorsqu'ils ont la même dimension.
  
  if (length(vect1) == length(vect2)){
    return(sum(abs(vect1-vect2))/length(vect1))
  }
  else{
    return(cat("Erreur : les tailles des deux vecteurs ne correspondent pas"))
  }
}
