error_MSE = function(vect1, vect2){
  # error_MSE calcule l'erreur quadratique moyenne entre les vecteurs vect1 et vect2 lorsqu'ils ont la même dimension.
  
  if (length(vect1) == length(vect2)){
    return(sum((vect1-vect2)^2)/length(vect1))
  }
  else{
    return(cat("Erreur : les tailles des deux vecteurs ne correspondent pas"))
  }
}
