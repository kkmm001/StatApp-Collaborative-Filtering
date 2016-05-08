matUS_matSV = function(list.SVD, tau){
  # INPUT   list.SVD  : décomposition SVD d'une matrice
  #         tau       : taux d'inertie à conservé (entre 0 et 100)
  # OUTPUT            : 
  # Pré-requis : charger le package expm pour avoir accès a la fonction square root d'une matrice
  
  # Décomposition SVD
  mat.U = list.SVD$u
  mat.V = list.SVD$v
  vect.S = list.SVD$d
  mat.S = diag(vect.S) # matrice diagonale
  
  # Détermination du rang k de la matrice lorsque le taux d'inertie vaut tau
  totalInertie = sum(vect.S)
  
  k=1
  inertie = sum(vect.S[1:k])
  
  while(inertie/totalInertie < tau/100){
    k = k+1
    inertie = sum(vect.S[1:k])
  }
  #Sk=diag(list.SVD$d[1:k],k)
  
  
  # Calcul de la racine carré de la matrice mat.S de rang k
  mat.S_k = mat.S[1:k, 1:k] # matrice diagonale de rang k
  mat.Rac_S_k= mat.S_k^0.5 # matrice diagonale
  
  mat.U_k = mat.U[,1:k]
  mat.V_k = mat.V[,1:k]
  
  if(k != 1){
  matUS_matSV = list(US = mat.U_k %*% mat.Rac_S_k,
                     SV = mat.Rac_S_k %*% t(mat.V_k)
                     )
  }else{
    matUS_matSV = list(US = mat.U_k * mat.Rac_S_k,
                       SV = mat.Rac_S_k * t(mat.V_k)
    )
  }
  
  return(matUS_matSV)
}
