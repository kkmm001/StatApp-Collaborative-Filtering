matUS_matSV=function(mat.SVD,X){
  # il faut charger le package expm
  tot=sum(mat.SVD$d)
  k=1
  m=sum(mat.SVD$d[1:k])
  while(m/tot<X){
    k=k+1
    m=sum(mat.SVD$d[1:k])
  }
  Sk=diag(mat.SVD$d[1:k])
  
  
  # compute two resultant matrices: UkSk^(1/2) and Sk^(1/2)Vk
  sqr_Sk=sqrtm(Sk)
  Uk=mat.SVD$u[,1:k]
  Vk=mat.SVD$v[,1:k]
  res=list(US=Uk%*%sqr_Sk,SV=sqr_Sk%*%t(Vk))
  return(res)
}
