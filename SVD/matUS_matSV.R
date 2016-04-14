matUS_matSV=function(SVD,X){

  tot=sum(SVD$d)
  k=1
  m=sum(SVD$d[1:k])
  while(m/tot<X){
    k=k+1
    m=sum(SVD$d[1:k])
  }
  Sk=diag(SVD$d[1:k])
  
  
  # compute two resultant matrices: UkSk^(1/2) and Sk^(1/2)Vk
  sqr_Sk=sqrtm(Sk)
  Uk=SVD$u[,1:k]
  Vk=SVD$v[,1:k]
  res=list(US=Uk%*%sqr_Sk,SV=sqr_Sk%*%t(Vk))
  return(res)
}
