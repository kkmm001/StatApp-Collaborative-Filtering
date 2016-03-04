#=============== Par defaut on prends la similarité de Pearson / autre similarite possible distance euclidienne sinon message d'erreur ===========================

nearest_neighbor= function(user_ID,data.Ratings, similarity=P){
  if(similarity=P){
    Mat= proxi_UsersComplet_Pearson(user_ID,data.Ratings)
    g=max}
  else{
    if(similarity=S){
      Mat=t.dist_euclid = proxi_UsersComplet_Pearson(user_ID,data.Ratings)
      g=min}
    else{
      Mat=null
    }
  }
  if(Mat=NULL){break}
  colnames(Mat) = c("ID","corcoef")
  Mat = as.data.frame(Mat)
    Mat = Mat[apply(Mat,1,function(Mat) !any(is.na(Mat))),]
  near_n = Mat[Mat[2] == g(Mat[2]),]
  return (near_n)
}
