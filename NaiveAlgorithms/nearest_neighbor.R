#=============== Par defaut on prends la similarité de Pearson / autre similarite possible distance euclidienne sinon message d'erreur ===========================

nearest_neighbor= function(user_ID,data.Ratings, similarity=P){
  if(similarity=P){
  Mat= proxi_UsersComplet_Pearson(user_ID,data.Ratings)
  g=max ifelse(similarity=S,Mat=t.dist_euclid = proxi_UsersComplet_Pearson(user_ID,data.Ratings),Mat=null))
  if(Mat=NULL){break}
  colnames(Mat) = c("ID","corcoef")
  Mat = as.data.frame(Mat)
  
  Mat = Mat[apply(Mat,1,function(Mat) !any(is.na(Mat))),]
  if(similarity=P,near_n = Mat[Mat[2] == max(Mmat.cor_pearson[2]),]
  return (near_n_P)
}

#============================ 2. Du point de vue de la distance euclidienne  ==============================

nearest_neighborE = function(user_ID,data.Ratings){
  mat.dist_euclid = proxi_UsersComplet_Pearson(user_ID,data.Ratings)
  colnames(mat.dist_euclid) = c("ID","corcoef")
  mat.dist_euclid = as.data.frame(mat.dist_euclid)
  mat.dist_euclid = mat.dist_euclid[apply(mat.dist_euclid,1,function(mat.dist_euclid) !any(is.na(mat.dist_euclid))),]
  near_n_E = mat.dist_euclid[mat.dist_euclid[2] == min(mat.dist_euclid[2]),]
  return (near_n_E)
}
