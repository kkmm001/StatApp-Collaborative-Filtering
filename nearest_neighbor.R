#============================= 1. Du point de vue du coefficient de corrélation de Pearson ===========================

nearest_neighborP = function(user_ID,data.Ratings){
  mat.cor_pearson = proxi_UsersComplet_Pearson(user_ID,data.Ratings)
  colnames(mat.cor_pearson) = c("ID","corcoef")
  mat.cor_pearson = as.data.frame(mat.cor_pearson)
  mat.cor_pearson = mat.cor_pearson[apply(mat.cor_pearson,1,function(mat.cor_pearson) !any(is.na(mat.cor_pearson))),]
  near_n_P = mat.cor_pearson[mat.cor_pearson[2] == max(mat.cor_pearson[2]),]
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
