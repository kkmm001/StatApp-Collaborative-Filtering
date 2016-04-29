filtrer_similarite = function(mat.sim0, mat.InCommon, nbMin.InCommon){
  # INPUT   mat.sim0        : la matrice des similarités d'origine, sans filtre, calculé à partir de proxi_Users_AllvsAll.R
  #         mat.InCommon    : la matrice contenant le nombre de films vus en commun
  #         nbMin.InCommon  : le seuil du nombre de films vus en commun
  # OUTPUT                  : la matrice des similarités filtrée
  
  mat.sim_filtre = mat.sim0
  mat.sim_filtre[mat.InCommon < nbMin.InCommon] = NA
  
  return(mat.sim_filtre)
}