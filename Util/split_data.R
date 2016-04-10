split_data = function(data.Ratings, nb.Tests){
  # INPUT   data.Ratings  : la base des notes
  #         nb.Tests      : le nombre de test de validation croisée
  # OUTPUT  list.Datasets : la liste des nb.Tests sub-datasets de data.Ratings

  set.seed(42)
  
  # Génération d'un vecteur aléatoire
  alea = runif(nrow(data.Ratings))

  # Découpage de la base des notes en nb.Tests
  list.Datasets = list()
  
  for(i in 1:nb.Tests){
    if (i==1){
      list.Datasets[[i]] = subset(data.Ratings, (alea<quantile(alea,1/nb.Tests)))
    }
    else if(i==nb.Tests){
      list.Datasets[[i]] = subset(data.Ratings, (alea>=quantile(alea,(nb.Tests-1)/nb.Tests)))
    }
    else{
      list.Datasets[[i]] = subset(data.Ratings, (alea<quantile(alea,i/nb.Tests))&(alea>=quantile(alea,(i-1)/nb.Tests)))
    }
  }
  
  return(list.Datasets) 

  }
