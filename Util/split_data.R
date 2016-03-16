# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#       Fichier : split_data.R
#       Description : DECOUPAGE DE LA BASE EN n SOUS-BASES DE MEME TAILLE
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

#Choix des paramètres
set.seed(100)
n = 5

base = data.Ratings
alea = runif(nrow(base))
base = cbind(base,alea)

#Decoupage de la base en n
U = list()
for(i in 1:n){
  if (i==1){
    U[[1]] = subset(base, (alea<quantile(alea,1/n)))[,-c(length(base))]
  }
  else if(i==n){
    U[[n]] = subset(base, (alea>=quantile(alea,(n-1)/n)))[,-c(length(base))]
  }
  else{
    U[[i]] = subset(base, (alea<quantile(alea,i/n))&(alea>=quantile(alea,(i-1)/n)))[,-c(length(base))]
  }
}

all_ind = 1:n

#Creation des bases d'apprentissage et de test
for(ind in 1:n){
  ind_row = all_ind[all_ind != ind]
  assign(paste0('TrainingU',ind),do.call("rbind",U[ind_row]))
  assign(paste0('TestU',ind),U[[ind]])
}

rm(base, alea,U, all_ind, i, ind, ind_row)
