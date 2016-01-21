# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#       Fichier : Decoupage_base.R
#       Description : DECOUPAGE DE LA BASE EN 5 SOUS-BASES DE MEME TAILLE
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# ===================================== 1.PREAMBULE ===============================================

set.seed(1000) # avant de genere le vecteur aleatoire toujours effectuer cette commande comme ca on a toujours le meme resulat
vect.Alea=runif(nrow(data.Ratings))
data.Ratings=cbind(data.Ratings,vect.Alea)
colnames(data.Ratings) = c("userID", "movieID", "rating","alea")

# ===================================== 2.DECOUPAGE PROPREMENT DIT ===============================================

n=5

U = list()
for(i in 1:n)
{
  U[[i]] = subset(data.Ratings, (alea<=quantile(vect.Alea,i/n))&(alea>=quantile(vect.Alea,(i-1)/n)))[,-c(4)]
}
rm(vect.Alea)

# ===================================== 3.RECONSTITUTION DES BASES (APPRENTISSAGES ET TESTS) ===============================================

# On prend U1 comme base test

TrainingU1 = do.call("rbind",U[c(2:5)])
TestU1 = U[[1]]

# On prend U2 comme base test

TrainingU2 = do.call("rbind",U[c(1,3:5)])
TestU2 = U[[2]]

# On prend U3 comme base test

TrainingU3 = do.call("rbind",U[c(1:2,4:5)])
TestU3 = U[[3]]

# On prend U4 comme base test

TrainingU4 = do.call("rbind",U[c(1:3,5)])
TestU4 = U[[4]]

# On prend U5 comme base test

TrainingU5 = do.call("rbind",U[c(1:4)])
TestU5 = U[[5]]

