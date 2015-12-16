# DECOUPAGE DE LA BASE EN 5 BASES DE MEME TAILLE 

# Clean up
rm(list=ls()) 
cat("\014") 

data.Ratings = read.table(file=file.choose(),header=F,colClasses = c(V4 = "NULL"))
colnames(data.Ratings) = c("userID", "movieID", "rating")

set.seed(1000) # avant de genere le vecteur aleatoire toujours effectuer cette commende comme ca on a tyoujours le meme resulatt
vect.Alea=runif(nrow(data.Ratings))
data.Ratings=cbind(data.Ratings,vect.Alea)
colnames(data.Ratings) = c("userID", "movieID", "rating","alea")
summary(vect.Alea)

n=5

i=1
U1=subset(data.Ratings, (alea<quantile(vect.Alea,i/n))&(alea>=quantile(vect.Alea,(i-1)/n)))[,-c(4)]

i=2
U2=subset(data.Ratings, (alea<quantile(vect.Alea,i/n))&(alea>=quantile(vect.Alea,(i-1)/n)))[,-c(4)]

i=3
U3=subset(data.Ratings, (alea<quantile(vect.Alea,i/n))&(alea>=quantile(vect.Alea,(i-1)/n)))[,-c(4)]

i=4
U4=subset(data.Ratings, (alea<quantile(vect.Alea,i/n))&(alea>=quantile(vect.Alea,(i-1)/n)))[,-c(4)]

i=5
U5=subset(data.Ratings, (alea<=quantile(vect.Alea,i/n))&(alea>=quantile(vect.Alea,(i-1)/n)))[,-c(4)]

rm(data.Ratings,vect.Alea)

#Penser à sauvegarder les bases U1, U2, U3, U4 et U5

#U=list()

#for(i in 1:n)
#{
#  U[i]=subset(Ratings, alea<quantile(vect.Alea,i/n),alea>quantile(vect.Alea,(i-1)/n))
#}

# r=cbind(Ratings,new1 = mapply(function(x) (x<quantile(vect.Alea,i/n))*(x>quantile(vect.Alea,(i-1)/n)), Ratings$alea) )


