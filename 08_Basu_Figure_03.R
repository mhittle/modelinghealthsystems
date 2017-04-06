n = 5000000
chars=c(75, 130, 25, 108)
sds = c(10, 25, 5, 20)
cormat = matrix(c( 1.000,	 0.302, 	0.571, 	 0.776,
                   0.302, 1.000,	 0.110, 	 0.587,
                   0.571,	 0.110, 	1.000 ,	 0.002,
                   0.776,	 0.587,	 0.002,	 1.000 ),ncol=4,byrow=TRUE) 
#install.packages('MASS')  
#install.packages('MBESS')   
library(MASS)  
library(MBESS)      
sigma   =cor2cov(cormat,sds)    
pop     =mvrnorm(n,chars,sigma)              
head(pop, 10)

#install.packages('matrixStats')
library(matrixStats)
colMeans(pop)
colSds(pop)
cor(pop)
pairs(~pop[1:1000,1]+pop[1:1000,2]+pop[1:1000,3]+pop[1:1000,4])

dm = (pop[,4]>=126)
mean(dm)

survey1pos = (pop[,1]>80)
survey2pos = (pop[,1]>80)&(pop[,2]>=140)
survey3pos = (pop[,2]>=150)&(pop[,3]>=30)

table(survey1pos)
table(survey2pos)
table(survey3pos)
table(survey1pos)/n
table(survey2pos)/n
table(survey3pos)/n

sens1 = sum((survey1pos==1)&(dm==1))/sum(dm==1)
sens2 = sum((survey2pos==1)&(dm==1))/sum(dm==1)
sens3 = sum((survey3pos==1)&(dm==1))/sum(dm==1)

spec1 = sum((survey1pos==0)&(dm==0))/sum(dm==0)
spec2 = sum((survey2pos==0)&(dm==0))/sum(dm==0)
spec3 = sum((survey3pos==0)&(dm==0))/sum(dm==0)

sens1
sens2
sens3
spec1
spec2
spec3

install.packages('Epi')
library(Epi)
ROC(survey1pos,dm)

