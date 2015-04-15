A = c(1,3,5,7,9)                              # index
P = c(0.2,0.1,0.3,0.25,0.15)                  # probabilities
n = length(A)                                 # get length
P = n*P                                       # define alias
Sm = which(P<1)                               # get small numbers
Lr = which(P>=1)                              # get large numbers
Pr = matrix(0,1,n)                            # set probability table
Al = matrix(0,1,n)                            # set alias table

while(length(Sm) > 0 && length(Lr) > 0){
  l = Sm[1]
  Sm = Sm[Sm!=Sm[1]]
  g = Lr[1]
  Lr = Lr[Lr!=Lr[1]]
  
  Al[l] = g
  
  P[g] = (P[g]+P[l])-1
  if(P[g] < 1){
    Sm = c(Sm, g)
  }else{
    Lr = c(Lr, g)
  }
}
while(length(Lr) > 0){
  g = Lr[1]
  Lr = Lr[Lr!=Lr[1]]
  P[g] = 1
}
while(length(Sm) > 0){
  l = Sm[1]
  Sm = Sm[Sm!=Sm[1]]
  P[l] = 1
}

j=1
R=c()
for(j in 1:100){
  dieRoll = round(runif(1,1,5))
  coinProb = P[dieRoll]
  coinToss = rbinom(1,2,coinProb)
  if(coinToss == 2){
    R = c(R, dieRoll)
  }else{
    R = c(R, Al[dieRoll])
  }
  j=j+1
}
R
breaks=seq(0, n, 1)
hist(R,breaks=breaks,main='Alias Method Sampling (Histogram)',xlab='X',
	ylab='counts')
