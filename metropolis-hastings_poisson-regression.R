#########################################################
#########################################################
##  
##  Model: y[i]~POIS(mu[i])
##         log(mu[i]) = B[0]+B[1]*x[i]
##         X~N(0,1)
##  
##  where B[0] = 1 and B[1] = 0.5
##  
##  Use a bivariate normal proposal density
##  B[0],B[1] ~ BVN(0,c*(1,0
##                       0,1))
##  
#########################################################
#########################################################

##  Needed Libraries  -----------------------------------

library(mvtnorm)
library(ggplot2)

##  Create given data -----------------------------------

x=c();y=c()
x=rnorm(1000,0,1)
muInitSim = exp(1+0.5*x)
for (i in 1:1000){
  y[i] = rpois(1,muInitSim[i])
}

# Make sure data is what we want it to be
par(mfrow=c(2,2))
hist(y);hist(x)
plot(x,y);plot(x,log(muInitSim))

##  Metropolis-Hastings Algorithm -----------------------

allRatios=c()                                             # track ratios
betaCurrent = matrix(c(0,0),1,2)                          # init values
iter=1000                                                 # number of iterations
betas=matrix(NA,nrow=iter,ncol=2)                         # init beta matrix
accepts=0                                                 # track number of accepted values to get acceptance rate
proposedCov = (1/1000)*matrix(c(1,0,0,1),nrow=2,ncol=2)   # covariance matrix. `c` is a tuning param

for(i in 1:iter){
  betaProposed = rmvnorm(1,betaCurrent,proposedCov)
  
  logRatio = sum(dpois(y,exp(betaProposed[,1])*exp(x*betaProposed[,2]),log=T)) -
              sum(dpois(y,exp(betaCurrent[,1])*exp(betaCurrent[,2]*x),log=T)) +
              sum(dnorm(betaCurrent,c(0,0),c(10^3,10^3),log=T)) - 
              sum(dnorm(betaProposed,c(0,0),c(10^3,10^3),log=T))

  expRatio = exp(logRatio)
  allRatios[i] = expRatio
  
  if (expRatio >= runif(1)){
    betaCurrent = betaProposed
    accepts=accepts+1
  }
  betas[i,]=betaCurrent
}

##  Results ---------------------------------------------

cat("\tBeta 0: ", betas[iter,1],"\n",
	"Beta 1: ", betas[iter,2],"\n",
	"Accept Rate:", accepts/iter*100,"%")
	
## Plots  -----------------------------------------------

iteration=seq(1,iter,1)
beta0_plot = qplot(iteration,betas[,1],geom=c("point","smooth")) +
              geom_hline(y=1,size=.8,color="red",alpha=0.4) +
              ggtitle(expression(""*beta[0]*" Times Series Plot"))+
              theme(plot.title=element_text(family="serif",
	              		lineheight=0.8,
	              		face="bold",
	              		size=28)) +
              ylab(expression(beta[0])) + xlab("Iteration") + 
              theme(axis.title.x=element_text(size=18),
              		axis.title.y=element_text(size=24))

beta1_plot = qplot(iteration,betas[,2],geom=c("point","smooth")) +
              geom_hline(y=0.5,size=.8,color="red",alpha=0.4) +
              ggtitle(expression(""*beta[1]*" Times Series Plot"))+
              theme(plot.title=element_text(family="serif",
					lineheight=0.8,
					face="bold",
					size=28)) +
              ylab(expression(beta[1])) + xlab("Iteration") + 
              theme(axis.title.x=element_text(size=18),
              		axis.title.y=element_text(size=24))

beta0_plot
beta1_plot
