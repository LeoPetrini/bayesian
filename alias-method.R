# alias method

X = c(1,3,5,7,9)								# index
p = c(0.2,0.1,0.3,0.25,0.15)							# probabilities
n = length(X)									# get length
K = matrix(c(X,p), nrow=2, ncol=n, byrow=TRUE)					# define K, our distribution (pmf)
A = matrix(data=NA, nrow=1, ncol=n)						# initiate A vector will null data
aliasK = matrix(c(X,n*p), nrow=2, ncol=n, byrow=TRUE)				# create alias table

# this needs to be put into a loop of some kind 

indexMinK = which.min(aliasK[2,])						# find the min in the alias table 
differenceMin = abs( min(aliasK[2,])-1 )					# get the difference of the min and 1
aliasK[2,indexMinK] = 1								# set the min to 1
indexMaxK = which.max(aliasK[2,])						# find the max in the alias table
aliasK[2,indexMaxK] = indexMaxK - differenceMin					# reduce max by amount from abs(min-1)
A[indexMaxK] = aliasK[1,indexMaxK]						# set a_i equal to X of the largest p

# repeat above until p row is all 1

a = A[1,]									# define a vector
w = n*p										# define w vector

U = runif(1,0,1)								# generate random uniform number
v = n*U+1									# define v, it lies between (1, n+1)
int = floor(v)									# make v an integer
r = v-int									# get difference
if(r <= w[int]){
  Z[i] = int
}
else{
  Z[i] = a[int]
} 
