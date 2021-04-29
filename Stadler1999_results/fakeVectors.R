# returns a data frame of two variables which correlate with a population correlation of rho
# If desired, one of both variables can be fixed to an existing variable by specifying x
getBiCop <- function(n, rho, mar.fun=rnorm, x = NULL, ...) {
  if (!is.null(x)) {X1 <- x} else {X1<-rnorm(n = n,mean = 0,sd = (1/n))}#{X1 <- mar.fun(n, ...)}
  if (!is.null(x) & length(x) != n) warning("Variable x does not have the same length as n!")
  
  C <- matrix(rho, nrow = 2, ncol = 2)
  diag(C) <- 1
  
  C <- chol(C)
  
  #X2 <- mar.fun(n)
  X2 <- rnorm(n = n,mean = 0,sd = (1/n))
  X <- cbind(X1,X2)
  
  # induce correlation (does not change X1)
  df <- X %*% C
  
  
  ## if desired: check results
  #all.equal(X1,X[,1])
  #cor(X)
  
  return(df)
}

#x<-(getBiCop(n=100000, rho=.1))
#cor(x)

getSemanticCluster <- function(tword,min,max,numInCluster,vecLen){
  words<-c()
  for(i in 1:numInCluster){
    words<-c(words,getBiCop(n = vecLen,rho = runif(n = 1,min = min,max = max),x = critWord)[,2])
  }
  return(words)
}

library(LSAfun)

vecLength<-1000
numDistractors<-15
min<-.2
max<-.5
critWord<-rnorm(n = vecLength,mean = 0,sd = 1/vecLength)
distactWord1<-rnorm(n = vecLength,mean = 0,sd = 1/vecLength)
distactWord2<-rnorm(n = vecLength,mean = 0,sd = 1/vecLength)
distactWord3<-rnorm(n = vecLength,mean = 0,sd = 1/vecLength)
distactWord4<-rnorm(n = vecLength,mean = 0,sd = 1/vecLength)
distactWord5<-rnorm(n = vecLength,mean = 0,sd = 1/vecLength)
distactWord6<-rnorm(n = vecLength,mean = 0,sd = 1/vecLength)
distactWord7<-rnorm(n = vecLength,mean = 0,sd = 1/vecLength)
distactWord8<-rnorm(n = vecLength,mean = 0,sd = 1/vecLength)
distactWord9<-rnorm(n = vecLength,mean = 0,sd = 1/vecLength)

M<-matrix(data = c(
  # target semantic cluster
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  # getBiCop(n = vecLength,rho = runif(n = 1,min = min,max = max),x = critWord)[,2],
  getSemanticCluster(tword = critWord,min = min,max = max,numInCluster = numDistractors,vecLen = vecLength),
  getSemanticCluster(tword = distactWord1,min = min,max = max,numInCluster = numDistractors,vecLen = vecLength),
  distactWord1,
  getSemanticCluster(tword = distactWord2,min = min,max = max,numInCluster = numDistractors,vecLen = vecLength),
  distactWord2,
  getSemanticCluster(tword = distactWord3,min = min,max = max,numInCluster = numDistractors,vecLen = vecLength),
  distactWord3,
  getSemanticCluster(tword = distactWord4,min = min,max = max,numInCluster = numDistractors,vecLen = vecLength),
  distactWord4,
  getSemanticCluster(tword = distactWord5,min = min,max = max,numInCluster = numDistractors,vecLen = vecLength),
  distactWord5,
  getSemanticCluster(tword = distactWord6,min = min,max = max,numInCluster = numDistractors,vecLen = vecLength),
  distactWord6,
  getSemanticCluster(tword = distactWord7,min = min,max = max,numInCluster = numDistractors,vecLen = vecLength),
  distactWord7,
  getSemanticCluster(tword = distactWord8,min = min,max = max,numInCluster = numDistractors,vecLen = vecLength),
  distactWord8,
  getSemanticCluster(tword = distactWord9,min = min,max = max,numInCluster = numDistractors,vecLen = vecLength),
  distactWord9,
  
  rnorm(n = vecLength*1000,mean = 0,sd = 1/vecLength),
  critWord
),ncol = vecLength,byrow = T)

rownames(M)<-1:nrow(M)

# results<-rep(0,nrow(M))
# for(i in 1:nrow(M)){
#   results[i]<-(cosine(M[i,],critWord))
# }
# 
# barplot(results,ylim = c(0,1))



