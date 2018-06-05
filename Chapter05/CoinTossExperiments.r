library(compiler)    # allow for compilation of functions
library(rbenchmark)  # benchmarking tool


#------------------------------------------------------------------
# Test fastest way to generate synthetic coin tosses
#------------------------------------------------------------------

# verision 1
coinTossSample.1 = function(n, b=0.5) {
  sample( x=c(0,1), size=n, replace=TRUE, prob=c(1-b, b) )
}

# version 2
coinTossSample.2 = function(n, b=0.5) {
  0 + ( runif(n) < b )
}

#coinBias <- 0.8
#numFlips <- 10000
#benchmark(coinTossSample.1(numFlips, b=0.8), coinTossSample.2(numFlips, b=0.8), 
#          columns=c("test", "elapsed", "relative"),
#          order="relative", replications=5000)


#a <- coinTossSample.1(numFlips, coinBias)
#b <- coinTossSample.2(numFlips, coinBias)


#------------------------------------------------------------------
# Coin Toss : Updating Prior Example
#------------------------------------------------------------------

coinBias <- 0.8
numFlips <- 500
lenTheta <- 1000
flipVector <- coinTossSample.1(numFlips, coinBias)
  
Theta <- seq( 0 , 1 , length = lenTheta )

initMean <- 0.5
initSd <- 0.001
initPrior <- dnorm(x=Theta, mean=initMean, sd=initSd)
initPrior <- initPrior/sum(initPrior)

##
initPrior <- rep(1, lenTheta)
initPrior <- initPrior/sum(initPrior)

#initPrior[round(lenTheta/2,0)] <- 1

# slow
tmpOut <- matrix(0, nrow=numFlips, ncol=3)
tmpPrior <- initPrior
for (i in 1:numFlips) {
  
  z <- sum( flipVector[1:i] )
  N <- i
  
  pDataGivenTheta <- Theta^z * (1 - Theta)^(N-z)
  pData <- sum(pDataGivenTheta * tmpPrior)
  pThetaGivenData <- (pDataGivenTheta * tmpPrior)/ pData

  tmpPrior <- pThetaGivenData
  tmpOut[i, ] <- c(Theta[cumsum(tmpPrior) > 0.05][1], 
                   Theta[cumsum(tmpPrior) > 0.50][1],
                   Theta[cumsum(tmpPrior) > 0.95][1])
  
}

plot(Theta, initPrior)

plot(Theta, pDataGivenTheta)

plot(Theta, tmpPrior)


plot(1:numFlips, tmpOut[,3], ylim=c(0,1), type="l", log="x")
points(1:numFlips, tmpOut[,2], type="p")
points(1:numFlips, tmpOut[,1], type="l")

#------------------------------------------------------------------
# Create summary values of Data
#------------------------------------------------------------------
z = sum( Data ) # number of 1's in Data
N = length( Data ) 

#------------------------------------------------------------------
# Compute the Bernoulli likelihood at each value of Theta:
#------------------------------------------------------------------
pDataGivenTheta = Theta^z * (1-Theta)^(N-z)

#------------------------------------------------------------------
# Compute the evidence and the posterior via Bayes' rule:
#------------------------------------------------------------------
pData = sum( pDataGivenTheta * pTheta )
pThetaGivenData = pDataGivenTheta * pTheta / pData
