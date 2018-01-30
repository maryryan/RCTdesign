####################
######################## RCT DESIGN SIMULATIONS
######################## MARY RYAN
####################

library(RCTdesign)

#### OBF design, J=4 ####
## H0: theta=0
## H1: theta=-0.7
## N=1700 (850 per arm)

OBF.j4.H0.2 <- seqDesign(
   prob.model= "proportions", #distribution 
   arms= 2, #2 trt arms
   ratio= c(1,1), #how the total sample size is distributed between the arms
   nbr.analyses=4,
   sample.size = 1700,
   null.hypothesis= 0.3, #no differences in trt
   alt.hypothesis= 0.23,
   variance="null",
   test.type= "two.sided",
   size= 0.05, #alpha
   power="calculate"
)

OBF.j4.H1.2 <- update(OBF.j4.H0.2, variance="alternative")
OBF.j4.H0.less <- update(OBF.j4.H0.2, test.type="less")
OBF.j4.H1.less <- update(OBF.j4.H0.less, variance="alternative")
OBF.j4.H0.2.fut <- update(OBF.j4.H0.2, early.stopping="both")


J1 <-425


J <- c(J1*1:4)

## STOPPING BOUNDARIES UNDER 4 SCENARIOS ##
stop.eff.H0.2 <- OBF.j4.H0.2$boundary[,1]
stop.fut.H0.2 <- OBF.j4.H0.2$boundary[,4]

stop.eff.H1.2 <- OBF.j4.H1.2$boundary[,1]
stop.fut.H1.2 <- OBF.j4.H1.2$boundary[,4]

stop.eff.H0.less <- OBF.j4.H0.less$boundary[,1]
stop.fut.H0.less <- OBF.j4.H0.less$boundary[,2]

stop.eff.H1.less <- OBF.j4.H1.less$boundary[,1]
stop.fut.H1.less <- OBF.j4.H1.less$boundary[,2]

Ni <- 850
N <- 1700
H0 <- 0.3
H1 <- 0.23


nsims <- 100000
observed <- matrix(NA, ncol=nsims, nrow=length(J))


for(i in 1:nsims){
   ## SIMULATE DATA ##
   control <- rbinom(Ni, 1, 0.3)
   trt <- rbinom(Ni, 1, H1)
   
   ## FIND THE OBSEVRED DIFFERENCE IN PROPORTIONS AT THE 4 ANALYSIS POINTS ##
   for( j in seq( length(J)-1 ) ){
      observed[j,i] <- ( mean(trt[seq(J[j]/2)]) - mean(control[seq(J[j]/2)]) )
      
   }
   observed[4,i] <- ( mean(trt) - mean(control) )
  
}

## WHEN DO WE CROSS THE BOUNDARIES? ##
finalTrtEffect <- rep(NA, nsims)

for( i in seq( length(J) ) ){
   finalTrtEffect <- ifelse( is.na(finalTrtEffect) & (observed[i,] < stop.eff.H1.2[i] | observed[i,] > stop.fut.H1.2[i]),
                        observed[i,], finalTrtEffect )
}
finalTrtEffect <- ifelse( is.na(finalTrtEffect), observed[4,], finalTrtEffect )
mean.est <- mean(finalTrtEffect)

H1.less <- density(finalTrtEffect)
xlabel <- "Estimated Treatment Effect"

## MY SPIKEY BOIS ##
par(mfrow=c(2,2))
plot(H0.2, xlab=xlabel, main="Two-sided Test Under H0", cex.main=0.8)
plot(H1.2, xlab=xlabel, main="Two-sided Test Under H1", cex.main=0.8)
plot(H0.less, xlab=xlabel, main="One-sided Test Under H0", cex.main=0.8)
plot(H1.less, xlab=xlabel, main="One-sided Test Under H1", cex.main=0.8)
mtext("OBF J=4", outer=T, line=-20, cex=1.7)



#### VECTORIZED SIMULATIONS ####
library(RCTdesign)

## OBF design, J=4 ##
## H0: theta=0
## H1: theta=-0.7
## N=1700 (850 per arm)

OBF.j4.H0.2 <- seqDesign(
   prob.model= "proportions", #distribution 
   arms= 2, #2 trt arms
   ratio= c(1,1), #how the total sample size is distributed between the arms
   nbr.analyses=4,
   sample.size = 1700,
   null.hypothesis= 0.3, #no differences in trt
   alt.hypothesis= 0.23,
   variance="null",
   test.type= "two.sided",
   size= 0.05, #alpha
   power="calculate"
)

OBF.j4.H1.2 <- update(OBF.j4.H0.2, variance="alternative")
OBF.j4.H0.less <- update(OBF.j4.H0.2, test.type="less")
OBF.j4.H1.less <- update(OBF.j4.H0.less, variance="alternative")
OBF.j4.H0.2.fut <- update(OBF.j4.H0.2, early.stopping="both")

## STOPPING BOUNDARIES UNDER 4 SCENARIOS ##
stop.eff.H0.2 <- OBF.j4.H0.2$boundary[,1]
stop.fut.H0.2 <- OBF.j4.H0.2$boundary[,4]

stop.eff.H1.2 <- OBF.j4.H1.2$boundary[,1]
stop.fut.H1.2 <- OBF.j4.H1.2$boundary[,4]

stop.eff.H0.less <- OBF.j4.H0.less$boundary[,1]
stop.fut.H0.less <- OBF.j4.H0.less$boundary[,2]

stop.eff.H1.less <- OBF.j4.H1.less$boundary[,1]
stop.fut.H1.less <- OBF.j4.H1.less$boundary[,2]

J1 <-425
J <- c(J1*1:4)
J.len <- length(J)

Ni <- 850
N <- 1700
H0 <- 0.3
H1 <- 0.23

nsims <- 100000
#observed <- matrix(NA, ncol=nsims, nrow=length(J))


## SIMULATE DATA ##
data <- rep(list(cbind(rep(NA, Ni), rep(NA, Ni))), nsims)
data <- lapply(data, function(x){
   cbind(rbinom(Ni, 1, 0.3), rbinom(Ni, 1, H1))
   })

## FIND THE OBSEVRED DIFFERENCE IN PROPORTIONS AT THE 4 ANALYSIS POINTS ##
observed <- lapply(data, function(x){
   
   rbind( ( mean(x[sequence(J/2),2]) -  mean(x[sequence(J/2),1]) ) )

   
})


## WHEN DO WE CROSS THE BOUNDARIES? ##
finalTrtEffect <- rep(NA, nsims)

finalTrtEffect <- ifelse( is.na(finalTrtEffect) & (observed - stop.eff.H1.less < 0 | observed - stop.fut.H1.less > 0),
                             observed, finalTrtEffect )

finalTrtEffect <- ifelse( is.na(finalTrtEffect), observed[4,], finalTrtEffect )

mean.est <- mean(finalTrtEffect)






#### CONFIDENCE INTERVALS: FIXED NORMAL TEST RUN ####
mean.true <- 0

H1.CI.normLower <- seq(0, -5, by=-0.05)
H1.CI.normUpper <- seq(0, 5, by=0.05)

for( i in seq( length(H1.CI.normLower) ) ){
   
   upper <- qnorm(.975, H1.CI.normLower[i], 1)

   if(mean.true <= (upper + 0.05) & mean.true >= (upper - 0.05) ){
      CI.lower <- H1.CI.normLower[i]
      break
   }
   
}

for( i in seq( length(H1.CI.normUpper) ) ){
   
   lower <- qnorm(.025, H1.CI.normUpper[i], 1)
   
   if(mean.true >= (lower - 0.05) & mean.true <= (lower + 0.05) ){
      CI.Upper <- H1.CI.normUpper[i]
      break
   }
   
}

#### CONFIDENCE INTERVALS: FIXED SIZE SEQDESIGN TEST RUN ####
OBF.FIXED <- seqDesign(
   prob.model= "proportions", #distribution 
   arms= 2, #2 trt arms
   ratio= c(1,1),
   sample.size = 1700,
   null.hypothesis= 0.3, #no differences in trt
   alt.hypothesis= 0.23,
   variance="alternative",
   test.type= "two.sided",
   size= 0.05, #alpha
   power="calculate"
)

OBF.FIXED.lower <- seqBoundary(OBF.FIXED)[,1]
OBF.FIXED.upper <- seqBoundary(OBF.FIXED)[,4]


Ni <- 850
N <- 1700
H0 <- 0.3
#true H1 is 0.23, so want -0.2 and 0.2 on either side
H1.lowerBound <- seq(0.23, 0.03, by= -0.01)
H1.upperBound <- seq(0.23, 0.53, by= 0.01)



nsims <- 100000
observed.lowerBound <- rep(list(rep( NA, nsims )), length(H1.lowerBound))
observed.upperBound <- rep(list(rep( NA, nsims )), length(H1.upperBound))
finalTrtEffect.lowerBound <- rep(list(rep( NA, nsims )), length(H1.lowerBound))
finalTrtEffect.upperBound <- rep(list(rep( NA, nsims )), length(H1.upperBound))


## UPPER BOUND ##
for( j in seq( length(H1.upperBound) ) ){
   for(i in 1:nsims){
      ## SIMULATE DATA ##
      control.upperBound <- rbinom(Ni, 1, 0.3)
      trt.upperBound <- rbinom(Ni, 1, H1.upperBound[j])
      
      observed.upperBound[[j]][i] <- ( mean(trt.upperBound) - mean(control.upperBound) )
      
   }


   finalTrtEffect.upperBound[[j]] <- ifelse( is.na(finalTrtEffect.upperBound[[j]]) & (observed.upperBound[[j]] < OBF.FIXED.upper | observed.upperBound[[j]] > OBF.FIXED.upper),
                             observed.upperBound[[j]], finalTrtEffect.upperBound[[j]] )

   #H1.upperDensity <- density(finalTrtEffect.upperBound[[j]])
   if( mean.est >= (quantile(finalTrtEffect.upperBound[[j]], 0.025, na.rm=T) - 0.005) & mean.est <= (quantile(finalTrtEffect.upperBound[[j]], 0.025, na.rm=T) + 0.005)){
      CI.upper <- mean(finalTrtEffect.upperBound[[j]], na.rm=T)
      break
   }
}

## LOWER BOUND ##
for( j in seq( length(H1.lowerBound) ) ){
   for(i in 1:nsims){
      ## SIMULATE DATA ##
      control.lowerBound <- rbinom(Ni, 1, 0.3)
      trt.lowerBound <- rbinom(Ni, 1, H1.lowerBound[j])
      
      observed.lowerBound[[j]][i] <- ( mean(trt.lowerBound) - mean(control.lowerBound) )
      
   }
   
   
   ## WHEN DO WE CROSS THE BOUNDARIES? ##
   finalTrtEffect.lowerBound[[j]] <- ifelse( is.na(finalTrtEffect.lowerBound[[j]]) & (observed.lowerBound[[j]] < OBF.FIXED.lower | observed.lowerBound[[j]] > OBF.FIXED.lower),
                                             observed.lowerBound[[j]], finalTrtEffect.lowerBound[[j]] )
   
   #H1.lowerDensity <- density(finalTrtEffect.lowerBound[[j]])
   if( mean.est <= (quantile(finalTrtEffect.lowerBound[[j]], 0.975, na.rm=T) + 0.005) & mean.est >= (quantile(finalTrtEffect.lowerBound[[j]], 0.975, na.rm=T) - 0.005)){
      CI.lower <- mean(finalTrtEffect.lowerBound[[j]], na.rm=T)
      break
   }
}




#### CONFIEDENCE INTERVALS: FOR REAL THIS TIME ####
stop.eff.H1.2 <- OBF.j4.H1.2$boundary[,1]
stop.fut.H1.2 <- OBF.j4.H1.2$boundary[,4]


Ni <- 850
N <- 1700
H0 <- 0.3
J <- c(425*1:4)
#true H1 is 0.23, so want -0.2 and 0.2 on either side
H1.lowerBound <- seq(0.23, 0.03, by= -0.01)
H1.upperBound <- seq(0.23, 0.53, by= 0.01)



nsims <- 100000
## YOU NEED TO PREP THESE IN ORDER FOR THEM TO FILL ##
observed.lowerBound <- rep(list(matrix(rep( NA, nsims ), ncol=nsims, nrow=length(J))), length(H1.lowerBound))
observed.upperBound <- rep(list(matrix(rep( NA, nsims ), ncol=nsims, nrow=length(J))), length(H1.upperBound))
finalTrtEffect.lowerBound <- rep(list(matrix(rep( NA, nsims ), ncol=nsims, nrow=length(J))), length(H1.lowerBound))
finalTrtEffect.upperBound <- rep(list(matrix(rep( NA, nsims ), ncol=nsims, nrow=length(J))), length(H1.upperBound))


## UPPER BOUND ##
for( j in seq( length(H1.upperBound) ) ){
   for(i in 1:nsims){
      ## SIMULATE DATA ##
      control.upperBound <- rbinom(Ni, 1, 0.3)
      trt.upperBound <- rbinom(Ni, 1, H1.upperBound[j])
      
      for( k in seq( length(J)-1 ) ){
         observed.upperBound[[j]][k,i] <- ( mean(trt.upperBound[seq(J[k]/2)]) - mean(control.upperBound[seq(J[k]/2)]) )
         
      }#k for end
      observed.upperBound[[j]][4,i] <- ( mean(trt.upperBound) - mean(control.upperBound) )
      
   }#i for end
   
   for( k in seq( length(J) ) ){
   ## WHEN DO WE CROSS THE BOUNDARIES? ##
      finalTrtEffect.upperBound[[j]] <- ifelse( is.na(finalTrtEffect.upperBound[[j]]) & (observed.upperBound[[j]][k,] < stop.eff.H1.2[k] | observed.upperBound[[j]][k,] > stop.fut.H1.2[k]),
                                                observed.upperBound[[j]][k,], finalTrtEffect.upperBound[[j]] )
   }# k for end
   finalTrtEffect.upperBound[[j]] <- ifelse( is.na(finalTrtEffect[[j]]), observed.upperBound[[j]][4,], finalTrtEffect[[j]] )
   #H1.upperDensity <- density(finalTrtEffect.upperBound[[j]])
   if( mean.est >= (quantile(finalTrtEffect.upperBound[[j]], 0.025, na.rm=T) - 0.005) & mean.est <= (quantile(finalTrtEffect.upperBound[[j]], 0.025, na.rm=T) + 0.005)){
      CI.upper <- mean(finalTrtEffect.upperBound[[j]], na.rm=T)
      break
   }#if end
}#j for end



## LOWER BOUND ##
for( j in seq( length(H1.lowerBound) ) ){
   for(i in 1:nsims){
      ## SIMULATE DATA ##
      control.lowerBound <- rbinom(Ni, 1, 0.3)
      trt.lowerBound <- rbinom(Ni, 1, H1.lowerBound[j])
      
      for( k in seq( length(J)-1 ) ){
         observed.lowerBound[[j]][k,i] <- ( mean(trt.lowerBound[seq(J[k]/2)]) - mean(control.lowerBound[seq(J[k]/2)]) )
         
      }#k for end
      observed.lowerBound[[j]][4,i] <- ( mean(trt.lowerBound) - mean(control.lowerBound) )
      
   }#i for end
   
   for( k in seq( length(J) ) ){
      ## WHEN DO WE CROSS THE BOUNDARIES? ##
      finalTrtEffect.lowerBound[[j]] <- ifelse( is.na(finalTrtEffect.lowerBound[[j]]) & (observed.lowerBound[[j]][k,] < stop.eff.H1.2[k] | observed.lowerBound[[j]][k,] > stop.fut.H1.2[k]),
                                                observed.lowerBound[[j]][k,], finalTrtEffect.lowerBound[[j]] )
   }# k for end
   finalTrtEffect.lowerBound[[j]] <- ifelse( is.na(finalTrtEffect[[j]]), observed.lowerBound[[j]][4,], finalTrtEffect[[j]] )
   #H1.lowerDensity <- density(finalTrtEffect.lowerBound[[j]])
   if( mean.est <= (quantile(finalTrtEffect.lowerBound[[j]], 0.975, na.rm=T) + 0.005) & mean.est >= (quantile(finalTrtEffect.lowerBound[[j]], 0.975, na.rm=T) - 0.005)){
      CI.lower <- mean(finalTrtEffect.lowerBound[[j]], na.rm=T)
      break
   }#if end
}#j for end





theta.true <- mean(finalTrtEffect)

H1.CILower <- seq(theta.true, (theta.true-0.2), by=-0.02)
H1.CIUpper <- seq(theta.true, (theta.true+0.2), by=0.02)









boundary.eff <- update(OBF.j4.H1.less, alt.hypothesis=H1.CI[k])$boundary[,1]
boundary.fut <- update(OBF.j4.H1.less, alt.hypothesis=H1.CI[k])$boundary[,2]


for(i in nsims){
   control <- rbinom(Ni, 1, 0.3)
   trt <- rbinom(Ni, 1, H1)
   
   for( j in seq( length(J)-1 ) ){
      observed[j,i] <- ( mean(trt[seq(J[j]/2)]) - mean(control[seq(J[j]/2)]) )
      
   }
   observed[4,i] <- ( mean(trt) - mean(control) )
   
}

finalTrtEffect <- rep(NA, nsims)

for( i in seq( length(J) ) ){
   finalTrtEffect <- ifelse( is.na(finalTrtEffect) & (observed[i,] < stop.eff.H1.less[i] | observed[i,] > stop.fut.H1.less[i]),
                             observed[i,], finalTrtEffect )
}
finalTrtEffect <- ifelse( is.na(finalTrtEffect), observed[4,], finalTrtEffect )












#### stopping rules of mean single group, X ~ N(0,2), N=100, J=4 ##
## calculating Z stat
## H0: mu=0
## H1: mu!=0
## PART A ##
set.seed(123456)
Nj <- 100
nsims <- 100000
J <- c(25, 50, 75, 100)


interimStat <- matrix(NA, ncol=nsims, nrow=length(J))

for(i in seq(nsims)){
   
   data <- rnorm(Nj, 0,2)
   
   for(j in 1:length(J)){
      
      samp <- data[1:J[j]]
      
      samp.m <- mean(samp)
      samp.var <- var(samp)
      
      interimStat[j,i] <- ( samp.m/sqrt(samp.var/J[j]) )
      
   }
   
   
   
}

## PART B ##
sig <- matrix(NA, ncol=nsims, nrow=length(J))
propSig <- NULL
for( i in seq( length(J) ) ){
   
   sig[i,] <- ifelse(abs(interimStat[i,]) > qnorm(.975), 1, 0)
   
   propSig[i] <- sum(sig[i,])/nsims
   
}

propSig.stops <- rep(0,4)
for( j in seq(nsims) ){
   
   if( sum(sig[,j])==1 ){
      propSig.stops[1] <- propSig.stops[1] + 1
   } else if(sum(sig[,j])==2){
      propSig.stops[2] <- propSig.stops[2] + 1
      
   } else if(sum(sig[,j])==3){
      propSig.stops[3] <- propSig.stops[3] + 1
      
   } else if(sum(sig[,j])==4){
      propSig.stops[4] <- propSig.stops[4] + 1
      
   }
   
}
propSig.stops <- propSig.stops/nsims

resultsTable <- cbind(c(1:4), propSig)
resultsTable2 <- cbind(c("exactly 1", "exactly 2", "exactly 3", "exactly 4",
                         "any"), c(propSig.stops, sum(sig)/(4*nsims))
                       )

## PART C ##
finalStat <- rep(NA, nsims)

for( i in seq( length(J) ) ){
   finalStat <- ifelse( is.na(finalStat) & (abs(interimStat[i,]) > rep(qnorm(.975),4)),
                        interimStat[i,], finalStat )
}
finalStat <- ifelse( is.na(finalStat), interimStat[4,], finalStat )

plot( density( finalStat, bw=.2 ) )