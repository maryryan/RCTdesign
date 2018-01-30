####################
######################## RCT DESIGN PRACTICE
######################## MARY RYAN
####################
library(RCTdesign)
#### GETTING STARTED TUTORIAL ####
## clinical trial design created with seqDesign() ##
#### finding stopping boundary sampling size ####
dsnN <- seqDesign(
   prob.model= "mean", #distribution 
   arms= 2, #2 trt arms
   ratio= c(1,1), #how the total sample size is distributed between the arms
   null.hypothesis= 0, #no differences in trt
   alt.hypothesis= -10, #trt lowers response by 10
   sd= c(30,30),
   test.type= "less",
   size= 0.025, #alpha
   power= 0.90
   )
   #can specify sample size with sample.size
   #don't specify one of these and specify sample.size to find that other thing

#### finding power for given sample size and H1 ####
dsnPwr <- seqDesign(
   sample.size= 300,
   alt.hypothesis= -10,
   sd=30,
   power= "calculate")

#### finding H1 for given sample size and power ####
dsnAlt <- seqDesign(
   sample.size= 300,
   test.type= "less",
   sd= 30,
   power= 0.9
   )

#### update design to have 1 or 3 interim analyses with OBF boundaries ####
dsnJ2 <- update(dsnN,
                nbr.analyses=2)
dsnJ4 <- update(dsnN,
                nbr.analyses=4)
   # note that max sample size increases to maintain power

#### Evaluating Designs ####
## get operating characteristics via seqEvaluate() ##
dsnJ4.eval <- seqEvaluate(dsnJ4)
   # get specific OC by doing dsnJ4.eval$OCname
#boundary table
dsnJ4.eval$bndTable

#power table including ASN and cumulative stopping probs
dsnJ4.eval$pwrTable

#inference table with CIs of values obtained on stopping boundaries
dsnJ4.eval$inferenceTable

#### Comparing Designs Using Plots ####
#compare stopping boundaries
plot(dsnJ4, dsnJ2)

#compare power curves
seqPlotPower(dsnJ4, dsnJ2)

#relative power curves
seqPlotPower(dsnJ4, dsnJ2, reference=T)

#ASN curves
seqPlotASN(dsnJ4, dsnJ2)

#stopping prob curves
seqPlotStopProb(dsnJ4, dsnJ2)

#### PROBABILITY MODELS TUTORIAL ####
#### change design scale ####
dsnFixed <- seqDesign( arms= 1,
                       alt.hypothesis= -10,
                       variance= 30^2,
                       power= 0.90)
changeSeqScale(dsnFixed, "Z")
seqBoundary(dsnFixed, "Z")
dsnFixed.Z <- seqDesign( arms= 1,
                         alt.hypothesis= -10,
                         sd= 30,
                         power= 0.90,
                         display.scale="Z")

#### unequally spaced analyses ####
dsn6uneq <- seqDesign( arms= 1,
                       alt.hypothesis= -10,
                       sd= 30,
                       power= 0.90,
                       nbr.analyses=6,
                       sample.size=c(1,2,4,6,8,10))

#### ANCOVA ####
dsn6 <- seqDesign( alt.hypothesis= -10,
                   sd= 30,
                   power= 0.90,
                   nbr.analyses=6)
dsn6ancova <- update( dsn6, sd= sqrt(27.386^2 * (1 - 0.4^2)) )

#### Obrien-Flemming Boundaries, max sample size 1700, H1=-0.07 ####
obf <- seqDesign("mean", nbr.analyses=4,
                 alt.hypothesis=-0.07,
                 sample.size=1700,
                 test.type="less",
                 power="calculate"
                 )
fixed <- seqDesign("mean",
                   alt.hypothesis=-0.07,
                   sample.size=1700,
                   test.type="less",
                   power="calculate"
                   )
## dSeq() to find the density of the grp seq design ##
#not sure how to plot the density...
dSeq(obf, 1:4, observed=c(-0.171,
                          -0.086,
                          -0.057,
                          -0.043), theta=0 )

#### BOUNDARY SCALES TUTORIAL ####
