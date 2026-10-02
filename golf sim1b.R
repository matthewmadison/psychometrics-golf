## Author: Matthew Madison
## Date: 9/26/2026
## Purpose: Golf + psychometrics simulation 

#load mirt library for IRT estimation 
library(mirt)


## Fixed parameters
#number of players (N) and holes (n_holes)
N <- 2000
n_holes <- 18
reps <- 500 #number of replications

#store esitmated rankings for each replication 
rk <- matrix(NA, nrow = reps, ncol = n_holes)

#start simulation loop
for(i in 1:reps){#loop over replications
  
  #set random seed for replicability 
  set.seed(i)
  
  # Generate random item parameters for a 2PL model
  # a = discrimination (slopes), d = intercepts (related to difficulty b via d = -a * b)
  a <- sort(matrix(rnorm(n_holes, 0.7, 0.2), ncol = 1))  # Discrimination parameters
  d <- matrix(rnorm(n_holes, mean = 0, sd = 1.0), ncol = 1)  # Intercept parameters
  
  # Combine into an item parameter matrix format required by mirt
  # itemtype specifies '2PL' for each item
  itemtype <- rep("2PL", n_holes)
  pars <- data.frame(a = a, d = d, g = 0, u = 1) # g=guessing (0), u=upper asymptote (1)
  
  # Generate random response data using simdata()
  # Theta (player ability) is drawn from standard normal N(0, 1) by default
  dat <- simdata(a = a, d = d, itemtype = itemtype, N = N)
  #ability can be scaled to N(14,5)
  
  #calibrate 2-PL model
  twopl <- mirt(dat, 1, "2PL")
  
  #extract parameter estimates 
  parmsest <- matrix(NA, n_holes, 2)
  
  #rank the hole discriminations (aka, hole handicaps) 
  x <- coef(twopl, simplify = T, irtpars = T)
  rk[i, ] <- rank(x$items[ , 1])
  
  print(paste("Running replication ", i, "."), sep="")

} #end replication loop

#summarize estimated hole handicaps
colMeans(rk)


