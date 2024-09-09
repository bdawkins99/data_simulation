# Function to create simulated case-control data, containing variables with a main effect, variables involved
# in interaction effects (no main effect), and variables that are just random noise. This is useful for
# comparing the performance of several feature selection methodologies in detecting relevant features.
#

# github library containing functions for generating statistical effects, such as main effects and interaction
# effects. 
library(privateEC)

# sim_mixed_fn(): Uses privateEC to generate simulated data with both interaction effects & main effects.
#                     
# 
# Parameters:
#
#   num.samples      - (numeric) number of samples
#   num.variables    - (numeric) number of variables/features
#   pct.imbalance    - (numeric) fraction cases==1 out of total number of samples. (1 - pct.imbalance) = fraction controls==0
#   pct.signals      - (numeric) fraction functional out of total number of variables (num.variables)
#   main.bias        - (numeric) controls effect size for main effects (should be positive - e.g., 0.8) - see Leek and Storey paper
#   interaction.bias - (numeric) controls effect size for interaction effects (should be positive). Closer to 0 ==> stronger effect.
#   pct.mixed        - (numeric) fraction of functional variables involved in interaction effects (between 0 and 1).
#                      (1 - pct.mixed) = fraction main effect variables
#   pct.train        - (numeric) size of training set as fraction of total samples (num.samples). Should be between 0 and 1.
#   pct.holdout      - (numeric) size of holdout set as fraction of total samples (num.samples). Should be between 0 and 1.
#   pct.validation   - (numeric) size of validation set as fraction of total samples (num.samples). Should be between 0 and 1.
#   label            - (character) name of column with case/control labels
#
#   Note: pct.train + pct.holdout + pct.validation = 1 (Must choose pct.train, pct.holdout, & pct.validation to satisfy constraint)
#   
#   Functional variables are named simvar1, simvar2, ..., simvarN1 and noise is just var1, var2, ..., varN2.
#
sim_mixed_fn <- function(seed             = 1234,
                         num.samples      = 100,      # number of samples
                         num.variables    = 100,    # number of variables
                         pct.imbalance    = 0.5,    # fraction
                         pct.signals      = 0.1,
                         main.bias        = 0.8,
                         interaction.bias = 0.4,
                         pct.mixed        = 0.5,
                         label            = "class"){
  
  rand.seeds <- withr::with_seed(seed, 
                                 sample(1:10000, size = 2, replace = FALSE))
  num.attr <- num.variables
  num.samp <- num.samples
  main.bias <- main.bias
  int.bias <- interaction.bias
  
  n.sig <- num.attr * pct.signals
  n.main <- round(n.sig * (1 - pct.mixed))
  n.interaction <- round(n.sig * pct.mixed)
  
  n.noise <- num.attr - n.sig
  
  # interactionErdos simulation
    sim.data <- withr::with_seed(rand.seeds[1],
                    createSimulation(num.samples   = num.samp, 
                                     num.variables = num.attr, 
                                     pct.imbalance = pct.imbalance,
                                     pct.signals   = pct.signals, 
                                     pct.train     = 0.5, 
                                     pct.holdout   = 0.5,  
                                     label         = label,
                                     bias          = int.bias, 
                                     sim.type      = "interactionErdos", 
                                     verbose       = FALSE)
                    )
    dat <- rbind(sim.data$train, sim.data$holdout)
    predictors.mat <- dat[, - which(colnames(dat) == label)]
  
  # main effect simulation
    sim.data2 <- withr::with_seed(rand.seeds[2], 
                                  createSimulation(num.samples   = num.samp, 
                                                   num.variables = num.attr, 
                                                   pct.imbalance = pct.imbalance,
                                                   pct.signals   = pct.signals, 
                                                   pct.train     = 0.5, 
                                                   pct.holdout   = 0.5, 
                                                   label         = label,
                                                   bias          = main.bias, 
                                                   sim.type      = "mainEffect", 
                                                   verbose       = FALSE)
                                  )
    dat2 <- rbind(sim.data2$train, sim.data2$holdout)
    predictors.mat2 <- dat2[, - which(colnames(dat2) == label)]
  
  dat[, label] <- as.factor(dat[, label]) 
  pheno.class <- dat[, label]
  attr.names <- colnames(predictors.mat)
  num.samp <- nrow(dat)
  
  idx.interactions <- grep(colnames(dat), pattern="simvar")
  my.interactions <- dat[,idx.interactions[1:n.interaction]]
  
  idx.mains <- grep(colnames(dat2), pattern="simvar")
  my.mains <- dat2[,idx.mains[1:n.main]]
  
  my.noise <- dat[,-c(idx.interactions, which(colnames(dat)==label))]
  
  dat.all <- cbind(my.mains, my.interactions, my.noise, class=dat[,label])
  colnames(dat.all)[1:n.main] <- paste("simvar",1:n.main, sep="")
  colnames(dat.all)[(n.main+1):(n.main+1 + n.interaction - 1)] <- paste("simvar",(n.main+1):(n.main+1 + n.interaction - 1),sep="")
  dim(dat.all)
  
  return(dat.all)
  
}


#my.dats <- sim_mixed_fn(num.samples=num.samp,
#                        num.variables=num.attr,
#                        pct.imbalance=pct.imbalance,
#                        pct.signals=pct.signals,
#                        main.bias=main.bias,
#                        interaction.bias=int.bias,
#                        pct.mixed=pct.mixed)
#dim(my.dats)
#colnames(my.dats)
