##########################
# Bayesian Data Analysis with STAN
# STAN installation and more: 
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#how-to-install-rstan
#
# Alberto Sanchez alberto.sanchez.rodelgo@gmail.com
##########################
# Basic set up
#
# These options respectively allow you to automatically save a bare version of a 
# compiled Stan program to the hard disk so that it does not need to be recompiled 
# and to execute multiple Markov chains in parallel.
#
library(rstan) # observe startup messages
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#
# 
# univariate gaussian process basic stan model from Stan reference manual page 161
gauss <- list(N = nrow(regSeason[((regSeason$Season==2012) & (regSeason$Wteam==1228 | regSeason$Lteam==1228)),]), # number of games
                    x = c(regSeason[((regSeason$Season==2012) & (regSeason$Wteam==1228)),]$Wscore,
                          regSeason[((regSeason$Season==2012) & (regSeason$Lteam==1228)),]$Lscore)# scores
                    ) # estimated std

# multivariate gaussian process basic stan model from Stan reference manual page 161
gaussMult <- list(D = 2, N = 2*nrow(regSeason[((regSeason$Wteam==1228 & regSeason$Lteam==1321) | (regSeason$Lteam==1228 & regSeason$Wteam==1321)),]), # number of games
              x = c(regSeason[(regSeason$Wteam==1228 & regSeason$Lteam==1321),]$Wscore,
                         regSeason[(regSeason$Wteam==1321 & regSeason$Lteam==1228),]$Lscore,
                        regSeason[(regSeason$Wteam==1321 & regSeason$Lteam==1228),]$Wscore,
                           regSeason[(regSeason$Wteam==1228 & regSeason$Lteam==1321),]$Lscore) 
              # scores
) # estimated std


fit <- stan(file = 'gaussianProcess.stan', 
            data = gaussMult, 
            iter = 1000, chains = 4)

print(fit) # print the results in a table
#plot(fit, pars=c("mu", "tau", "theta")) # plot confidence intervals for selected parameters
#pairs(fit, pars = c("mu", "tau", "lp__", "theta")) # plot histograms and correlations between parameters

# extracts samples from the stanfit for parameters of interest
la <- extract(fit, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

# return an array of three dimensions: iterations, chains, parameters a[iter,chains,param]
a <- extract(fit, permuted = FALSE) 

plot(a[,1,17:18])


##################
# Shiny Stan
##################
#
# install.packages("shinystan")
library(shinystan)
# launch shiny for a fit object
my_sso <- launch_shinystan(fit)
