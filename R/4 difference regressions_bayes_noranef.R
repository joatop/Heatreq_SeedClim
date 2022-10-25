library(rjags)

sink('input/diffreg_alp_warm.txt')
cat("
    model{
    
    #Likelihood
    for (i in 1:n) {
    # beta distribution
    covdiff[i] ~ dbeta(a1[i],a2[i])
    a1[i] <- mu[i]*phi
    a2[i] <- (1-mu[i])*phi
    
    # linear predictor 
    logit(mu[i]) <- group.mean[Hrf[i],fyear[i]] # + alpha.Site[siteID[i]]
    }
    
    #priors
    phi ~ dgamma(.1,.1)
    
    for (j in 1:n.Hrf) {
      for (k in 1:n.fyear) {
      group.mean[j,k] ~ dnorm(0,0.001)
      }}

#    for (l in 1:n.Site) {
#    alpha.Site[l] ~ dnorm(0,tau.alpha.Site)     
#    }
#    
#    tau.alpha.Site ~ dunif(0,100)
#    sigma.alpha.Site <- sqrt(1/tau.alpha.Site)
    
    }

    
    ",fill=TRUE)
sink()



###########################################################

# Setting up the JAGS run:

# Specify a function to generate inital values for the parameters
inits.fn <- function() list(group.mean = matrix(rnorm(length(unique(diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='alpine','Hrfyr'])),
                                                      mean(diff.cumsum$diff.cumsum2),2),
                                                nrow=10,ncol=6),
#                            alpha.Site = rnorm(4,1,2),
#                            tau.alpha.Site = 1,
                            phi = 1
)


# Set up a list that contains all the necessary data
Data <- list( n = nrow(diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='alpine',]), 
              covdiff = diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='alpine','diff.cumsum2'], 
              Hrf = diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='alpine','Hrf'], 
              fyear = as.factor(diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='alpine','year']),
              siteID = as.factor(diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='alpine','siteID']), 
              n.Hrf = length(unique(diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='alpine','Hrf'])),
              n.fyear = length(unique(as.factor(diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='alpine','year']))),
              n.Site = length(unique(diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='alpine','siteID']))
)


#### running the model ####
# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel_diff.cumsum_alpine <- jags.model(file= "input/diffreg_alp_warm.txt", data=Data, inits = inits.fn, n.chains = 3, n.adapt= 20000)
# Specify parameters for which posterior samples are saved
para.names <- c('group.mean','phi') #,'sigma.alpha.Site'
# Continue the MCMC runs with sampling
Samples_diff.cumsum_alpine <- coda.samples(jagsModel_diff.cumsum_alpine , variable.names = para.names, n.iter = 100000)

summary(Samples_diff.cumsum_alpine)

gelman.diag(Samples_diff.cumsum_alpine)

plot(Samples_diff.cumsum_alpine)

#### model checks ####

### posterior predictive check
fit_Samples_poll_S1_time <- c(Samples_poll_S1_time[[1]][,"fit"],Samples_poll_S1_time[[2]][,"fit"],Samples_poll_S1_time[[3]][,"fit"])
fit.new_Samples_poll_S1_time <- c(Samples_poll_S1_time[[1]][,"fit.new"],Samples_poll_S1_time[[2]][,"fit.new"],Samples_poll_S1_time[[3]][,"fit.new"])

lim <-c(0,3200)
plot(fit_Samples_poll_S1_time,fit.new_Samples_poll_S1_time,main="Graphical posterior predictivecheck",las=1,xlab="SSQ for actualdataset",
     ylab="SSQ for ideal (new) datasets")    #,xlim=lim,ylim=lim
abline(0, 1)
mean(fit.new_Samples_poll_S1_time>fit_Samples_poll_S1_time)#Bayesianp-value


### predicted vs. residuals
predicted_Samples_poll_S1_time <- numeric()
for (i in 1:nrow(dat1)) {
  predicted_Samples_poll_S1_time[i] <- mean(c(Samples_poll_S1_time[[1]][,paste("predicted[",i,"]",sep="")],Samples_poll_S1_time[[2]][,paste("predicted[",i,"]",sep="")],Samples_poll_S1_time[[3]][,paste("predicted[",i,"]",sep="")]))
}

residual_Samples_poll_S1_time <- numeric()
for (i in 1:nrow(dat1)) {
  residual_Samples_poll_S1_time[i] <- mean(c(Samples_poll_S1_time[[1]][,paste("residual[",i,"]",sep="")],Samples_poll_S1_time[[2]][,paste("residual[",i,"]",sep="")],Samples_poll_S1_time[[3]][,paste("residual[",i,"]",sep="")]))
}

plot(predicted_Samples_poll_S1_time, residual_Samples_poll_S1_time, main = "Residuals vs. predicted values", 
     las = 1, xlab = "Predicted values", ylab = "Residuals")
abline(h = 0)

#### sampling for convergence check ####
para.names <- c('alpha','beta','beta2','sigma','sigma.alpha.plot') #
# Continue the MCMC runs with sampling
Samples_poll_S1_time <- coda.samples(jagsModel_poll_S1_time , variable.names = para.names, n.iter = 20000, thin=1)
# convergence check
gelman.diag(Samples_poll_S1_time)

summary(Samples_poll_S1_time)

plot(Samples_poll_S1_time)

Posterior <- as.matrix(Samples_poll_S1_time)
Posterior <- as.data.frame(Posterior)

