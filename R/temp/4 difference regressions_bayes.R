library(rjags)
#### analysis with constant sigma - including diagnostics ####

sink('input/diff_reg.txt')
cat("
    model{
    
    ## Likelihood
    for (i in 1:n) {
    # binomial errors
    diff[i] ~ dbin(p[i], N[i])
    
    # linear predictor 
    logit(p[i]) <- alpha.diff[HeatGr[i]] + alpha.Site[siteID[i]] + eps[i]       
    
    # overdispersion
    eps[i] ~ dnorm(0,tau) # Note that precision tau = 1/sig?
    } 
    
    ## priors
    for (i in 1:n.HeatGr) {
    alpha.diff[i] ~ dnorm(0,0.001)     
    }

    for (j in 1:n.Site) {
    alpha.Site[j] ~ dnorm(0,tau.alpha.Site)     
    }
    tau.alpha.Site ~ dunif(0,100)
    sigma.alpha.Site <- sqrt(1/tau.alpha.Site)
    
    tau ~ dunif(0,1000000)
    sigma <- sqrt(1/tau)
    
    
    
    # Assess modelfit using pearson residuals
    for (i in 1:n) {
    # Pearson residuals
    Presi[i] <- (diff[i]-N[i]*p[i]) / sqrt(N[i]*p[i]*(1-p[i])) 
    # generate replicate dataset & compute Pearson residuals
    diff.new[i] ~ dbin(p[i],N[i])
    Presi.new[i] <- (diff.new[i]-N[i]*p[i]) / sqrt(N[i]*p[i]*(1-p[i])) 
    # squared Pearson residuals
    sq[i] <- pow(Presi[i],2)
    sq.new[i] <- pow(Presi.new[i],2)   
    }
    
    fit <- sum(sq[])                                    # Sumofsquaredresidualsforactualdataset
    fit.new <- sum(sq.new[])                            #Sumofsquaredresidualsfornewdataset
    test <- step(fit.new-fit)                           #Testwhethernewdatasetmoreextreme
    bpvalue <- mean(test)                               # Bayesianp-value
    }

    
    ",fill=TRUE)
sink()



###########################################################

# Setting up the JAGS run:
dat <- diff.cumsum[diff.cumsum$alt.orig %in% list('alpine','sub-alpine') & diff.cumsum$TTtreat %in% list('TTC','TT2'),]
dat$HeatGr <- as.factor(paste(dat$TTtreat,dat$Heat_requirement,dat$alt.orig,sep='_'))
n.HeatGr <- length(levels(dat$HeatGr))
dat$siteID <- as.factor(dat$siteID)
n.Site <- length(levels(dat$siteID))

# Specify a function to generate inital values for the parameters
inits.fn <- function() list(alpha.diff = rnorm(n.HeatGr,mean(dat$diff.cumsum),2),
                            alpha.Site = rnorm(n.Site,0,2),
                            tau.alpha.Site = 1,
                            tau = 1   
)


# Set up a list that contains all the necessary data
Data <- list(diff = (dat$diff.cumsum+1)/2*1000,
             N=rep(1000,nrow(dat)),
             HeatGr = dat$HeatGr,
             siteID = dat$siteID,
             n=nrow(dat), 
             n.HeatGr = length(levels(dat$HeatGr)),
             n.Site = length(levels(dat$siteID))
)


#### running the model ####
# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel_diff <- jags.model(file= "input/diff_reg.txt", data=Data, inits = inits.fn, n.chains = 3, n.adapt= 20000)
# Specify parameters for which posterior samples are saved
para.names <- c('alpha.diff','sigma','sigma.alpha.Site',#'alpha.Site',
                "fit","fit.new","bpvalue","Presi") #
# Continue the MCMC runs with sampling
Samples_diff <- coda.samples(jagsModel_diff , variable.names = para.names, n.iter = 10000)

summary(Samples_diff)

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





