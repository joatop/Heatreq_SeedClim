library(rjags)

sink('input/diffreg_alp_warm.txt')
cat("
    model{
    
    #Likelihood
    for (i in 1:n) {
    # beta distribution
    diff.cumsum2[i] ~ dbeta(a1[i],a2[i])
    a1[i] <- mu[i]*phi
    a2[i] <- (1-mu[i])*phi
    
    # linear predictor 
    logit(mu[i]) <- group.mean[Hrf[i],fyear[i]] + alpha.Site[siteID[i]]
    }
    
    #priors
    phi ~ dgamma(.1,.1)
    
    for (j in 1:n.Hrf) {
      for (k in 1:n.fyear) {
      group.mean[j,k] ~ dnorm(0,0.001)
      }}

    for (l in 1:n.Site) {
    alpha.Site[l] ~ dnorm(0,tau.alpha.plot)     
    }
    
    tau.alpha.plot ~ dunif(0,100)
    sigma.alpha.plot <- sqrt(1/tau.alpha.plot)
    
    }

    
    ",fill=TRUE)
sink()



###########################################################

# Setting up the JAGS run:
dat1$treatTime <- as.factor(paste(dat1$Treatment,dat1$Year,sep='_'))
n.treatTime = length(levels(dat1$treatTime))
n.treat=length(levels(dat1$Treatment))
n.time=length(levels(dat1$fTime))
n.trans=length(levels(dat1$Felt))
n.plot=length(levels(dat1$fid))
# Specify a function to generate inital values for the parameters
inits.fn <- function() list(alpha = rnorm(n.treatTime,mean(dat1$Production),2),
                            beta = rnorm(n.treatTime,1,0.7),
                            beta2 = rnorm(1,1,0.7),
                            #                            alpha.time = rnorm(n.time,10,2),
                            alpha.plot = rnorm(n.plot,5,2),
                            #                            tau.alpha.time = 1,
                            tau.alpha.plot = 1,
                            tau = 1   
)


# Set up a list that contains all the necessary data
Data <- list( n=nrow(dat1), prod = dat1$Production, dist = dat1$m_trans, dist2 = dat1$m_trans^2,
              #              treat = dat1$Treatment, n.treat=length(levels(dat1$Treatment)),
              #              time = dat1$fTime, n.time=length(levels(dat1$fTime)),
              plot = dat1$fid, n.plot=length(levels(dat1$fid)),
              treatTime = dat1$treatTime, n.treatTime = length(levels(dat1$treatTime))
)


#### running the model ####
# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel_poll_S1_time <- jags.model(file= "input/poll3a.txt", data=Data, inits = inits.fn, n.chains = 3, n.adapt= 20000)
# Specify parameters for which posterior samples are saved
para.names <- c('alpha','beta','beta2','sigma','sigma.alpha.plot',
                "fit","fit.new","bpvalue","residual","predicted") #
# Continue the MCMC runs with sampling
Samples_poll_S1_time <- coda.samples(jagsModel_poll_S1_time , variable.names = para.names, n.iter = 6000)

summary(Samples_poll_S1_time)

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

