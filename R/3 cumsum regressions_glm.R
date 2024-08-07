#####################################
# analysis of cumulative cover sums #
#####################################

#### libraries & functions ####
library(lme4)
# functions for logit-link
logit <- function(p) log( p / (1-p) )
expit <- function(L) exp(L) / (1+exp(L))

#### data handling for analysis ####
abun.cumsums$D <- round(abun.cumsums$cover_cumsum*1000)  # successes
abun.cumsums$N <- 1000-abun.cumsums$D  # failures
abun.cumsums$Hr <- abun.cumsums$Heat_requirement-1  # for Heat_requirement with 1 as intercept
abun.cumsums$yrf <- as.factor(abun.cumsums$year)  # year as factor
abun.cumsums$Hrf <- as.factor(abun.cumsums$Heat_requirement) # Heat_requirement as factor

abun.cumsums.sp.alp$D <- round(abun.cumsums.sp.alp$cover_cumsum*1000)  # successes
abun.cumsums.sp.alp$N <- 1000-abun.cumsums.sp.alp$D  # failures
abun.cumsums.sp.alp$Hr <- abun.cumsums.sp.alp$Heat_requirement-1  # for Heat_requirement with 1 as intercept
abun.cumsums.sp.alp$yrf <- as.factor(abun.cumsums.sp.alp$year)  # year as factor
abun.cumsums.sp.alp$Hrf <- as.factor(abun.cumsums.sp.alp$Heat_requirement) # Heat_requirement as factor

abun.cumsums.sp.subalp$D <- round(abun.cumsums.sp.subalp$cover_cumsum*1000)  # successes
abun.cumsums.sp.subalp$N <- 1000-abun.cumsums.sp.subalp$D  # failures
abun.cumsums.sp.subalp$Hr <- abun.cumsums.sp.subalp$Heat_requirement-1  # for Heat_requirement with 1 as intercept
abun.cumsums.sp.subalp$yrf <- as.factor(abun.cumsums.sp.subalp$year)  # year as factor
abun.cumsums.sp.subalp$Hrf <- as.factor(abun.cumsums.sp.subalp$Heat_requirement) # Heat_requirement as factor

#### regressions ####
mod.cumsums_TT2_alpine_2009 <- with(abun.cumsums[abun.cumsums$TTtreat=='TT2' & 
                                           abun.cumsums$year=='2009' & 
                                           abun.cumsums$alt.orig=='alpine',],
                            glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TT2_alpine_2019 <- with(abun.cumsums[abun.cumsums$TTtreat=='TT2' & 
                                           abun.cumsums$year=='2019' & 
                                           abun.cumsums$alt.orig=='alpine',],
                            glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TTC_alpine_2009 <- with(abun.cumsums[abun.cumsums$TTtreat=='TTC' & 
                                           abun.cumsums$year=='2009' & 
                                           abun.cumsums$alt.orig=='alpine',],
                            glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TTC_alpine_2019 <- with(abun.cumsums[abun.cumsums$TTtreat=='TTC' & 
                                           abun.cumsums$year=='2019' & 
                                           abun.cumsums$alt.orig=='alpine',],
                            glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TT2_alpine_2009.sp.alp <- with(abun.cumsums.sp.alp[abun.cumsums.sp.alp$TTtreat=='TT2' & 
                                           abun.cumsums.sp.alp$year=='2009' & 
                                           abun.cumsums.sp.alp$alt.orig=='alpine',],
                            glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TT2_alpine_2019.sp.alp <- with(abun.cumsums.sp.alp[abun.cumsums.sp.alp$TTtreat=='TT2' & 
                                           abun.cumsums.sp.alp$year=='2019' & 
                                           abun.cumsums.sp.alp$alt.orig=='alpine',],
                            glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TT2_subalpine_2009 <- with(abun.cumsums[abun.cumsums$TTtreat=='TT2' & 
                                              abun.cumsums$year=='2009' & 
                                              abun.cumsums$alt.orig=='sub-alpine',],
                               glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TT2_subalpine_2019 <- with(abun.cumsums[abun.cumsums$TTtreat=='TT2' & 
                                              abun.cumsums$year=='2019' & 
                                              abun.cumsums$alt.orig=='sub-alpine',],
                               glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TTC_subalpine_2009 <- with(abun.cumsums[abun.cumsums$TTtreat=='TTC' & 
                                              abun.cumsums$year=='2009' & 
                                              abun.cumsums$alt.orig=='sub-alpine',],
                               glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TTC_subalpine_2019 <- with(abun.cumsums[abun.cumsums$TTtreat=='TTC' & 
                                              abun.cumsums$year=='2019' & 
                                              abun.cumsums$alt.orig=='sub-alpine',],
                               glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TT2_subalpine_2009.sp.subalp <- with(abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$TTtreat=='TT2' & 
                                                                  abun.cumsums.sp.subalp$year=='2009' & 
                                                                  abun.cumsums.sp.subalp$alt.orig=='sub-alpine',],
                                         glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)
mod.cumsums_TT2_subalpine_2019.sp.subalp <- with(abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$TTtreat=='TT2' & 
                                                                  abun.cumsums.sp.subalp$year=='2019' & 
                                                                  abun.cumsums.sp.subalp$alt.orig=='sub-alpine',],
                                         glmer(cbind(D,N)~Hrf+(1|siteID), family='binomial')
)



#### predictions ####
## alpine
pred.cumsums_TT2_alpine_2009 <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TT2_alpine_2009)
pred.cumsums_TT2_alpine_2009$fit <- X %*% fixef(mod.cumsums_TT2_alpine_2009)
pred.cumsums_TT2_alpine_2009$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TT2_alpine_2009) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TT2_alpine_2009$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TT2_alpine_2009$lo <- expit( pred.cumsums_TT2_alpine_2009$fit - (1.96 * pred.cumsums_TT2_alpine_2009$SE) )
pred.cumsums_TT2_alpine_2009$up <- expit( pred.cumsums_TT2_alpine_2009$fit + (1.96 * pred.cumsums_TT2_alpine_2009$SE) )
# prediction
pred.cumsums_TT2_alpine_2009$fit.resp <- expit(pred.cumsums_TT2_alpine_2009$fit)
pred.cumsums_TT2_alpine_2009

pred.cumsums_TT2_alpine_2019 <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TT2_alpine_2019)
pred.cumsums_TT2_alpine_2019$fit <- X %*% fixef(mod.cumsums_TT2_alpine_2019)
pred.cumsums_TT2_alpine_2019$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TT2_alpine_2019) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TT2_alpine_2019$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TT2_alpine_2019$lo <- expit( pred.cumsums_TT2_alpine_2019$fit - (1.96 * pred.cumsums_TT2_alpine_2019$SE) )
pred.cumsums_TT2_alpine_2019$up <- expit( pred.cumsums_TT2_alpine_2019$fit + (1.96 * pred.cumsums_TT2_alpine_2019$SE) )
# prediction
pred.cumsums_TT2_alpine_2019$fit.resp <- expit(pred.cumsums_TT2_alpine_2019$fit)
pred.cumsums_TT2_alpine_2019

pred.cumsums_TTC_alpine_2009 <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TTC_alpine_2009)
pred.cumsums_TTC_alpine_2009$fit <- X %*% fixef(mod.cumsums_TTC_alpine_2009)
pred.cumsums_TTC_alpine_2009$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TTC_alpine_2009) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TTC_alpine_2009$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TTC_alpine_2009$lo <- expit( pred.cumsums_TTC_alpine_2009$fit - (1.96 * pred.cumsums_TTC_alpine_2009$SE) )
pred.cumsums_TTC_alpine_2009$up <- expit( pred.cumsums_TTC_alpine_2009$fit + (1.96 * pred.cumsums_TTC_alpine_2009$SE) )
# prediction
pred.cumsums_TTC_alpine_2009$fit.resp <- expit(pred.cumsums_TTC_alpine_2009$fit)
pred.cumsums_TTC_alpine_2009

pred.cumsums_TTC_alpine_2019 <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TTC_alpine_2019)
pred.cumsums_TTC_alpine_2019$fit <- X %*% fixef(mod.cumsums_TTC_alpine_2019)
pred.cumsums_TTC_alpine_2019$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TTC_alpine_2019) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TTC_alpine_2019$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TTC_alpine_2019$lo <- expit( pred.cumsums_TTC_alpine_2019$fit - (1.96 * pred.cumsums_TTC_alpine_2019$SE) )
pred.cumsums_TTC_alpine_2019$up <- expit( pred.cumsums_TTC_alpine_2019$fit + (1.96 * pred.cumsums_TTC_alpine_2019$SE) )
# prediction
pred.cumsums_TTC_alpine_2019$fit.resp <- expit(pred.cumsums_TTC_alpine_2019$fit)
pred.cumsums_TTC_alpine_2019

pred.cumsums_TT2_alpine_2009.sp.alp <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TT2_alpine_2009.sp.alp)
pred.cumsums_TT2_alpine_2009.sp.alp$fit <- X %*% fixef(mod.cumsums_TT2_alpine_2009.sp.alp)
pred.cumsums_TT2_alpine_2009.sp.alp$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TT2_alpine_2009.sp.alp) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TT2_alpine_2009.sp.alp$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TT2_alpine_2009.sp.alp$lo <- expit( pred.cumsums_TT2_alpine_2009.sp.alp$fit - (1.96 * pred.cumsums_TT2_alpine_2009.sp.alp$SE) )
pred.cumsums_TT2_alpine_2009.sp.alp$up <- expit( pred.cumsums_TT2_alpine_2009.sp.alp$fit + (1.96 * pred.cumsums_TT2_alpine_2009.sp.alp$SE) )
# prediction
pred.cumsums_TT2_alpine_2009.sp.alp$fit.resp <- expit(pred.cumsums_TT2_alpine_2009.sp.alp$fit)
pred.cumsums_TT2_alpine_2009.sp.alp

pred.cumsums_TT2_alpine_2019.sp.alp <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TT2_alpine_2019.sp.alp)
pred.cumsums_TT2_alpine_2019.sp.alp$fit <- X %*% fixef(mod.cumsums_TT2_alpine_2019.sp.alp)
pred.cumsums_TT2_alpine_2019.sp.alp$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TT2_alpine_2019.sp.alp) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TT2_alpine_2019.sp.alp$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TT2_alpine_2019.sp.alp$lo <- expit( pred.cumsums_TT2_alpine_2019.sp.alp$fit - (1.96 * pred.cumsums_TT2_alpine_2019.sp.alp$SE) )
pred.cumsums_TT2_alpine_2019.sp.alp$up <- expit( pred.cumsums_TT2_alpine_2019.sp.alp$fit + (1.96 * pred.cumsums_TT2_alpine_2019.sp.alp$SE) )
# prediction
pred.cumsums_TT2_alpine_2019.sp.alp$fit.resp <- expit(pred.cumsums_TT2_alpine_2019.sp.alp$fit)
pred.cumsums_TT2_alpine_2019.sp.alp

## sub-alpine
pred.cumsums_TT2_subalpine_2009 <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TT2_subalpine_2009)
pred.cumsums_TT2_subalpine_2009$fit <- X %*% fixef(mod.cumsums_TT2_subalpine_2009)
pred.cumsums_TT2_subalpine_2009$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TT2_subalpine_2009) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TT2_subalpine_2009$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TT2_subalpine_2009$lo <- expit( pred.cumsums_TT2_subalpine_2009$fit - (1.96 * pred.cumsums_TT2_subalpine_2009$SE) )
pred.cumsums_TT2_subalpine_2009$up <- expit( pred.cumsums_TT2_subalpine_2009$fit + (1.96 * pred.cumsums_TT2_subalpine_2009$SE) )
# prediction
pred.cumsums_TT2_subalpine_2009$fit.resp <- expit(pred.cumsums_TT2_subalpine_2009$fit)
pred.cumsums_TT2_subalpine_2009

pred.cumsums_TT2_subalpine_2019 <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TT2_subalpine_2019)
pred.cumsums_TT2_subalpine_2019$fit <- X %*% fixef(mod.cumsums_TT2_subalpine_2019)
pred.cumsums_TT2_subalpine_2019$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TT2_subalpine_2019) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TT2_subalpine_2019$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TT2_subalpine_2019$lo <- expit( pred.cumsums_TT2_subalpine_2019$fit - (1.96 * pred.cumsums_TT2_subalpine_2019$SE) )
pred.cumsums_TT2_subalpine_2019$up <- expit( pred.cumsums_TT2_subalpine_2019$fit + (1.96 * pred.cumsums_TT2_subalpine_2019$SE) )
# prediction
pred.cumsums_TT2_subalpine_2019$fit.resp <- expit(pred.cumsums_TT2_subalpine_2019$fit)
pred.cumsums_TT2_subalpine_2019

pred.cumsums_TTC_subalpine_2009 <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TTC_subalpine_2009)
pred.cumsums_TTC_subalpine_2009$fit <- X %*% fixef(mod.cumsums_TTC_subalpine_2009)
pred.cumsums_TTC_subalpine_2009$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TTC_subalpine_2009) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TTC_subalpine_2009$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TTC_subalpine_2009$lo <- expit( pred.cumsums_TTC_subalpine_2009$fit - (1.96 * pred.cumsums_TTC_subalpine_2009$SE) )
pred.cumsums_TTC_subalpine_2009$up <- expit( pred.cumsums_TTC_subalpine_2009$fit + (1.96 * pred.cumsums_TTC_subalpine_2009$SE) )
# prediction
pred.cumsums_TTC_subalpine_2009$fit.resp <- expit(pred.cumsums_TTC_subalpine_2009$fit)
pred.cumsums_TTC_subalpine_2009

pred.cumsums_TTC_subalpine_2019 <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TTC_subalpine_2019)
pred.cumsums_TTC_subalpine_2019$fit <- X %*% fixef(mod.cumsums_TTC_subalpine_2019)
pred.cumsums_TTC_subalpine_2019$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TTC_subalpine_2019) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TTC_subalpine_2019$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TTC_subalpine_2019$lo <- expit( pred.cumsums_TTC_subalpine_2019$fit - (1.96 * pred.cumsums_TTC_subalpine_2019$SE) )
pred.cumsums_TTC_subalpine_2019$up <- expit( pred.cumsums_TTC_subalpine_2019$fit + (1.96 * pred.cumsums_TTC_subalpine_2019$SE) )
# prediction
pred.cumsums_TTC_subalpine_2019$fit.resp <- expit(pred.cumsums_TTC_subalpine_2019$fit)
pred.cumsums_TTC_subalpine_2019

pred.cumsums_TT2_subalpine_2009.sp.subalp <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TT2_subalpine_2009.sp.subalp)
pred.cumsums_TT2_subalpine_2009.sp.subalp$fit <- X %*% fixef(mod.cumsums_TT2_subalpine_2009.sp.subalp)
pred.cumsums_TT2_subalpine_2009.sp.subalp$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TT2_subalpine_2009.sp.subalp) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TT2_subalpine_2009.sp.subalp$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TT2_subalpine_2009.sp.subalp$lo <- expit( pred.cumsums_TT2_subalpine_2009.sp.subalp$fit - (1.96 * pred.cumsums_TT2_subalpine_2009.sp.subalp$SE) )
pred.cumsums_TT2_subalpine_2009.sp.subalp$up <- expit( pred.cumsums_TT2_subalpine_2009.sp.subalp$fit + (1.96 * pred.cumsums_TT2_subalpine_2009.sp.subalp$SE) )
# prediction
pred.cumsums_TT2_subalpine_2009.sp.subalp$fit.resp <- expit(pred.cumsums_TT2_subalpine_2009.sp.subalp$fit)
pred.cumsums_TT2_subalpine_2009.sp.subalp

pred.cumsums_TT2_subalpine_2019.sp.subalp <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the model as it was specified above
X <- model.matrix(~ Hrf,data = pred.cumsums_TT2_subalpine_2019.sp.subalp)
pred.cumsums_TT2_subalpine_2019.sp.subalp$fit <- X %*% fixef(mod.cumsums_TT2_subalpine_2019.sp.subalp)
pred.cumsums_TT2_subalpine_2019.sp.subalp$SE <- sqrt(diag(X %*% vcov(mod.cumsums_TT2_subalpine_2019.sp.subalp) %*% t(X)))
# SE for intercept is 0 here
pred.cumsums_TT2_subalpine_2019.sp.subalp$SE[1] <- 0
# 95%-confidence intervals
pred.cumsums_TT2_subalpine_2019.sp.subalp$lo <- expit( pred.cumsums_TT2_subalpine_2019.sp.subalp$fit - (1.96 * pred.cumsums_TT2_subalpine_2019.sp.subalp$SE) )
pred.cumsums_TT2_subalpine_2019.sp.subalp$up <- expit( pred.cumsums_TT2_subalpine_2019.sp.subalp$fit + (1.96 * pred.cumsums_TT2_subalpine_2019.sp.subalp$SE) )
# prediction
pred.cumsums_TT2_subalpine_2019.sp.subalp$fit.resp <- expit(pred.cumsums_TT2_subalpine_2019.sp.subalp$fit)
pred.cumsums_TT2_subalpine_2019.sp.subalp
# fixing up for Hrf 9 & 10
pred.cumsums_TT2_subalpine_2019.sp.subalp$up[9:10] <- 0
pred.cumsums_TT2_subalpine_2019.sp.subalp



#### plot test ####

with(abun.cumsums[abun.cumsums$TTtreat=='TT2' & 
                    abun.cumsums$year=='2009' & 
                    abun.cumsums$alt.orig=='alpine',],
     plot(Heat_requirement+0.06,cover_cumsum,col='#56B4E9'))
with(pred.cumsums_TT2_alpine_2009,
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#56B4E990",border="#56B4E990")
     
     )
with(pred.cumsums_TT2_alpine_2009,
     points(1:10,fit.resp, col='#56B4E9',type='l',lwd=2)
     )

with(abun.cumsums[abun.cumsums$TTtreat=='TT2' & 
                    abun.cumsums$year=='2019' & 
                    abun.cumsums$alt.orig=='alpine',],
     points(Heat_requirement-0.06,cover_cumsum,col='#E69F00'))
with(pred.cumsums_TT2_alpine_2019,
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#E69F0090",border="#E69F0090")
     
)
with(pred.cumsums_TT2_alpine_2019,
     points(1:10,fit.resp, col='#E69F00',type='l',lwd=2)
)









