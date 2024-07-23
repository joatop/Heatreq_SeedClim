########################################
# analysis of differences in coversums #
########################################

# modeling cover differences with a beta-likelihood with logit-link

#### libraries and functions ####
library(glmmTMB)
# logit-link function and inverse
logit <- function(p) log( p / (1-p) )
expit <- function(L) exp(L) / (1+exp(L))

#### data handling for analysis ####
diff.cumsum$diff.cumsum2 <- (diff.cumsum$diff.cumsum+1)/2
diff.cumsum[diff.cumsum$cover_cumsum==0,'diff.cumsum2'] <- 0.00001
diff.cumsum[diff.cumsum$cover_cumsum>0.99999,'diff.cumsum2'] <- 0.99999
diff.cumsum$Hr <- diff.cumsum$Heat_requirement-1
diff.cumsum$Hrf <- as.factor(diff.cumsum$Heat_requirement)
diff.cumsum$Hrfyr <- paste(diff.cumsum$Hrf,diff.cumsum$year,sep="_")

diff.cumsum.sp.alp$diff.cumsum2 <- (diff.cumsum.sp.alp$diff.cumsum+1)/2
diff.cumsum.sp.alp[diff.cumsum.sp.alp$cover_cumsum==0,'diff.cumsum2'] <- 0.00001
diff.cumsum.sp.alp[diff.cumsum.sp.alp$cover_cumsum>0.99999,'diff.cumsum2'] <- 0.99999
diff.cumsum.sp.alp$Hr <- diff.cumsum.sp.alp$Heat_requirement-1
diff.cumsum.sp.alp$Hrf <- as.factor(diff.cumsum.sp.alp$Heat_requirement)
diff.cumsum.sp.alp$Hrfyr <- paste(diff.cumsum.sp.alp$Hrf,diff.cumsum.sp.alp$year,sep="_")

diff.cumsum.sp.subalp$diff.cumsum2 <- (diff.cumsum.sp.subalp$diff.cumsum+1)/2
diff.cumsum.sp.subalp[diff.cumsum.sp.subalp$cover_cumsum==0,'diff.cumsum2'] <- 0.00001
diff.cumsum.sp.subalp[diff.cumsum.sp.subalp$cover_cumsum>0.99999,'diff.cumsum2'] <- 0.99999
diff.cumsum.sp.subalp$Hr <- diff.cumsum.sp.subalp$Heat_requirement-1
diff.cumsum.sp.subalp$Hrf <- as.factor(diff.cumsum.sp.subalp$Heat_requirement)
diff.cumsum.sp.subalp$Hrfyr <- paste(diff.cumsum.sp.subalp$Hrf,diff.cumsum.sp.subalp$year,sep="_")


#### regressions ####
mod.diff_TT2_alpine <- glmmTMB(diff.cumsum2~0+Hrf*as.factor(year)+(1|siteID), family='beta_family',
                                    data = diff.cumsum[diff.cumsum$TTtreat=='TT2' & 
                                                         diff.cumsum$alt.orig=='alpine',])

mod.diff_TTC_alpine <- glmmTMB(diff.cumsum2~0+Hrf*as.factor(year)+(1|siteID), family='beta_family',
                                    data = diff.cumsum[diff.cumsum$TTtreat=='TTC' & 
                                                         diff.cumsum$alt.orig=='alpine',])

mod.diff_TT1_alpine <- glmmTMB(diff.cumsum2~0+Hrf*as.factor(year)+(1|siteID), family='beta_family',
                               data = diff.cumsum[diff.cumsum$TTtreat=='TT1' & 
                                                    diff.cumsum$alt.orig=='alpine',])

mod.diff_TT2_alpine.sp.alp <- glmmTMB(diff.cumsum2~0+Hrf*as.factor(year)+(1|siteID), family='beta_family',
                                    data = diff.cumsum.sp.alp[diff.cumsum.sp.alp$TTtreat=='TT2' & 
                                                                diff.cumsum.sp.alp$alt.orig=='alpine',])

mod.diff_TT2_subalpine <- glmmTMB(diff.cumsum2~0+Hrf*as.factor(year)+(1|siteID), family='beta_family',
                                    data = diff.cumsum[diff.cumsum$TTtreat=='TT2' & 
                                                         diff.cumsum$alt.orig=='sub-alpine',])

mod.diff_TTC_subalpine <- glmmTMB(diff.cumsum2~0+Hrf*as.factor(year)+(1|siteID), family='beta_family',
                                    data = diff.cumsum[diff.cumsum$TTtreat=='TTC' & 
                                                         diff.cumsum$alt.orig=='sub-alpine',])

mod.diff_TT1_subalpine <- glmmTMB(diff.cumsum2~0+Hrf*as.factor(year)+(1|siteID), family='beta_family',
                                  data = diff.cumsum[diff.cumsum$TTtreat=='TT1' & 
                                                       diff.cumsum$alt.orig=='sub-alpine',])

mod.diff_TT2_subalpine.sp.subalp <- glmmTMB(diff.cumsum2~0+Hrf*as.factor(year)+(1|siteID), family='beta_family',
                                           data = diff.cumsum.sp.subalp[diff.cumsum.sp.subalp$TTtreat=='TT2' & 
                                                                          diff.cumsum.sp.subalp$alt.orig=='sub-alpine',])


#### predictions ####
## alpine
pred.diff_TT2_alpine <- expand.grid(Hrf=as.factor(c(1:10)),year=as.factor(c(2011,2012,2013,2015,2017,2019)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~0+Hrf*as.factor(year),data = pred.diff_TT2_alpine)
pred.diff_TT2_alpine$fit <- X %*% fixef(mod.diff_TT2_alpine)[['cond']]
pred.diff_TT2_alpine$SE <- sqrt(diag(X %*% vcov(mod.diff_TT2_alpine)[['cond']] %*% t(X)))
# SE for intercept is 0 here
pred.diff_TT2_alpine$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TT2_alpine$lo <- expit( pred.diff_TT2_alpine$fit - (1.96 * pred.diff_TT2_alpine$SE) )*2-1
pred.diff_TT2_alpine$up <- expit( pred.diff_TT2_alpine$fit + (1.96 * pred.diff_TT2_alpine$SE) )*2-1
# prediction
pred.diff_TT2_alpine$fit.resp <- expit(pred.diff_TT2_alpine$fit)*2-1
pred.diff_TT2_alpine

pred.diff_TTC_alpine <- expand.grid(Hrf=as.factor(c(1:10)),year=as.factor(c(2010,2011,2012,2013,2015,2016,2017,2019)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~0+Hrf*as.factor(year),data = pred.diff_TTC_alpine)
pred.diff_TTC_alpine$fit <- X %*% fixef(mod.diff_TTC_alpine)[['cond']]
pred.diff_TTC_alpine$SE <- sqrt(diag(X %*% vcov(mod.diff_TTC_alpine)[['cond']] %*% t(X)))
# SE for intercept is 0 here
pred.diff_TTC_alpine$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TTC_alpine$lo <- expit( pred.diff_TTC_alpine$fit - (1.96 * pred.diff_TTC_alpine$SE) )*2-1
pred.diff_TTC_alpine$up <- expit( pred.diff_TTC_alpine$fit + (1.96 * pred.diff_TTC_alpine$SE) )*2-1
# prediction
pred.diff_TTC_alpine$fit.resp <- expit(pred.diff_TTC_alpine$fit)*2-1
pred.diff_TTC_alpine

pred.diff_TT2_alpine.sp.alp <- expand.grid(Hrf=as.factor(c(1:10)),year=as.factor(c(2011,2012,2013,2015,2017,2019)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~0+Hrf*as.factor(year),data = pred.diff_TT2_alpine.sp.alp)
pred.diff_TT2_alpine.sp.alp$fit <- X %*% fixef(mod.diff_TT2_alpine.sp.alp)[['cond']]
pred.diff_TT2_alpine.sp.alp$SE <- sqrt(diag(X %*% vcov(mod.diff_TT2_alpine.sp.alp)[['cond']] %*% t(X)))
# SE for intercept is 0 here
pred.diff_TT2_alpine.sp.alp$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TT2_alpine.sp.alp$lo <- expit( pred.diff_TT2_alpine.sp.alp$fit - (1.96 * pred.diff_TT2_alpine.sp.alp$SE) )*2-1
pred.diff_TT2_alpine.sp.alp$up <- expit( pred.diff_TT2_alpine.sp.alp$fit + (1.96 * pred.diff_TT2_alpine.sp.alp$SE) )*2-1
# prediction
pred.diff_TT2_alpine.sp.alp$fit.resp <- expit(pred.diff_TT2_alpine.sp.alp$fit)*2-1
pred.diff_TT2_alpine.sp.alp

## sub-alpine
pred.diff_TT2_subalpine <- expand.grid(Hrf=as.factor(c(1:10)),year=as.factor(c(2011,2012,2013,2015,2017,2019)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~0+Hrf*as.factor(year),data = pred.diff_TT2_subalpine)
pred.diff_TT2_subalpine$fit <- X %*% fixef(mod.diff_TT2_subalpine)[['cond']]
pred.diff_TT2_subalpine$SE <- sqrt(diag(X %*% vcov(mod.diff_TT2_subalpine)[['cond']] %*% t(X)))
# SE for intercept is 0 here
pred.diff_TT2_subalpine$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TT2_subalpine$lo <- expit( pred.diff_TT2_subalpine$fit - (1.96 * pred.diff_TT2_subalpine$SE) )*2-1
pred.diff_TT2_subalpine$up <- expit( pred.diff_TT2_subalpine$fit + (1.96 * pred.diff_TT2_subalpine$SE) )*2-1
# prediction
pred.diff_TT2_subalpine$fit.resp <- expit(pred.diff_TT2_subalpine$fit)*2-1
pred.diff_TT2_subalpine

pred.diff_TTC_subalpine <- expand.grid(Hrf=as.factor(c(1:10)),year=as.factor(c(2011,2012,2013,2015,2016,2017,2019)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~0+Hrf*as.factor(year),data = pred.diff_TTC_subalpine)
pred.diff_TTC_subalpine$fit <- X %*% fixef(mod.diff_TTC_subalpine)[['cond']]
pred.diff_TTC_subalpine$SE <- sqrt(diag(X %*% vcov(mod.diff_TTC_subalpine)[['cond']] %*% t(X)))
# SE for intercept is 0 here
pred.diff_TTC_subalpine$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TTC_subalpine$lo <- expit( pred.diff_TTC_subalpine$fit - (1.96 * pred.diff_TTC_subalpine$SE) )*2-1
pred.diff_TTC_subalpine$up <- expit( pred.diff_TTC_subalpine$fit + (1.96 * pred.diff_TTC_subalpine$SE) )*2-1
# prediction
pred.diff_TTC_subalpine$fit.resp <- expit(pred.diff_TTC_subalpine$fit)*2-1
pred.diff_TTC_subalpine

pred.diff_TT2_subalpine.sp.subalp <- expand.grid(Hrf=as.factor(c(1:10)),year=as.factor(c(2011,2012,2013,2015,2017,2019)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~0+Hrf*as.factor(year),data = pred.diff_TT2_subalpine.sp.subalp)
pred.diff_TT2_subalpine.sp.subalp$fit <- X %*% fixef(mod.diff_TT2_subalpine.sp.subalp)[['cond']]
pred.diff_TT2_subalpine.sp.subalp$SE <- sqrt(diag(X %*% vcov(mod.diff_TT2_subalpine.sp.subalp)[['cond']] %*% t(X)))
# SE for intercept is 0 here
pred.diff_TT2_subalpine.sp.subalp$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TT2_subalpine.sp.subalp$lo <- expit( pred.diff_TT2_subalpine.sp.subalp$fit - (1.96 * pred.diff_TT2_subalpine.sp.subalp$SE) )*2-1
pred.diff_TT2_subalpine.sp.subalp$up <- expit( pred.diff_TT2_subalpine.sp.subalp$fit + (1.96 * pred.diff_TT2_subalpine.sp.subalp$SE) )*2-1
# prediction
pred.diff_TT2_subalpine.sp.subalp$fit.resp <- expit(pred.diff_TT2_subalpine.sp.subalp$fit)*2-1
pred.diff_TT2_subalpine.sp.subalp




#### plot test ####

with(diff.cumsum[diff.cumsum$TTtreat=='TTC' & 
                    diff.cumsum$alt.orig=='alpine',],
     plot(Heat_requirement,diff.cumsum,col='#808080')
     )
with(pred.diff_TTC_alpine,
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#80808090",border="#80808090")
     )
with(pred.diff_TTC_alpine,
     points(1:10,fit.resp, col='white',type='l',lwd=2)
     )
abline(h=0,lty=2)





#### summaries ####
summary(mod.diff_TT2_alpine)
summary(mod.diff_TT2_alpine.sp.alp)
summary(mod.diff_TTC_alpine)
summary(mod.diff_TT1_alpine)

summary(mod.diff_TT2_subalpine)
summary(mod.diff_TT2_subalpine.sp.subalp)
summary(mod.diff_TTC_subalpine)
summary(mod.diff_TT1_subalpine)


