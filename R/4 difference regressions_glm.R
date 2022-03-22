########################################
# analysis of differences in coversums #
########################################

#### libraries and functions ####
library(lme4)
# logit-link function and inverse
logit <- function(p) log( p / (1-p) )
expit <- function(L) exp(L) / (1+exp(L))

#### data handling for analysis ####
diff.cumsum$D <- round((diff.cumsum$diff.cumsum+1)/2*1000)
diff.cumsum$N <- 1000-diff.cumsum$D
diff.cumsum$Hr <- diff.cumsum$Heat_requirement-1
diff.cumsum$Hrf <- as.factor(diff.cumsum$Heat_requirement)

diff.cumsum.sp.alp$D <- round((diff.cumsum.sp.alp$diff.cumsum+1)/2*1000)
diff.cumsum.sp.alp$N <- 1000-diff.cumsum.sp.alp$D
diff.cumsum.sp.alp$Hr <- diff.cumsum.sp.alp$Heat_requirement-1
diff.cumsum.sp.alp$Hrf <- as.factor(diff.cumsum.sp.alp$Heat_requirement)

diff.cumsum.sp.subalp$D <- round((diff.cumsum.sp.subalp$diff.cumsum+1)/2*1000)
diff.cumsum.sp.subalp$N <- 1000-diff.cumsum.sp.subalp$D
diff.cumsum.sp.subalp$Hr <- diff.cumsum.sp.subalp$Heat_requirement-1
diff.cumsum.sp.subalp$Hrf <- as.factor(diff.cumsum.sp.subalp$Heat_requirement)


#### regressions ####
mod.diff_TT2_alpine <- with(diff.cumsum[diff.cumsum$TTtreat=='TT2' & 
                                          diff.cumsum$alt.orig=='alpine',],
                            glmer(cbind(D,N)~0+Hrf+(1|siteID), family='binomial')
)
mod.diff_TTC_alpine <- with(diff.cumsum[diff.cumsum$TTtreat=='TTC' & 
                                          diff.cumsum$alt.orig=='alpine',],
                            glmer(cbind(D,N)~0+Hrf+(1|siteID), family='binomial')
)
mod.diff_TT2_alpine.sp.alp <- with(diff.cumsum.sp.alp[diff.cumsum.sp.alp$TTtreat=='TT2' & 
                                          diff.cumsum.sp.alp$alt.orig=='alpine',],
                            glmer(cbind(D,N)~0+Hrf+(1|siteID), family='binomial')
)
mod.diff_TT2_subalpine <- with(diff.cumsum[diff.cumsum$TTtreat=='TT2' & 
                                          diff.cumsum$alt.orig=='sub-alpine',],
                            glmer(cbind(D,N)~0+Hrf+(1|siteID), family='binomial')
)
mod.diff_TTC_subalpine <- with(diff.cumsum[diff.cumsum$TTtreat=='TTC' & 
                                          diff.cumsum$alt.orig=='sub-alpine',],
                            glmer(cbind(D,N)~0+Hrf+(1|siteID), family='binomial')
)
mod.diff_TT2_subalpine.sp.subalp <- with(diff.cumsum.sp.subalp[diff.cumsum.sp.subalp$TTtreat=='TT2' & 
                                                        diff.cumsum.sp.subalp$alt.orig=='sub-alpine',],
                                   glmer(cbind(D,N)~0+Hrf+(1|siteID), family='binomial')
)

#### predictions ####
## alpine
pred.diff_TT2_alpine <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~ Hrf,data = pred.diff_TT2_alpine)
pred.diff_TT2_alpine$fit <- X %*% fixef(mod.diff_TT2_alpine)
pred.diff_TT2_alpine$SE <- sqrt(diag(X %*% vcov(mod.diff_TT2_alpine) %*% t(X)))
# SE for intercept is 0 here
pred.diff_TT2_alpine$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TT2_alpine$lo <- expit( pred.diff_TT2_alpine$fit - (1.96 * pred.diff_TT2_alpine$SE) )*2-1
pred.diff_TT2_alpine$up <- expit( pred.diff_TT2_alpine$fit + (1.96 * pred.diff_TT2_alpine$SE) )*2-1
# prediction
pred.diff_TT2_alpine$fit.resp <- expit(pred.diff_TT2_alpine$fit)*2-1
pred.diff_TT2_alpine

pred.diff_TTC_alpine <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~ Hrf,data = pred.diff_TTC_alpine)
pred.diff_TTC_alpine$fit <- X %*% fixef(mod.diff_TTC_alpine)
pred.diff_TTC_alpine$SE <- sqrt(diag(X %*% vcov(mod.diff_TTC_alpine) %*% t(X)))
# SE for intercept is 0 here
pred.diff_TTC_alpine$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TTC_alpine$lo <- expit( pred.diff_TTC_alpine$fit - (1.96 * pred.diff_TTC_alpine$SE) )*2-1
pred.diff_TTC_alpine$up <- expit( pred.diff_TTC_alpine$fit + (1.96 * pred.diff_TTC_alpine$SE) )*2-1
# prediction
pred.diff_TTC_alpine$fit.resp <- expit(pred.diff_TTC_alpine$fit)*2-1
pred.diff_TTC_alpine

pred.diff_TT2_alpine.sp.alp <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~ Hrf,data = pred.diff_TT2_alpine.sp.alp)
pred.diff_TT2_alpine.sp.alp$fit <- X %*% fixef(mod.diff_TT2_alpine.sp.alp)
pred.diff_TT2_alpine.sp.alp$SE <- sqrt(diag(X %*% vcov(mod.diff_TT2_alpine.sp.alp) %*% t(X)))
# SE for intercept is 0 here
pred.diff_TT2_alpine.sp.alp$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TT2_alpine.sp.alp$lo <- expit( pred.diff_TT2_alpine.sp.alp$fit - (1.96 * pred.diff_TT2_alpine.sp.alp$SE) )*2-1
pred.diff_TT2_alpine.sp.alp$up <- expit( pred.diff_TT2_alpine.sp.alp$fit + (1.96 * pred.diff_TT2_alpine.sp.alp$SE) )*2-1
# prediction
pred.diff_TT2_alpine.sp.alp$fit.resp <- expit(pred.diff_TT2_alpine.sp.alp$fit)*2-1
pred.diff_TT2_alpine.sp.alp

## sub-alpine
pred.diff_TT2_subalpine <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~ Hrf,data = pred.diff_TT2_subalpine)
pred.diff_TT2_subalpine$fit <- X %*% fixef(mod.diff_TT2_subalpine)
pred.diff_TT2_subalpine$SE <- sqrt(diag(X %*% vcov(mod.diff_TT2_subalpine) %*% t(X)))
# SE for intercept is 0 here
pred.diff_TT2_subalpine$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TT2_subalpine$lo <- expit( pred.diff_TT2_subalpine$fit - (1.96 * pred.diff_TT2_subalpine$SE) )*2-1
pred.diff_TT2_subalpine$up <- expit( pred.diff_TT2_subalpine$fit + (1.96 * pred.diff_TT2_subalpine$SE) )*2-1
# prediction
pred.diff_TT2_subalpine$fit.resp <- expit(pred.diff_TT2_subalpine$fit)*2-1
pred.diff_TT2_subalpine

pred.diff_TTC_subalpine <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~ Hrf,data = pred.diff_TTC_subalpine)
pred.diff_TTC_subalpine$fit <- X %*% fixef(mod.diff_TTC_subalpine)
pred.diff_TTC_subalpine$SE <- sqrt(diag(X %*% vcov(mod.diff_TTC_subalpine) %*% t(X)))
# SE for intercept is 0 here
pred.diff_TTC_subalpine$SE[1] <- 0
# 95%-confidence intervals
pred.diff_TTC_subalpine$lo <- expit( pred.diff_TTC_subalpine$fit - (1.96 * pred.diff_TTC_subalpine$SE) )*2-1
pred.diff_TTC_subalpine$up <- expit( pred.diff_TTC_subalpine$fit + (1.96 * pred.diff_TTC_subalpine$SE) )*2-1
# prediction
pred.diff_TTC_subalpine$fit.resp <- expit(pred.diff_TTC_subalpine$fit)*2-1
pred.diff_TTC_subalpine

pred.diff_TT2_subalpine.sp.subalp <- expand.grid(Hrf=as.factor(c(1:10)))  
# you need to define the fixed effects part of the mod.cumsumsel as it was specified above
X <- model.matrix(~ Hrf,data = pred.diff_TT2_subalpine.sp.subalp)
pred.diff_TT2_subalpine.sp.subalp$fit <- X %*% fixef(mod.diff_TT2_subalpine.sp.subalp)
pred.diff_TT2_subalpine.sp.subalp$SE <- sqrt(diag(X %*% vcov(mod.diff_TT2_subalpine.sp.subalp) %*% t(X)))
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

