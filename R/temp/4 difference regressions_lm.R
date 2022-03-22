#### analysis of cumsum differences ####
library(lmerTest)

#### all species ####
## TT2 regression
diff.reg.TT2 <- data.frame(Heat_requirement=rep(1:10,2),
                           alt=rep(c('alpine','sub-alpine'),each=10),
                           mean.diff=NA,
                           se.diff=NA,
                           p.diff=NA)

for (i in 1:10) {
  dat.a <- diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='alpine' & diff.cumsum$Heat_requirement==i,]
  dat.sa <- diff.cumsum[diff.cumsum$TTtreat=='TT2' & diff.cumsum$alt.orig=='sub-alpine' & diff.cumsum$Heat_requirement==i,]
  if(max(abs(dat.a$diff))>0) {
    diff.reg.TT2[i,3:5] <- summary(lmer(diff.cumsum~1+(1|siteID), data=dat.a))$coefficients[c(1,2,5)]
  } else {diff.reg.TT2[i,3:5] <- c(0,0,NA)}
  
  if(max(abs(dat.sa$diff))>0) {
    diff.reg.TT2[i+10,3:5] <- summary(lmer(diff.cumsum~1+(1|siteID), data=dat.sa))$coefficients[c(1,2,5)]
  } else {diff.reg.TT2[i+10,3:5] <- c(0,0,NA)}
}



diff.reg.TT2

## TTC regression
diff.reg.TTC <- data.frame(Heat_requirement=rep(1:10,3),
                           alt=rep(c('alpine','sub-alpine','boreal'),each=10),
                           mean.diff=NA,
                           se.diff=NA,
                           p.diff=NA)

for (i in 1:10) {
  dat.a <- diff.cumsum[diff.cumsum$TTtreat=='TTC' & diff.cumsum$alt.orig=='alpine' & diff.cumsum$Heat_requirement==i,]
  dat.sa <- diff.cumsum[diff.cumsum$TTtreat=='TTC' & diff.cumsum$alt.orig=='sub-alpine' & diff.cumsum$Heat_requirement==i,]
  dat.b <- diff.cumsum[diff.cumsum$TTtreat=='TTC' & diff.cumsum$alt.orig=='boreal' & diff.cumsum$Heat_requirement==i,]
  
  if(max(abs(dat.a$diff))>0) {
    diff.reg.TTC[i,3:5] <- summary(lmer(diff.cumsum~1+(1|siteID), data=dat.a))$coefficients[c(1,2,5)]
  } else {diff.reg.TTC[i,3:5] <- c(0,0,NA)}
  
  if(max(abs(dat.sa$diff))>0) {
    diff.reg.TTC[i+10,3:5] <- summary(lmer(diff.cumsum~1+(1|siteID), data=dat.sa))$coefficients[c(1,2,5)]
  } else {diff.reg.TTC[i+10,3:5] <- c(0,0,NA)}
  
  if(max(abs(dat.b$diff))>0) {
    diff.reg.TTC[i+20,3:5] <- summary(lmer(diff.cumsum~1+(1|siteID), data=dat.b))$coefficients[c(1,2,5)]
  } else {diff.reg.TTC[i+10,3:5] <- c(0,0,NA)}
}



diff.reg.TTC


#### down to alpine species only ####
## TT2 regression
diff.reg.TT2.sp.alp <- data.frame(Heat_requirement=rep(1:10,1),
                                  alt=rep(c('alpine'),each=10),
                                  mean.diff=NA,
                                  se.diff=NA,
                                  p.diff=NA)

for (i in 1:10) {
  dat.a <- diff.cumsum.sp.alp[diff.cumsum.sp.alp$TTtreat=='TT2' & diff.cumsum.sp.alp$alt.orig=='alpine' & diff.cumsum.sp.alp$Heat_requirement==i,]
  if(max(abs(dat.a$diff))>0) {
    diff.reg.TT2.sp.alp[i,3:5] <- summary(lmer(diff.cumsum~1+(1|siteID), data=dat.a))$coefficients[c(1,2,5)]
  } else {diff.reg.TT2.sp.alp[i,3:5] <- c(0,0,NA)}
  
}



diff.reg.TT2.sp.alp


#### down to sub-alpine species only ####
## TT2 regression
diff.reg.TT2.sp.subalp <- data.frame(Heat_requirement=rep(1:10,1),
                                     alt=rep(c('sub-alpine'),each=10),
                                     mean.diff=NA,
                                     se.diff=NA,
                                     p.diff=NA)

for (i in 1:10) {
  dat.a <- diff.cumsum.sp.subalp[diff.cumsum.sp.subalp$TTtreat=='TT2' & diff.cumsum.sp.subalp$alt.orig=='sub-alpine' & diff.cumsum.sp.subalp$Heat_requirement==i,]
  if(max(abs(dat.a$diff))>0) {
    diff.reg.TT2.sp.subalp[i,3:5] <- summary(lmer(diff.cumsum~1+(1|siteID), data=dat.a))$coefficients[c(1,2,5)]
  } else {diff.reg.TT2.sp.subalp[i,3:5] <- c(0,0,NA)}
  
}



diff.reg.TT2.sp.subalp