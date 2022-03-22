#### cumsum regressions ####
library(lmerTest)
#### all species ####
# TTC regression for 2009
cumsum.reg.TTC.2009 <- data.frame(Heat_requirement=rep(1:10,3),
                                  alt=rep(c('alpine','sub-alpine','boreal'),each=10),
                                  mean.cs09=NA,
                                  se.cs09=NA,
                                  p.cs09=NA)

for (i in 2:10) {
  cumsum09.a <- abun.cumsums[abun.cumsums$TTtreat=='TTC' & abun.cumsums$year=='2009' & abun.cumsums$alt.orig=='alpine' & abun.cumsums$Heat_requirement==i,]
  cumsum09.sa <- abun.cumsums[abun.cumsums$TTtreat=='TTC' & abun.cumsums$year=='2009' & abun.cumsums$alt.orig=='sub-alpine' & abun.cumsums$Heat_requirement==i,]
  cumsum09.b <- abun.cumsums[abun.cumsums$TTtreat=='TTC' & abun.cumsums$year=='2009' & abun.cumsums$alt.orig=='boreal' & abun.cumsums$Heat_requirement==i,]
  
  if(max(abs(cumsum09.a$cover_cumsum))>0) {
    cumsum.reg.TTC.2009[i,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum09.a))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TTC.2009[i,3:5] <- c(0,0,NA)}
  
  if(max(abs(cumsum09.sa$cover_cumsum))>0) {
    cumsum.reg.TTC.2009[i+10,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum09.sa))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TTC.2009[i+10,3:5] <- c(0,0,NA)}
  
  if(max(abs(cumsum09.b$cover_cumsum))>0) {
    cumsum.reg.TTC.2009[i+20,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum09.b))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TTC.2009[i+10,3:5] <- c(0,0,NA)}
}

cumsum.reg.TTC.2009[c(1,11,21),c(3,4)] <- c(1,1,1,0,0,0)
cumsum.reg.TTC.2009[c(29,30),c(3,4)] <- 0

# TTC regression for 2019
cumsum.reg.TTC.2019 <- data.frame(Heat_requirement=rep(1:10,3),
                                  alt=rep(c('alpine','sub-alpine','boreal'),each=10),
                                  mean.cs19=NA,
                                  se.cs19=NA,
                                  p.cs19=NA)

for (i in 2:10) {
  cumsum19.a <- abun.cumsums[abun.cumsums$TTtreat=='TTC' & abun.cumsums$year=='2019' & abun.cumsums$alt.orig=='alpine' & abun.cumsums$Heat_requirement==i,]
  cumsum19.sa <- abun.cumsums[abun.cumsums$TTtreat=='TTC' & abun.cumsums$year=='2019' & abun.cumsums$alt.orig=='sub-alpine' & abun.cumsums$Heat_requirement==i,]
  cumsum19.b <- abun.cumsums[abun.cumsums$TTtreat=='TTC' & abun.cumsums$year=='2019' & abun.cumsums$alt.orig=='boreal' & abun.cumsums$Heat_requirement==i,]
  
  if(max(abs(cumsum19.a$cover_cumsum))>0) {
    cumsum.reg.TTC.2019[i,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum19.a))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TTC.2019[i,3:5] <- c(0,0,NA)}
  
  if(max(abs(cumsum19.sa$cover_cumsum))>0) {
    cumsum.reg.TTC.2019[i+10,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum19.sa))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TTC.2019[i+10,3:5] <- c(0,0,NA)}
  
  if(max(abs(cumsum19.b$cover_cumsum))>0) {
    cumsum.reg.TTC.2019[i+20,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum19.b))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TTC.2019[i+10,3:5] <- c(0,0,NA)}
}

cumsum.reg.TTC.2019[c(1,11,21),c(3,4)] <- c(1,1,1,0,0,0)
cumsum.reg.TTC.2019[c(30),c(3,4)] <- 0


# TT2 regression for 2009
cumsum.reg.TT2.2009 <- data.frame(Heat_requirement=rep(1:10,2),
                                  alt=rep(c('alpine','sub-alpine'),each=10),
                                  mean.cs09=NA,
                                  se.cs09=NA,
                                  p.cs09=NA)

for (i in 2:10) {
  cumsum09.a <- abun.cumsums[abun.cumsums$TTtreat=='TT2' & abun.cumsums$year=='2009' & abun.cumsums$alt.orig=='alpine' & abun.cumsums$Heat_requirement==i,]
  cumsum09.sa <- abun.cumsums[abun.cumsums$TTtreat=='TT2' & abun.cumsums$year=='2009' & abun.cumsums$alt.orig=='sub-alpine' & abun.cumsums$Heat_requirement==i,]
  
  if(max(abs(cumsum09.a$cover_cumsum))>0) {
    cumsum.reg.TT2.2009[i,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum09.a))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TT2.2009[i,3:5] <- c(0,0,NA)}
  
  if(max(abs(cumsum09.sa$cover_cumsum))>0) {
    cumsum.reg.TT2.2009[i+10,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum09.sa))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TT2.2009[i+10,3:5] <- c(0,0,NA)}
}

cumsum.reg.TT2.2009[c(1,11),c(3,4)] <- c(1,1,0,0)

# TT2 regression for 2019
cumsum.reg.TT2.2019 <- data.frame(Heat_requirement=rep(1:10,2),
                                  alt=rep(c('alpine','sub-alpine'),each=10),
                                  mean.cs19=NA,
                                  se.cs19=NA,
                                  p.cs19=NA)

for (i in 2:10) {
  cumsum19.a <- abun.cumsums[abun.cumsums$TTtreat=='TT2' & abun.cumsums$year=='2019' & abun.cumsums$alt.orig=='alpine' & abun.cumsums$Heat_requirement==i,]
  cumsum19.sa <- abun.cumsums[abun.cumsums$TTtreat=='TT2' & abun.cumsums$year=='2019' & abun.cumsums$alt.orig=='sub-alpine' & abun.cumsums$Heat_requirement==i,]
  
  if(max(abs(cumsum19.a$cover_cumsum))>0) {
    cumsum.reg.TT2.2019[i,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum19.a))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TT2.2019[i,3:5] <- c(0,0,NA)}
  
  if(max(abs(cumsum19.sa$cover_cumsum))>0) {
    cumsum.reg.TT2.2019[i+10,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum19.sa))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TT2.2019[i+10,3:5] <- c(0,0,NA)}
}

cumsum.reg.TT2.2019[c(1,11),c(3,4)] <- c(1,1,0,0)
#cumsum.reg.TT2.2019[c(29,30),c(3,4)] <- 0



#### down to alpine species only ####
#### cumsum regressions for TT2 in 2009 & 2019 ####
# TT2 regression for 2009
cumsum.reg.TT2.2009.sp.alp <- data.frame(Heat_requirement=rep(1:10,1),
                                         alt=rep(c('alpine'),each=10),
                                         mean.cs09=NA,
                                         se.cs09=NA,
                                         p.cs09=NA)

for (i in 2:10) {
  cumsum09.a <- abun.cumsums.sp.alp[abun.cumsums.sp.alp$TTtreat=='TT2' & abun.cumsums.sp.alp$year=='2009' & abun.cumsums.sp.alp$alt.orig=='alpine' & abun.cumsums.sp.alp$Heat_requirement==i,]
  
  if(max(abs(cumsum09.a$cover_cumsum))>0) {
    cumsum.reg.TT2.2009.sp.alp[i,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum09.a))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TT2.2009.sp.alp[i,3:5] <- c(0,0,NA)}
  
}

cumsum.reg.TT2.2009.sp.alp[c(1),c(3,4)] <- c(1,0)

# TT2 regression for 2019
cumsum.reg.TT2.2019.sp.alp <- data.frame(Heat_requirement=rep(1:10,1),
                                         alt=rep(c('alpine'),each=10),
                                         mean.cs19=NA,
                                         se.cs19=NA,
                                         p.cs19=NA)

for (i in 2:10) {
  cumsum19.a <- abun.cumsums.sp.alp[abun.cumsums.sp.alp$TTtreat=='TT2' & abun.cumsums.sp.alp$year=='2019' & abun.cumsums.sp.alp$alt.orig=='alpine' & abun.cumsums.sp.alp$Heat_requirement==i,]
  
  if(max(abs(cumsum19.a$cover_cumsum))>0) {
    cumsum.reg.TT2.2019.sp.alp[i,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum19.a))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TT2.2019.sp.alp[i,3:5] <- c(0,0,NA)}
  
}

cumsum.reg.TT2.2019.sp.alp[c(1),c(3,4)] <- c(1,0)
#cumsum.reg.TT2.2019[c(29,30),c(3,4)] <- 0



#### down to sub-alpine species only ####
#### cumsum regressions for TT2 in 2009 & 2019 ####
# TT2 regression for 2009
cumsum.reg.TT2.2009.sp.subalp <- data.frame(Heat_requirement=rep(1:10,1),
                                            alt=rep(c('sub-alpine'),each=10),
                                            mean.cs09=NA,
                                            se.cs09=NA,
                                            p.cs09=NA)

for (i in 2:10) {
  cumsum09.a <- abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$TTtreat=='TT2' & abun.cumsums.sp.subalp$year=='2009' & abun.cumsums.sp.subalp$alt.orig=='sub-alpine' & abun.cumsums.sp.subalp$Heat_requirement==i,]
  
  if(max(abs(cumsum09.a$cover_cumsum))>0) {
    cumsum.reg.TT2.2009.sp.subalp[i,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum09.a))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TT2.2009.sp.subalp[i,3:5] <- c(0,0,NA)}
  
}

cumsum.reg.TT2.2009.sp.subalp[c(1),c(3,4)] <- c(1,0)

# TT2 regression for 2019
cumsum.reg.TT2.2019.sp.subalp <- data.frame(Heat_requirement=rep(1:10,1),
                                            alt=rep(c('sub-alpine'),each=10),
                                            mean.cs19=NA,
                                            se.cs19=NA,
                                            p.cs19=NA)

for (i in 2:10) {
  cumsum19.a <- abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$TTtreat=='TT2' & abun.cumsums.sp.subalp$year=='2019' & abun.cumsums.sp.subalp$alt.orig=='sub-alpine' & abun.cumsums.sp.subalp$Heat_requirement==i,]
  
  if(max(abs(cumsum19.a$cover_cumsum))>0) {
    cumsum.reg.TT2.2019.sp.subalp[i,3:5] <- summary(lmer(cover_cumsum~1+(1|siteID), data=cumsum19.a))$coefficients[c(1,2,5)]
  } else {cumsum.reg.TT2.2019.sp.subalp[i,3:5] <- c(0,0,NA)}
  
}

cumsum.reg.TT2.2019.sp.subalp[c(1),c(3,4)] <- c(1,0)
#cumsum.reg.TT2.2019[c(29,30),c(3,4)] <- 0
