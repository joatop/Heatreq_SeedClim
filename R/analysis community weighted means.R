#### community weighted mean calculations ####
Hr.cwm <- data.frame(cwm=NA,
                     siteID=NA,
                     turfID=NA,
                     year=NA,
                     TTtreat=NA,
                     alt.orig=NA)

plotIDlist <- unique(comind$turfYr)

#i=plotIDlist[1]

# cwm mean per plot & year
for (i in 1:length(plotIDlist) ) {
  
  cwm_calc <- comind[comind$turfYr==plotIDlist[i],c('species_name','siteID','turfID','year','TTtreat','cover','Heat_requirement','alt.orig')]
  cwm_calc <- cwm_calc[!is.na(cwm_calc$Heat_requirement),]

  # relative cover
  cwm_calc[,'cover_rel'] <- cwm_calc[,'cover']/sum(cwm_calc[,'cover'])
  # calculate community weighted mean of Heat requirement
  Hr.cwm[i,'cwm'] <- with(cwm_calc, sum(cover*Heat_requirement)/sum(cover) )
  Hr.cwm[i,'siteID'] <- unique(cwm_calc$siteID)[1]
  Hr.cwm[i,'turfID'] <- unique(cwm_calc$turfID)[1]
  Hr.cwm[i,'year'] <- unique(cwm_calc$year)[1]
  Hr.cwm[i,'TTtreat'] <- unique(cwm_calc$TTtreat)[1]
  Hr.cwm[i,'alt.orig'] <- unique(cwm_calc$alt.orig)[1]
  
}

head(Hr.cwm)



#### difference between years in cwm for each plot ####
diff.cwm <- data.frame(diff=NA,
                       siteID=NA,
                       turfID=NA,
                       TTtreat=NA,
                       alt.orig=NA)

plotIDlist <- unique(Hr.cwm[Hr.cwm$TTtreat!='TT1','turfID'])

for (i in 1:length(plotIDlist) ) {
  
  diff.cwm[i,'diff'] <- Hr.cwm[Hr.cwm$turfID==plotIDlist[i] & Hr.cwm$year=='2019','cwm'] - 
    Hr.cwm[Hr.cwm$turfID==plotIDlist[i] & Hr.cwm$year=='2009','cwm']
  diff.cwm[i,'siteID'] <- unique(Hr.cwm[Hr.cwm$turfID==plotIDlist[i],'siteID'])
  diff.cwm[i,'turfID'] <- unique(Hr.cwm[Hr.cwm$turfID==plotIDlist[i],'turfID'])
  diff.cwm[i,'TTtreat'] <- unique(Hr.cwm[Hr.cwm$turfID==plotIDlist[i],'TTtreat'])
  diff.cwm[i,'alt.orig'] <- unique(Hr.cwm[Hr.cwm$turfID==plotIDlist[i],'alt.orig'])
  
}

head(diff.cwm)

#### plotting cwm ####
library(ggplot2)
#library(hrbrthemes)
#library(viridis)

ggplot(data=Hr.cwm[Hr.cwm$alt.orig!='boreal' & Hr.cwm$TTtreat!='TT1',], 
       aes(x=cwm, group=as.factor(year), fill=as.factor(year))) +
  geom_density(adjust=1.5, alpha=.4) +
  facet_wrap(TTtreat~alt.orig)

#### plotting, differences ####
ggplot(diff.cwm[diff.cwm$alt.orig!='boreal',], aes(x=diff)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  facet_wrap(TTtreat~alt.orig)


#### regression analyses of cwm differences ####
summary(lmer(diff~1 + (1|siteID), data=diff.cwm[diff.cwm$TTtreat=='TT2' & diff.cwm$alt.orig=='alpine',]))
summary(lmer(diff~1 + (1|siteID), data=diff.cwm[diff.cwm$TTtreat=='TT2' & diff.cwm$alt.orig=='sub-alpine',]))

summary(lmer(diff~1 + (1|siteID), data=diff.cwm[diff.cwm$TTtreat=='TTC' & diff.cwm$alt.orig=='alpine',]))
summary(lmer(diff~1 + (1|siteID), data=diff.cwm[diff.cwm$TTtreat=='TTC' & diff.cwm$alt.orig=='sub-alpine',]))

summary(lmer(diff~0 + TTtreat + (1|siteID), data=diff.cwm[diff.cwm$alt.orig=='alpine',]))

