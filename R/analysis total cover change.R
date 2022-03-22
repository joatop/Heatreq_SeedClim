#### analysis of change in total cover ####

# total cover
coversum <- data.frame(siteID=NA,turfID=NA,year=NA,TTtreat=NA,alt.orig=NA,tot.cover=NA)

plotIDlist <- unique(comind$turfYr)

#i=plotIDlist[1]

for (i in 1:length(plotIDlist) ) {
  
  coversum_calc <- comind[comind$turfYr==plotIDlist[i],c('species_name','siteID','turfID','year','TTtreat','cover','Heat_requirement','alt.orig')]
#  coversum_calc <- cumsum_calc[!is.na(cumsum_calc$Heat_requirement),]

  # total cover
  coversum[i,'tot.cover'] <- sum(coversum_calc[,'cover'],na.rm=T)
  coversum[i,'siteID'] <- unique(coversum_calc$siteID)[1]
  coversum[i,'turfID'] <- unique(coversum_calc$turfID)[1]
  coversum[i,'year'] <- unique(coversum_calc$year)[1]
  coversum[i,'TTtreat'] <- unique(coversum_calc$TTtreat)[1]
  coversum[i,'alt.orig'] <- unique(coversum_calc$alt.orig)[1]
  
}

head(coversum)


ggplot(coversum[coversum$TTtreat!='TT1',], aes(x=TTtreat, y=tot.cover, fill=as.factor(year))) + 
  geom_boxplot() +
  facet_wrap(alt.orig~TTtreat, scale="free_x", dir='v') #


# cover difference
coverdiff <- data.frame(siteID=NA,turfID=NA,TTtreat=NA,alt.orig=NA,diff.cover=NA)

plotIDlist <- unique(comind[comind$TTtreat!='TT1','turfID'])

#i=plotIDlist[1]

for (i in 1:length(plotIDlist) ) {
  
  coverdiff_calc <- comind[comind$turfID==plotIDlist[i],c('species_name','siteID','turfID','year','TTtreat','cover','Heat_requirement','alt.orig')]
  #  coverdiff_calc <- cumsum_calc[!is.na(cumsum_calc$Heat_requirement),]
  
  # total cover
  coverdiff[i,'diff.cover'] <- sum(coverdiff_calc[coverdiff_calc$year==2019,'cover'],na.rm=T)-
    sum(coverdiff_calc[coverdiff_calc$year==2009,'cover'],na.rm=T)
  coverdiff[i,'siteID'] <- unique(coverdiff_calc$siteID)[1]
  coverdiff[i,'turfID'] <- unique(coverdiff_calc$turfID)[1]
  coverdiff[i,'TTtreat'] <- unique(coverdiff_calc$TTtreat)[1]
  coverdiff[i,'alt.orig'] <- unique(coverdiff_calc$alt.orig)[1]
  
}

head(coverdiff)

ggplot(coverdiff, aes(x=TTtreat, y=diff.cover, fill=as.factor(alt.orig))) + 
  geom_boxplot() +
  facet_wrap(~TTtreat, scale="free_x") #


coverdiff$Treatalt <- paste(coverdiff$TTtreat,coverdiff$alt.orig,sep='_')
summary(lmer(diff.cover~0+Treatalt+(1|siteID),data=coverdiff))
