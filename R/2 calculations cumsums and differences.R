#### all species ####
#### cumulative sums calculations ####
# calculate cumulative sum of relative cover of species from high to low heat requirement
abun.cumsums <- list()

plotIDlist <- unique(comind$turfYr)

#i=plotIDlist[1]

for (i in 1:length(plotIDlist) ) {
  
  cumsum_calc <- comind[comind$turfYr==plotIDlist[i],c('species_name','siteID','turfID','year','turfYr','TTtreat','cover','Heat_requirement','alt.orig')]
  cumsum_calc <- cumsum_calc[!is.na(cumsum_calc$Heat_requirement),]
  abun.cumsums[[i]] <- data.frame(Heat_requirement=rev(1:max(comind$Heat_requirement)))
  
  # relative cover
  cumsum_calc[,'cover_rel'] <- cumsum_calc[,'cover']/sum(cumsum_calc[,'cover'])
  # make sure all Heat_req levels are present
  cumsum_calc <- cumsum_calc %>% add_row(Heat_requirement=1:max(comind$Heat_requirement), cover_rel=rep(0,max(comind$Heat_requirement)))
  # calculate cover sums for each Heat_req levels
  abun.cumsums[[i]][,'cover_cumsum'] <- rev(with(cumsum_calc,tapply(cover_rel,Heat_requirement,sum)))
  # calcualte cumulative sums
  abun.cumsums[[i]][,'cover_cumsum'] <- cumsum(abun.cumsums[[i]][,'cover_cumsum'])
  abun.cumsums[[i]][,'siteID'] <- unique(cumsum_calc$siteID)[1]
  abun.cumsums[[i]][,'turfID'] <- unique(cumsum_calc$turfID)[1]
  abun.cumsums[[i]][,'year'] <- unique(cumsum_calc$year)[1]
  abun.cumsums[[i]][,'turfYr'] <- unique(cumsum_calc$turfYr)[1]
  abun.cumsums[[i]][,'TTtreat'] <- unique(cumsum_calc$TTtreat)[1]
  abun.cumsums[[i]][,'alt.orig'] <- unique(cumsum_calc$alt.orig)[1]
  
}

abun.cumsums <- data.frame(Reduce(rbind, abun.cumsums))
head(abun.cumsums)
summary(abun.cumsums)


#### cumsum difference calculations ####
diff.cumsum <- list()
plotIDlist <- unique(abun.cumsums$turfID)
yeardifflist <- c(2019,2013,2011,2012,2017,2016,2015,2010)

for (i in 1:length(plotIDlist) ) {
  for (j in 1:length(yeardifflist) ) {
  
  diff.cumsum[[paste(i,j)]] <- data.frame(Heat_requirement=rev(1:max(comind$Heat_requirement)))
  diff.cumsum[[paste(i,j)]][,'diff.cumsum'] <- abun.cumsums[abun.cumsums$turfID==plotIDlist[i] & abun.cumsums$year==yeardifflist[j],'cover_cumsum'] - 
    abun.cumsums[abun.cumsums$turfID==plotIDlist[i] & abun.cumsums$year=='2009','cover_cumsum']
  diff.cumsum[[paste(i,j)]][,'siteID'] <- unique(abun.cumsums[abun.cumsums$turfID==plotIDlist[i],'siteID'])
  diff.cumsum[[paste(i,j)]][,'turfID'] <- unique(abun.cumsums[abun.cumsums$turfID==plotIDlist[i],'turfID'])
  diff.cumsum[[paste(i,j)]][,'year'] <- yeardifflist[j]
  diff.cumsum[[paste(i,j)]][,'TTtreat'] <- unique(abun.cumsums[abun.cumsums$turfID==plotIDlist[i],'TTtreat'])
  diff.cumsum[[paste(i,j)]][,'alt.orig'] <- unique(abun.cumsums[abun.cumsums$turfID==plotIDlist[i],'alt.orig'])
  
}}

diff.cumsum <- data.frame(Reduce(rbind, diff.cumsum))
head(diff.cumsum)  
summary(diff.cumsum)

diff.cumsum <- diff.cumsum %>%
  filter(!is.na(diff.cumsum))

summary(diff.cumsum)


#### down to alpine species only ####
#### cumulative sums calculations ####
# calculate cumulative sum of relative cover of species from high to low heat requirement
abun.cumsums.sp.alp <- list()

plotIDlist <- unique(comind[comind$TTtreat=='TT2' & comind$alt.orig=='alpine','turfYr'])

#i=plotIDlist[1]

for (i in 1:length(plotIDlist) ) {
  
  cumsum_calc <- comind[comind$turfYr==plotIDlist[i],c('species_name','siteID','turfID','year','TTtreat','cover','Heat_requirement','alt.orig','species.alp')]
  cumsum_calc <- cumsum_calc[!is.na(cumsum_calc$Heat_requirement),]
  cumsum_calc <- cumsum_calc[cumsum_calc$species.alp=='yes',]
  abun.cumsums.sp.alp[[i]] <- data.frame(Heat_requirement=rev(1:10))
  
  # relative cover
  cumsum_calc[,'cover_rel'] <- cumsum_calc[,'cover']/sum(cumsum_calc[,'cover'])
  # make sure all Heat_req levels are present
  cumsum_calc <- cumsum_calc %>% add_row(Heat_requirement=1:10, cover_rel=rep(0,10))
  # calculate cover sums for each Heat_req levels
  abun.cumsums.sp.alp[[i]][,'cover_cumsum'] <- rev(with(cumsum_calc,tapply(cover_rel,Heat_requirement,sum)))
  # calcualte cumulative sums
  abun.cumsums.sp.alp[[i]][,'cover_cumsum'] <- cumsum(abun.cumsums.sp.alp[[i]][,'cover_cumsum'])
  abun.cumsums.sp.alp[[i]][,'siteID'] <- unique(cumsum_calc$siteID)[1]
  abun.cumsums.sp.alp[[i]][,'turfID'] <- unique(cumsum_calc$turfID)[1]
  abun.cumsums.sp.alp[[i]][,'year'] <- unique(cumsum_calc$year)[1]
  abun.cumsums.sp.alp[[i]][,'TTtreat'] <- unique(cumsum_calc$TTtreat)[1]
  abun.cumsums.sp.alp[[i]][,'alt.orig'] <- unique(cumsum_calc$alt.orig)[1]
  
}

abun.cumsums.sp.alp <- data.frame(Reduce(rbind, abun.cumsums.sp.alp))
head(abun.cumsums.sp.alp)


#### cumsum difference calculations ####
diff.cumsum.sp.alp <- list()
plotIDlist <- unique(abun.cumsums.sp.alp$turfID)

for (i in 1:length(plotIDlist) ) {
  for (j in 1:length(yeardifflist) ) {
  
  diff.cumsum.sp.alp[[paste(i,j)]] <- data.frame(Heat_requirement=rev(1:10))
  diff.cumsum.sp.alp[[paste(i,j)]][,'diff.cumsum'] <- abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i] & abun.cumsums.sp.alp$year==yeardifflist[j],'cover_cumsum'] - 
    abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i] & abun.cumsums.sp.alp$year=='2009','cover_cumsum']
  diff.cumsum.sp.alp[[paste(i,j)]][,'siteID'] <- unique(abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i],'siteID'])
  diff.cumsum.sp.alp[[paste(i,j)]][,'turfID'] <- unique(abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i],'turfID'])
  diff.cumsum.sp.alp[[paste(i,j)]][,'year'] <- yeardifflist[j]
  diff.cumsum.sp.alp[[paste(i,j)]][,'TTtreat'] <- unique(abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i],'TTtreat'])
  diff.cumsum.sp.alp[[paste(i,j)]][,'alt.orig'] <- unique(abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i],'alt.orig'])
  
}}

diff.cumsum.sp.alp <- data.frame(Reduce(rbind, diff.cumsum.sp.alp))
head(diff.cumsum.sp.alp)  
summary(diff.cumsum.sp.alp)

diff.cumsum.sp.alp <- diff.cumsum.sp.alp %>%
  filter(!is.na(diff.cumsum))

summary(diff.cumsum.sp.alp)


#### down to sub-alpine species only ####
#### cumulative sums calculations ####
# calculate cumulative sum of relative cover of species from high to low heat requirement
abun.cumsums.sp.subalp <- list()

plotIDlist <- unique(comind[comind$TTtreat=='TT2' & comind$alt.orig=='sub-alpine','turfYr'])

#i=plotIDlist[1]

for (i in 1:length(plotIDlist) ) {
  
  cumsum_calc <- comind[comind$turfYr==plotIDlist[i],c('species_name','siteID','turfID','year','TTtreat','cover','Heat_requirement','alt.orig','species.subalp')]
  cumsum_calc <- cumsum_calc[!is.na(cumsum_calc$Heat_requirement),]
  cumsum_calc <- cumsum_calc[cumsum_calc$species.subalp=='yes',]
  abun.cumsums.sp.subalp[[i]] <- data.frame(Heat_requirement=rev(1:10))
  
  # relative cover
  cumsum_calc[,'cover_rel'] <- cumsum_calc[,'cover']/sum(cumsum_calc[,'cover'])
  # make sure all Heat_req levels are present
  cumsum_calc <- cumsum_calc %>% add_row(Heat_requirement=1:10, cover_rel=rep(0,10))
  # calculate cover sums for each Heat_req levels
  abun.cumsums.sp.subalp[[i]][,'cover_cumsum'] <- rev(with(cumsum_calc,tapply(cover_rel,Heat_requirement,sum)))
  # calcualte cumulative sums
  abun.cumsums.sp.subalp[[i]][,'cover_cumsum'] <- cumsum(abun.cumsums.sp.subalp[[i]][,'cover_cumsum'])
  abun.cumsums.sp.subalp[[i]][,'siteID'] <- unique(cumsum_calc$siteID)[1]
  abun.cumsums.sp.subalp[[i]][,'turfID'] <- unique(cumsum_calc$turfID)[1]
  abun.cumsums.sp.subalp[[i]][,'year'] <- unique(cumsum_calc$year)[1]
  abun.cumsums.sp.subalp[[i]][,'TTtreat'] <- unique(cumsum_calc$TTtreat)[1]
  abun.cumsums.sp.subalp[[i]][,'alt.orig'] <- unique(cumsum_calc$alt.orig)[1]
  
}

abun.cumsums.sp.subalp <- data.frame(Reduce(rbind, abun.cumsums.sp.subalp))
head(abun.cumsums.sp.subalp)


#### cumsum difference calculations ####
diff.cumsum.sp.subalp <- list()
plotIDlist <- unique(abun.cumsums.sp.subalp$turfID)

for (i in 1:length(plotIDlist) ) {
  for (j in 1:length(yeardifflist) ) {
  
  diff.cumsum.sp.subalp[[paste(i,j)]] <- data.frame(Heat_requirement=rev(1:10))
  diff.cumsum.sp.subalp[[paste(i,j)]][,'diff.cumsum'] <- abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i] & abun.cumsums.sp.subalp$year==yeardifflist[j],'cover_cumsum'] - 
    abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i] & abun.cumsums.sp.subalp$year=='2009','cover_cumsum']
  diff.cumsum.sp.subalp[[paste(i,j)]][,'siteID'] <- unique(abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i],'siteID'])
  diff.cumsum.sp.subalp[[paste(i,j)]][,'turfID'] <- unique(abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i],'turfID'])
  diff.cumsum.sp.subalp[[paste(i,j)]][,'year'] <- yeardifflist[j]
  diff.cumsum.sp.subalp[[paste(i,j)]][,'TTtreat'] <- unique(abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i],'TTtreat'])
  diff.cumsum.sp.subalp[[paste(i,j)]][,'alt.orig'] <- unique(abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i],'alt.orig'])
  
}}

diff.cumsum.sp.subalp <- data.frame(Reduce(rbind, diff.cumsum.sp.subalp))
head(diff.cumsum.sp.subalp)  
summary(diff.cumsum.sp.subalp)

diff.cumsum.sp.subalp <- diff.cumsum.sp.subalp %>%
  filter(!is.na(diff.cumsum))

summary(diff.cumsum.sp.subalp)