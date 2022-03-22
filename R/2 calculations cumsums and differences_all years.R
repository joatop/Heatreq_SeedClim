#### all species ####
#### cumulative sums calculations ####
# calculate cumulative sum of relative cover of species from high to low heat requirement
abun.cumsums <- list()

plotIDlist <- unique(comind$turfYr)

#i=plotIDlist[1]

for (i in 1:length(plotIDlist) ) {
  
  cumsum_calc <- comind[comind$turfYr==plotIDlist[i],c('species_name','siteID','turfID','year','TTtreat','cover','Heat_requirement','alt.orig')]
  cumsum_calc <- cumsum_calc[!is.na(cumsum_calc$Heat_requirement),]
  abun.cumsums[[i]] <- data.frame(Heat_requirement=rev(1:13))
  
  # relative cover
  cumsum_calc[,'cover_rel'] <- cumsum_calc[,'cover']/sum(cumsum_calc[,'cover'])
  # make sure all Heat_req levels are present
  cumsum_calc <- cumsum_calc %>% add_row(Heat_requirement=1:13, cover_rel=rep(0,13))
  # calculate cover sums for each Heat_req levels
  abun.cumsums[[i]][,'cover_cumsum'] <- rev(with(cumsum_calc,tapply(cover_rel,Heat_requirement,sum)))
  # calcualte cumulative sums
  abun.cumsums[[i]][,'cover_cumsum'] <- cumsum(abun.cumsums[[i]][,'cover_cumsum'])
  abun.cumsums[[i]][,'siteID'] <- unique(cumsum_calc$siteID)[1]
  abun.cumsums[[i]][,'turfID'] <- unique(cumsum_calc$turfID)[1]
  abun.cumsums[[i]][,'year'] <- unique(cumsum_calc$year)[1]
  abun.cumsums[[i]][,'TTtreat'] <- unique(cumsum_calc$TTtreat)[1]
  abun.cumsums[[i]][,'alt.orig'] <- unique(cumsum_calc$alt.orig)[1]
  
}

abun.cumsums <- data.frame(Reduce(rbind, abun.cumsums))
head(abun.cumsums)


#### cumsum difference calculations ####
diff.cumsum <- list()
plotIDlist <- unique(abun.cumsums$turfID)

for (i in 1:length(plotIDlist) ) {
  
  diff.cumsum[[i]] <- data.frame(Heat_requirement=rev(1:13))
  for (j in sort(unique(abun.cumsums[,'year'])) ) {
    diff.cumsum[[i]][,paste(j)] <- abun.cumsums[abun.cumsums$turfID==plotIDlist[i] & abun.cumsums$year==j,'cover_cumsum'] - 
      abun.cumsums[abun.cumsums$turfID==plotIDlist[i] & abun.cumsums$year=='2009','cover_cumsum']
  }
  diff.cumsum[[i]][,'siteID'] <- unique(abun.cumsums[abun.cumsums$turfID==plotIDlist[i],'siteID'])
  diff.cumsum[[i]][,'turfID'] <- unique(abun.cumsums[abun.cumsums$turfID==plotIDlist[i],'turfID'])
  diff.cumsum[[i]][,'TTtreat'] <- unique(abun.cumsums[abun.cumsums$turfID==plotIDlist[i],'TTtreat'])
  diff.cumsum[[i]][,'alt.orig'] <- unique(abun.cumsums[abun.cumsums$turfID==plotIDlist[i],'alt.orig'])
  
}

diff.cumsum <- data.frame(Reduce(rbind, diff.cumsum))
head(diff.cumsum)  
colnames(diff.cumsum)[2:10] <- sort(unique(abun.cumsums[,'year']))
head(diff.cumsum) 
  

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
  abun.cumsums.sp.alp[[i]] <- data.frame(Heat_requirement=rev(1:13))
  
  # relative cover
  cumsum_calc[,'cover_rel'] <- cumsum_calc[,'cover']/sum(cumsum_calc[,'cover'])
  # make sure all Heat_req levels are present
  cumsum_calc <- cumsum_calc %>% add_row(Heat_requirement=1:13, cover_rel=rep(0,13))
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
  
  diff.cumsum.sp.alp[[i]] <- data.frame(Heat_requirement=rev(1:13))
  for (j in sort(unique(abun.cumsums.sp.alp[,'year'])) ) {
    diff.cumsum.sp.alp[[i]][,paste(j)] <- abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i] & abun.cumsums.sp.alp$year==j,'cover_cumsum'] - 
      abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i] & abun.cumsums.sp.alp$year=='2009','cover_cumsum']
  }
  
  diff.cumsum.sp.alp[[i]][,'siteID'] <- unique(abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i],'siteID'])
  diff.cumsum.sp.alp[[i]][,'turfID'] <- unique(abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i],'turfID'])
  diff.cumsum.sp.alp[[i]][,'TTtreat'] <- unique(abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i],'TTtreat'])
  diff.cumsum.sp.alp[[i]][,'alt.orig'] <- unique(abun.cumsums.sp.alp[abun.cumsums.sp.alp$turfID==plotIDlist[i],'alt.orig'])
  
}

diff.cumsum.sp.alp <- data.frame(Reduce(rbind, diff.cumsum.sp.alp))
head(diff.cumsum.sp.alp)  
colnames(diff.cumsum.sp.alp)[2:8] <- sort(unique(abun.cumsums.sp.alp[,'year']))
head(diff.cumsum.sp.alp) 


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
  abun.cumsums.sp.subalp[[i]] <- data.frame(Heat_requirement=rev(1:13))
  
  # relative cover
  cumsum_calc[,'cover_rel'] <- cumsum_calc[,'cover']/sum(cumsum_calc[,'cover'])
  # make sure all Heat_req levels are present
  cumsum_calc <- cumsum_calc %>% add_row(Heat_requirement=1:13, cover_rel=rep(0,13))
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
  
  diff.cumsum.sp.subalp[[i]] <- data.frame(Heat_requirement=rev(1:13))
  for (j in sort(unique(abun.cumsums.sp.subalp[,'year'])) ) {
    diff.cumsum.sp.subalp[[i]][,paste(j)] <- abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i] & abun.cumsums.sp.subalp$year==j,'cover_cumsum'] - 
      abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i] & abun.cumsums.sp.subalp$year=='2009','cover_cumsum']
  }
  diff.cumsum.sp.subalp[[i]][,'siteID'] <- unique(abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i],'siteID'])
  diff.cumsum.sp.subalp[[i]][,'turfID'] <- unique(abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i],'turfID'])
  diff.cumsum.sp.subalp[[i]][,'TTtreat'] <- unique(abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i],'TTtreat'])
  diff.cumsum.sp.subalp[[i]][,'alt.orig'] <- unique(abun.cumsums.sp.subalp[abun.cumsums.sp.subalp$turfID==plotIDlist[i],'alt.orig'])
  
}

diff.cumsum.sp.subalp <- data.frame(Reduce(rbind, diff.cumsum.sp.subalp))
head(diff.cumsum.sp.subalp)  
colnames(diff.cumsum.sp.subalp)[2:8] <- sort(unique(abun.cumsums.sp.subalp[,'year']))
head(diff.cumsum.sp.subalp) 

# make years-diff.cumsum long
diff.cumsum <- pivot_longer(diff.cumsum, cols=starts_with('20'), names_to='year', values_to='diff.cumsum')
diff.cumsum.sp.alp <- pivot_longer(diff.cumsum.sp.alp, cols=starts_with('20'), names_to='year', values_to='diff.cumsum')
diff.cumsum.sp.subalp <- pivot_longer(diff.cumsum.sp.subalp, cols=starts_with('20'), names_to='year', values_to='diff.cumsum')

#diff.cumsum$turfYr <- paste(diff.cumsum$turfID, diff.cumsum$year, sep='_')

#### calculating sums of differences ####
sumsofdiffs <- diff.cumsum %>% 
#  filter(alt.orig %in% list("alpine")) %>%
  group_by(alt.orig,TTtreat,turfID,year) %>% 
  dplyr::summarise(across(diff.cumsum, sum))

sumsofdiffs.sp.alp <- diff.cumsum.sp.alp %>% 
  #  filter(alt.orig %in% list("alpine")) %>%
  group_by(alt.orig,TTtreat,turfID,year) %>% 
  dplyr::summarise(across(diff.cumsum, sum))

sumsofdiffs.sp.subalp <- diff.cumsum.sp.subalp %>% 
  #  filter(alt.orig %in% list("alpine")) %>%
  group_by(alt.orig,TTtreat,turfID,year) %>% 
  dplyr::summarise(across(diff.cumsum, sum))



#### plotting ####
# alpine TT2
with(sumsofdiffs[sumsofdiffs$alt.orig %in% list("alpine") & sumsofdiffs$TTtreat %in% list("TT2"),],
     plot(year,diff.cumsum)
     )
abline(h=0,lty=2)
# alpine TT2, alpine species only
with(sumsofdiffs.sp.alp[sumsofdiffs.sp.alp$alt.orig %in% list("alpine") & sumsofdiffs.sp.alp$TTtreat %in% list("TT2"),],
     plot(year,diff.cumsum)
)
abline(h=0,lty=2)

# sub-alpine TT2
with(sumsofdiffs[sumsofdiffs$alt.orig %in% list("sub-alpine") & sumsofdiffs$TTtreat %in% list("TT2"),],
     plot(year,diff.cumsum)
)
abline(h=0,lty=2)
# sub-alpine TT2, sub-alpine and alpine species only
with(sumsofdiffs.sp.subalp[sumsofdiffs.sp.subalp$alt.orig %in% list("sub-alpine") & sumsofdiffs.sp.subalp$TTtreat %in% list("TT2"),],
     plot(year,diff.cumsum)
)
abline(h=0,lty=2)



library(ggplot2)
library(hrbrthemes)

# alpine TT2
sumsofdiffs %>%
  filter(alt.orig %in% list("alpine") & TTtreat %in% list("TT2")) %>%
  ggplot(aes(x=as.numeric(year), y=diff.cumsum)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) + 
  geom_abline(slope=0,intercept=0,linetype=2)



# alpine & sub-alpine, TT2 & TTC
sumsofdiffs %>%
  filter(alt.orig %in% list("alpine","sub-alpine") & TTtreat %in% list("TT2", "TTC")) %>%
  ggplot(aes(x=as.numeric(year), y=diff.cumsum)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_abline(slope=0,intercept=0,linetype=2) +
  facet_grid(alt.orig~TTtreat)


# alpine species only
sumsofdiffs.sp.alp %>%
  filter(alt.orig %in% list("alpine") & TTtreat %in% list("TT2")) %>%
  ggplot(aes(x=as.numeric(year), y=diff.cumsum)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) + 
  geom_abline(slope=0,intercept=0,linetype=2)

# sub-alpine & alpine species only
sumsofdiffs.sp.subalp %>%
  filter(alt.orig %in% list("sub-alpine") & TTtreat %in% list("TT2")) %>%
  ggplot(aes(x=as.numeric(year), y=diff.cumsum)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) + 
  geom_abline(slope=0,intercept=0,linetype=2)
