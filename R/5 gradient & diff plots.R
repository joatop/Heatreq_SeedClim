#### plot cumsum temp gradient, panel ####
par(mfrow=c(1,3))
# alpine
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='alpine',],
     plot(Heat_requirement,mean.cs09, type='l',col='blue',ylab='Cover (cumul. rel cover [0-1])',main="Alpine")
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#56B4E9",border=F)
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='alpine',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)

# sub-alpine
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='sub-alpine',],
     plot(Heat_requirement,mean.cs09, type='l',col='orange', ylab="",main="Sub-alpine")
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#E69F00",border=F)
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='sub-alpine',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)

# boreal
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='boreal',],
     plot(Heat_requirement,mean.cs09, type='l',col='red', ylab="",main="Boreal",ylim=c(0,1))
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='boreal',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#D55E00",border=F)
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='boreal',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)

#### plot cumsum temp gradient, single ####
par(mfrow=c(1,1))
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='alpine',],
     plot(Heat_requirement,mean.cs09, type='l',col='white',xlab='Heat requirement',ylab='Cover (cumul. rel cover [0-1])',main="Cumulative distribution of cover along the temperature gradient")
)
# boreal
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='boreal',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#D55E0090",border=F)
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='boreal',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)

# sub-alpine
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#E69F0090",border=F)
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='sub-alpine',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)
# alpine
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#56B4E990",border=F)
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='alpine',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)
legend('topright',legend=c("alpine","sub-alpine","boreal"),fill=c("#56B4E9","#E69F00","#D55E00"))


#### difference plot ####

par(mfrow=c(2,3))
# warmer
# alpine
with(diff.reg.TT2[diff.reg.TT2$alt=='alpine',],
     plot(Heat_requirement,mean.diff, type='l',col='blue',ylim=c(-0.05,0.6),ylab='Cover difference (cumul. rel cover [0-1])',main="Alpine, warmer")
)
with(diff.reg.TT2[diff.reg.TT2$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="#56B4E9",border=F)
)
with(diff.reg.TT2[diff.reg.TT2$alt=='alpine',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)
# sub-alpine
with(diff.reg.TT2[diff.reg.TT2$alt=='sub-alpine',],
     plot(Heat_requirement,mean.diff, type='l',col='orange',ylim=c(-0.05,0.6), ylab="",main="Sub-alpine, warmer")
)
with(diff.reg.TT2[diff.reg.TT2$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="#E69F00",border=F)
)
with(diff.reg.TT2[diff.reg.TT2$alt=='sub-alpine',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)

plot(1,1,col='white',type='n',bty='n',xaxt='n',yaxt='n',xlab='',ylab='')

# extant
# alpine
with(diff.reg.TTC[diff.reg.TTC$alt=='alpine',],
     plot(Heat_requirement,mean.diff, type='l',col='blue',ylim=c(-0.05,0.6),ylab='Cover difference (cumul. rel cover [0-1])',main="Alpine, extant")
)
with(diff.reg.TTC[diff.reg.TTC$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="#56B4E9",border=F)
)
with(diff.reg.TTC[diff.reg.TTC$alt=='alpine',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)

# sub-alpine
with(diff.reg.TTC[diff.reg.TTC$alt=='sub-alpine',],
     plot(Heat_requirement,mean.diff, type='l',col='orange',ylim=c(-0.05,0.6), ylab="",main="Sub-alpine, extant")
)
with(diff.reg.TTC[diff.reg.TTC$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="#E69F00",border=F)
)
with(diff.reg.TTC[diff.reg.TTC$alt=='sub-alpine',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)

# boreal
with(diff.reg.TTC[diff.reg.TTC$alt=='boreal',],
     plot(Heat_requirement,mean.diff, type='l',col='red',ylim=c(-0.05,0.6), ylab="",main="Boreal, extant")
)
with(diff.reg.TTC[diff.reg.TTC$alt=='boreal',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="#D55E00",border=F)
)
with(diff.reg.TTC[diff.reg.TTC$alt=='boreal',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)





#### stuff ####

# illustration example TT2
par(mfrow=c(1,2))
dat <- abun.cumsums[abun.cumsums$TTtreat=='TT2',]
plot(rev(1:10),dat[dat$turfID==unique(dat$turfID)[40] & dat$year=='2009','cover_cumsum'],type='b',col='green')
points(rev(1:10),dat[dat$turfID==unique(dat$turfID)[40] & dat$year=='2019','cover_cumsum'],type='b',col='red')

dat.diff <- dat[dat$turfID==unique(dat$turfID)[40] & dat$year=='2019','cover_cumsum'] - dat[dat$turfID==unique(dat$turfID)[40] & dat$year=='2009','cover_cumsum']

plot(rev(1:10),dat.diff,type='b')
abline(h=0, lty=2)
