#### plots for cover and difference ####
#### alpine ####
par(mfrow=c(2,3))
## cover
# TT2 (all), 2009
with(cumsum.reg.TT2.2009[cumsum.reg.TT2.2009$alt=='alpine',],
     plot(Heat_requirement,mean.cs09, type='l',col='blue',xlab="",ylab='Cover (cumul. rel cover [0-1])',main="Warmer, all species")
)
with(cumsum.reg.TT2.2009[cumsum.reg.TT2.2009$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#56B4E990",border=F)
)
with(cumsum.reg.TT2.2009[cumsum.reg.TT2.2009$alt=='alpine',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)
# TT2 (all), 2019
#with(cumsum.reg.TT2.2019[cumsum.reg.TT2.2019$alt=='alpine',],
#     plot(Heat_requirement,mean.cs19, type='l',col='blue',ylab='Cover (cumul. rel cover [0-1])',main="Alpine")
#)
with(cumsum.reg.TT2.2019[cumsum.reg.TT2.2019$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs19-2*se.cs19,rev(mean.cs19+2*se.cs19)),
             col="#E69F0090",border=F)
)
with(cumsum.reg.TT2.2019[cumsum.reg.TT2.2019$alt=='alpine',],
     points(Heat_requirement,mean.cs19, type='l',col='white')
)

with(abun.cumsums[abun.cumsums$TTtreat=="TT2" & abun.cumsums$alt.orig=="alpine",],points(Heat_requirement+0.04, cover_cumsum, col="#56B4E930",pch=16))
with(abun.cumsums[abun.cumsums$TTtreat=="TT2" & abun.cumsums$alt.orig=="sub-alpine",],points(Heat_requirement-0.04, cover_cumsum, col="#E69F0030",pch=16))


# TT2 (sp.alp only), 2009
with(cumsum.reg.TT2.2009.sp.alp[cumsum.reg.TT2.2009.sp.alp$alt=='alpine',],
     plot(Heat_requirement,mean.cs09, type='l',col='blue',xlab="",ylab='Cover (cumul. rel cover [0-1])',main="Warmer, alpine species only")
)
with(cumsum.reg.TT2.2009.sp.alp[cumsum.reg.TT2.2009.sp.alp$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#56B4E990",border=F)
)
with(cumsum.reg.TT2.2009.sp.alp[cumsum.reg.TT2.2009.sp.alp$alt=='alpine',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)
# TT2 (sp.alp only), 2019
#with(cumsum.reg.TT2.2019[cumsum.reg.TT2.2019$alt=='alpine',],
#     plot(Heat_requirement,mean.cs19, type='l',col='blue',ylab='Cover (cumul. rel cover [0-1])',main="Alpine")
#)
with(cumsum.reg.TT2.2019.sp.alp[cumsum.reg.TT2.2019.sp.alp$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs19-2*se.cs19,rev(mean.cs19+2*se.cs19)),
             col="#E69F0090",border=F)
)
with(cumsum.reg.TT2.2019.sp.alp[cumsum.reg.TT2.2019.sp.alp$alt=='alpine',],
     points(Heat_requirement,mean.cs19, type='l',col='white')
)

with(abun.cumsums.sp.alp[abun.cumsums.sp.alp$TTtreat=="TT2" & abun.cumsums.sp.alp$alt.orig=="alpine",],points(Heat_requirement+0.04, cover_cumsum, col="#56B4E930",pch=16))
with(abun.cumsums[abun.cumsums$TTtreat=="TT2" & abun.cumsums$alt.orig=="sub-alpine",],points(Heat_requirement-0.04, cover_cumsum, col="#E69F0030",pch=16))


# TTC, 2009
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='alpine',],
     plot(Heat_requirement,mean.cs09, type='l',col='blue',xlab="",ylab='Cover (cumul. rel cover [0-1])',main="Control")
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#56B4E990",border=F)
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='alpine',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)
# TTC, 2019
#with(cumsum.reg.TTC.2019[cumsum.reg.TTC.2019$alt=='alpine',],
#     plot(Heat_requirement,mean.cs19, type='l',col='blue',ylab='Cover (cumul. rel cover [0-1])',main="Alpine")
#)
with(cumsum.reg.TTC.2019[cumsum.reg.TTC.2019$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs19-2*se.cs19,rev(mean.cs19+2*se.cs19)),
             col="#E69F0090",border=F)
)
with(cumsum.reg.TTC.2019[cumsum.reg.TTC.2019$alt=='alpine',],
     points(Heat_requirement,mean.cs19, type='l',col='white')
)

with(abun.cumsums[abun.cumsums$TTtreat=="TTC" & abun.cumsums$alt.orig=="alpine",],points(Heat_requirement+0.04, cover_cumsum, col="#56B4E930",pch=16))
with(abun.cumsums[abun.cumsums$TTtreat=="TTC" & abun.cumsums$alt.orig=="sub-alpine",],points(Heat_requirement-0.04, cover_cumsum, col="#E69F0030",pch=16))


legend('topright',legend=c("2009","2019"),fill=c("#56B4E9","#E69F00"))


## diff
# TT2 (all)
with(diff.reg.TT2[diff.reg.TT2$alt=='alpine',],
     plot(Heat_requirement,mean.diff, type='l',col='blue',ylim=c(-0.15,0.6),xlab='',ylab='Cover difference (cumul. rel cover [0-1])',main="")
)
with(diff.reg.TT2[diff.reg.TT2$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="grey70",border=F)
)
with(diff.reg.TT2[diff.reg.TT2$alt=='alpine',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)


# TT2 (sp.alp only)
with(diff.reg.TT2.sp.alp[diff.reg.TT2.sp.alp$alt=='alpine',],
     plot(Heat_requirement,mean.diff, type='l',col='blue',ylim=c(-0.15,0.6),xlab='Heat requirement',ylab='Cover difference (cumul. rel cover [0-1])',main="")
)
with(diff.reg.TT2.sp.alp[diff.reg.TT2.sp.alp$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="grey70",border=F)
)
with(diff.reg.TT2.sp.alp[diff.reg.TT2.sp.alp$alt=='alpine',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)
# TTC
with(diff.reg.TTC[diff.reg.TTC$alt=='alpine',],
     plot(Heat_requirement,mean.diff, type='l',col='blue',ylim=c(-0.15,0.6),xlab='',ylab='Cover difference (cumul. rel cover [0-1])',main="")
)
with(diff.reg.TTC[diff.reg.TTC$alt=='alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="grey70",border=F)
)
with(diff.reg.TTC[diff.reg.TTC$alt=='alpine',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)

