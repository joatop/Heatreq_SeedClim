

#### plots for cover and difference ####
#### sub-alpine ####
par(mfrow=c(2,3))
## cover
# TT2 (all), 2009
with(cumsum.reg.TT2.2009[cumsum.reg.TT2.2009$alt=='sub-alpine',],
     plot(Heat_requirement,mean.cs09, type='l',col='blue',xlab="",ylab='Cover (cumul. rel cover [0-1])',main="Warmer, all species")
)
with(cumsum.reg.TT2.2009[cumsum.reg.TT2.2009$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#56B4E990",border=F)
)
with(cumsum.reg.TT2.2009[cumsum.reg.TT2.2009$alt=='sub-alpine',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)
# TT2 (all), 2019
#with(cumsum.reg.TT2.2019[cumsum.reg.TT2.2019$alt=='sub-alpine',],
#     plot(Heat_requirement,mean.cs19, type='l',col='blue',ylab='Cover (cumul. rel cover [0-1])',main="sub-alpine")
#)
with(cumsum.reg.TT2.2019[cumsum.reg.TT2.2019$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs19-2*se.cs19,rev(mean.cs19+2*se.cs19)),
             col="#E69F0090",border=F)
)
with(cumsum.reg.TT2.2019[cumsum.reg.TT2.2019$alt=='sub-alpine',],
     points(Heat_requirement,mean.cs19, type='l',col='white')
)
# TT2 (sp.subalp only), 2009
with(cumsum.reg.TT2.2009.sp.subalp[cumsum.reg.TT2.2009.sp.subalp$alt=='sub-alpine',],
     plot(Heat_requirement,mean.cs09, type='l',col='blue',xlab="",ylab='Cover (cumul. rel cover [0-1])',main="Warmer, sub-alpine species only")
)
with(cumsum.reg.TT2.2009.sp.subalp[cumsum.reg.TT2.2009.sp.subalp$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#56B4E990",border=F)
)
with(cumsum.reg.TT2.2009.sp.subalp[cumsum.reg.TT2.2009.sp.subalp$alt=='sub-alpine',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)
# TT2 (sp.subalp only), 2019
#with(cumsum.reg.TT2.2019[cumsum.reg.TT2.2019$alt=='sub-alpine',],
#     plot(Heat_requirement,mean.cs19, type='l',col='blue',ylab='Cover (cumul. rel cover [0-1])',main="sub-alpine")
#)
with(cumsum.reg.TT2.2019.sp.subalp[cumsum.reg.TT2.2019.sp.subalp$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs19-2*se.cs19,rev(mean.cs19+2*se.cs19)),
             col="#E69F0090",border=F)
)
with(cumsum.reg.TT2.2019.sp.subalp[cumsum.reg.TT2.2019.sp.subalp$alt=='sub-alpine',],
     points(Heat_requirement,mean.cs19, type='l',col='white')
)

# TTC, 2009
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='sub-alpine',],
     plot(Heat_requirement,mean.cs09, type='l',col='blue',xlab="",ylab='Cover (cumul. rel cover [0-1])',main="Control")
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs09-2*se.cs09,rev(mean.cs09+2*se.cs09)),
             col="#56B4E990",border=F)
)
with(cumsum.reg.TTC.2009[cumsum.reg.TTC.2009$alt=='sub-alpine',],
     points(Heat_requirement,mean.cs09, type='l',col='white')
)
# TTC, 2019
#with(cumsum.reg.TTC.2019[cumsum.reg.TTC.2019$alt=='sub-alpine',],
#     plot(Heat_requirement,mean.cs19, type='l',col='blue',ylab='Cover (cumul. rel cover [0-1])',main="sub-alpine")
#)
with(cumsum.reg.TTC.2019[cumsum.reg.TTC.2019$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.cs19-2*se.cs19,rev(mean.cs19+2*se.cs19)),
             col="#E69F0090",border=F)
)
with(cumsum.reg.TTC.2019[cumsum.reg.TTC.2019$alt=='sub-alpine',],
     points(Heat_requirement,mean.cs19, type='l',col='white')
)
legend('topright',legend=c("2009","2019"),fill=c("#56B4E9","#E69F00"))


## diff
# TT2 (all)
with(diff.reg.TT2[diff.reg.TT2$alt=='sub-alpine',],
     plot(Heat_requirement,mean.diff, type='l',col='blue',ylim=c(-0.15,0.6),xlab='',ylab='Cover difference (cumul. rel cover [0-1])',main="")
)
with(diff.reg.TT2[diff.reg.TT2$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="grey70",border=F)
)
with(diff.reg.TT2[diff.reg.TT2$alt=='sub-alpine',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)


# TT2 (sp.subalp only)
with(diff.reg.TT2.sp.subalp[diff.reg.TT2.sp.subalp$alt=='sub-alpine',],
     plot(Heat_requirement,mean.diff, type='l',col='blue',ylim=c(-0.15,0.6),xlab='Heat requirement',ylab='Cover difference (cumul. rel cover [0-1])',main="")
)
with(diff.reg.TT2.sp.subalp[diff.reg.TT2.sp.subalp$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="grey70",border=F)
)
with(diff.reg.TT2.sp.subalp[diff.reg.TT2.sp.subalp$alt=='sub-alpine',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)
# TTC
with(diff.reg.TTC[diff.reg.TTC$alt=='sub-alpine',],
     plot(Heat_requirement,mean.diff, type='l',col='blue',ylim=c(-0.15,0.6),xlab='',ylab='Cover difference (cumul. rel cover [0-1])',main="")
)
with(diff.reg.TTC[diff.reg.TTC$alt=='sub-alpine',],
     polygon(c(1:10,rev(1:10)),
             c(mean.diff-2*se.diff,rev(mean.diff+2*se.diff)),
             col="grey70",border=F)
)
with(diff.reg.TTC[diff.reg.TTC$alt=='sub-alpine',],
     points(Heat_requirement,mean.diff, type='l',col='white')
)
abline(h=0,lty=2)

