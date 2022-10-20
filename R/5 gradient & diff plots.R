#### difference plot ####
x11()
par(mfrow=c(2,2))
# warmer
# alpine
with(diff.cumsum[diff.cumsum$TTtreat=='TT2' & 
                   diff.cumsum$alt.orig=='alpine',],
     plot(Heat_requirement,diff.cumsum,pch=16,cex=0.8,col='#56B4E930',
          xlab='Heat requirement',ylab='Cover (cumul. rel cover [0-1])',main="Alpine, warmed climate")
)
with(pred.diff_TT2_alpine,
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#56B4E980",border="#56B4E980")
)
with(pred.diff_TT2_alpine,
     points(1:10,fit.resp, col='white',type='l',lwd=2)
)
abline(h=0,lty=2)


# sub-alpine
with(diff.cumsum[diff.cumsum$TTtreat=='TT2' & 
                   diff.cumsum$alt.orig=='sub-alpine',],
     plot(Heat_requirement,diff.cumsum,pch=16,cex=0.8,col='#E69F0030',
          xlab='Heat requirement',ylab='Cover (cumul. rel cover [0-1])',main="Sub-alpine, warmed climate")
)
with(pred.diff_TT2_subalpine,
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#E69F0080",border="#E69F0080")
)
with(pred.diff_TT2_subalpine,
     points(1:10,fit.resp, col='white',type='l',lwd=2)
)
abline(h=0,lty=2)



# extant
# alpine
with(diff.cumsum[diff.cumsum$TTtreat=='TTC' & 
                   diff.cumsum$alt.orig=='alpine',],
     plot(Heat_requirement,diff.cumsum,pch=16,cex=0.8,col='#56B4E930',
          xlab='Heat requirement',ylab='Cover (cumul. rel cover [0-1])',main="Alpine, extant climate")
)
with(pred.diff_TTC_alpine,
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#56B4E980",border="#56B4E980")
)
with(pred.diff_TTC_alpine,
     points(1:10,fit.resp, col='white',type='l',lwd=2)
)
abline(h=0,lty=2)


# sub-alpine
with(diff.cumsum[diff.cumsum$TTtreat=='TTC' & 
                   diff.cumsum$alt.orig=='sub-alpine',],
     plot(Heat_requirement,diff.cumsum,pch=16,cex=0.8,col='#E69F0030',
          xlab='Heat requirement',ylab='Cover (cumul. rel cover [0-1])',main="Sub-alpine, extant climate")
)
with(pred.diff_TTC_subalpine,
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#E69F0080",border="#E69F0080")
)
with(pred.diff_TTC_subalpine,
     points(1:10,fit.resp, col='white',type='l',lwd=2)
)
abline(h=0,lty=2)

