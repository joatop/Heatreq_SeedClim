#### difference plot ####
x11()
par(mfrow=c(2,3))

# warmer, alpine
with(diff.cumsum[diff.cumsum$TTtreat=='TT2' & 
                   diff.cumsum$alt.orig=='alpine',],
     plot(Heat_requirement,diff.cumsum,pch=16,cex=0.8,col='#56B4E930',
          xlab='Heat requirement',ylab='Cover diff. (cumul. rel cover [0-1])',main="Alpine, warmed climate")
)
# 2019
with(pred.diff_TT2_alpine[pred.diff_TT2_alpine$year==2019,],
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#56B4E980",border="#56B4E980")
)
with(pred.diff_TT2_alpine[pred.diff_TT2_alpine$year==2019,],
     points(1:10,fit.resp, col='white',type='l',lwd=2)
)

# the other years (every 2nd)
with(pred.diff_TT2_alpine[pred.diff_TT2_alpine$year==2017,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=1)
)
with(pred.diff_TT2_alpine[pred.diff_TT2_alpine$year==2015,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=2)
)
with(pred.diff_TT2_alpine[pred.diff_TT2_alpine$year==2013,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=3)
)
with(pred.diff_TT2_alpine[pred.diff_TT2_alpine$year==2011,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=4)
)

#for (i in 1:length(unique(pred.diff_TT2_alpine$year))) {
#with(pred.diff_TT2_alpine[pred.diff_TT2_alpine$year==unique(pred.diff_TT2_alpine$year)[i],],
#     points(1:10, fit.resp, col='grey', lty=i, type='l', lwd=2)
#)
#}

abline(h=0,lty=2)

legend("topright", legend=c("2011", "2013", "2015", "2017", "2019"), lty= c(4,3,2,1,1), col= c(rep("grey",4),"#56B4E9"))


# warmer, alpine, alpine species only 
with(diff.cumsum.sp.alp[diff.cumsum.sp.alp$TTtreat=='TT2' & 
                   diff.cumsum.sp.alp$alt.orig=='alpine',],
     plot(Heat_requirement,diff.cumsum,pch=16,cex=0.8,col='#56B4E930',
          xlab='Heat requirement',ylab='Cover diff. (cumul. rel cover [0-1])',main="Alpine (alpine sp only), warmed climate")
)
# 2019
with(pred.diff_TT2_alpine.sp.alp[pred.diff_TT2_alpine.sp.alp$year==2019,],
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#56B4E980",border="#56B4E980")
)
with(pred.diff_TT2_alpine.sp.alp[pred.diff_TT2_alpine.sp.alp$year==2019,],
     points(1:10,fit.resp, col='white',type='l',lwd=2)
)

# the other years (every 2nd)
with(pred.diff_TT2_alpine.sp.alp[pred.diff_TT2_alpine.sp.alp$year==2017,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=1)
)
with(pred.diff_TT2_alpine.sp.alp[pred.diff_TT2_alpine.sp.alp$year==2015,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=2)
)
with(pred.diff_TT2_alpine.sp.alp[pred.diff_TT2_alpine.sp.alp$year==2013,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=3)
)
with(pred.diff_TT2_alpine.sp.alp[pred.diff_TT2_alpine.sp.alp$year==2011,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=4)
)

abline(h=0,lty=2)


# extant, alpine
with(diff.cumsum[diff.cumsum$TTtreat=='TTC' & 
                   diff.cumsum$alt.orig=='alpine',],
     plot(Heat_requirement,diff.cumsum,pch=16,cex=0.8,col='#56B4E930',
          xlab='Heat requirement',ylab='Cover diff. (cumul. rel cover [0-1])',main="Alpine, extant climate")
)
with(pred.diff_TTC_alpine[pred.diff_TTC_alpine$year==2019,],
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#56B4E980",border="#56B4E980")
)
with(pred.diff_TTC_alpine[pred.diff_TTC_alpine$year==2019,],
     points(1:10,fit.resp, col='white',type='l',lwd=2)
)

# the other years (every 2nd)
with(pred.diff_TTC_alpine[pred.diff_TTC_alpine$year==2017,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=1)
)
with(pred.diff_TTC_alpine[pred.diff_TTC_alpine$year==2015,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=2)
)
with(pred.diff_TTC_alpine[pred.diff_TTC_alpine$year==2013,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=3)
)
with(pred.diff_TTC_alpine[pred.diff_TTC_alpine$year==2011,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=4)
)

abline(h=0,lty=2)



# warmer, sub-alpine
with(diff.cumsum[diff.cumsum$TTtreat=='TT2' & 
                   diff.cumsum$alt.orig=='sub-alpine',],
     plot(Heat_requirement,diff.cumsum,pch=16,cex=0.8,col='#E69F0030',
          xlab='Heat requirement',ylab='Cover diff. (cumul. rel cover [0-1])',main="Sub-alpine, warmed climate")
)
with(pred.diff_TT2_subalpine[pred.diff_TT2_subalpine$year==2019,],
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#E69F0080",border="#E69F0080")
)
with(pred.diff_TT2_subalpine[pred.diff_TT2_subalpine$year==2019,],
     points(1:10,fit.resp, col='white',type='l',lwd=2)
)

# the other years (every 2nd)
with(pred.diff_TT2_subalpine[pred.diff_TT2_subalpine$year==2017,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=1)
)
with(pred.diff_TT2_subalpine[pred.diff_TT2_subalpine$year==2015,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=2)
)
with(pred.diff_TT2_subalpine[pred.diff_TT2_subalpine$year==2013,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=3)
)
with(pred.diff_TT2_subalpine[pred.diff_TT2_subalpine$year==2011,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=4)
)

abline(h=0,lty=2)



# warmer, sub-alpine, sub-alpine species only
with(diff.cumsum.sp.subalp[diff.cumsum.sp.subalp$TTtreat=='TT2' & 
                   diff.cumsum.sp.subalp$alt.orig=='sub-alpine',],
     plot(Heat_requirement,diff.cumsum,pch=16,cex=0.8,col='#E69F0030',
          xlab='Heat requirement',ylab='Cover diff. (cumul. rel cover [0-1])',main="Sub-alpine (subalp sp only), warmed climate")
)
with(pred.diff_TT2_subalpine.sp.subalp[pred.diff_TT2_subalpine.sp.subalp$year==2019,],
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#E69F0080",border="#E69F0080")
)
with(pred.diff_TT2_subalpine.sp.subalp[pred.diff_TT2_subalpine.sp.subalp$year==2019,],
     points(1:10,fit.resp, col='white',type='l',lwd=2)
)

# the other years (every 2nd)
with(pred.diff_TT2_subalpine.sp.subalp[pred.diff_TT2_subalpine.sp.subalp$year==2017,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=1)
)
with(pred.diff_TT2_subalpine.sp.subalp[pred.diff_TT2_subalpine.sp.subalp$year==2015,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=2)
)
with(pred.diff_TT2_subalpine.sp.subalp[pred.diff_TT2_subalpine.sp.subalp$year==2013,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=3)
)
with(pred.diff_TT2_subalpine.sp.subalp[pred.diff_TT2_subalpine.sp.subalp$year==2011,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=4)
)

abline(h=0,lty=2)


# extant, sub-alpine
with(diff.cumsum[diff.cumsum$TTtreat=='TTC' & 
                   diff.cumsum$alt.orig=='sub-alpine',],
     plot(Heat_requirement,diff.cumsum,pch=16,cex=0.8,col='#E69F0030',
          xlab='Heat requirement',ylab='Cover diff. (cumul. rel cover [0-1])',main="Sub-alpine, extant climate")
)
with(pred.diff_TTC_subalpine[pred.diff_TTC_subalpine$year==2019,],
     polygon(x=c(Hrf,rev(Hrf)),y=c(lo,rev(up)),col="#E69F0080",border="#E69F0080")
)
with(pred.diff_TTC_subalpine[pred.diff_TTC_subalpine$year==2019,],
     points(1:10,fit.resp, col='white',type='l',lwd=2)
)

# the other years (every 2nd)
with(pred.diff_TTC_subalpine[pred.diff_TTC_subalpine$year==2017,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=1)
)
with(pred.diff_TTC_subalpine[pred.diff_TTC_subalpine$year==2015,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=2)
)
with(pred.diff_TTC_subalpine[pred.diff_TTC_subalpine$year==2013,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=3)
)
with(pred.diff_TTC_subalpine[pred.diff_TTC_subalpine$year==2011,],
     points(1:10,fit.resp, col='grey',type='l',lwd=2, lty=4)
)

abline(h=0,lty=2)

