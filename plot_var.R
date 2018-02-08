# plots

var_dta_met <- readRDS("var_dta_met.RDS")
var_dta_par <- readRDS("var_dta_par.RDS")
range_dta_met <- readRDS("range_dta_met.RDS")
range_dta_par <- readRDS("range_dta_par.RDS")
mean_dta_met <- readRDS("mean_dta_met.RDS")
mean_dta_par <- readRDS("mean_dta_par.RDS")

filt=which(var_dta_met$MEAS=="SEVERITY"&var_dta_met$var=="q"&var_dta_met$REG=="CEU")
plot(mean_dta_met$yr[filt],as.numeric(mean_dta_met$MEAN[filt]), type="l", col="royalblue4", xlab = "", ylab="",
     ylim=c(0,6),xlim = c(1766,2016),lwd=3)
polygon(c(mean_dta_met$yr[filt],rev(mean_dta_met$yr[filt])),
        c(as.numeric(mean_dta_met$MEAN[filt])-sqrt(as.numeric(var_dta_met$MEAN[filt])),
          rev(as.numeric(mean_dta_met$MEAN[filt])+sqrt(as.numeric(var_dta_met$MEAN[filt])))),
          col=adjustcolor("royalblue2",alpha.f = 0.5), xlab = "", ylab="",
          ylim=c(0,6),xlim = c(1766,2016), border = adjustcolor("royalblue2",alpha.f = 0.5))
par(new=T)
plot(mean_dta_par$yr[filt],as.numeric(mean_dta_par$MEAN[filt]), 
     type = "l", col="darkorange", xlab ="year", ylab="Mean SEVERITY of q",
     ylim=c(0,6),xlim = c(1766,2016))
polygon(c(mean_dta_met$yr[filt],rev(mean_dta_met$yr[filt])),
        c(as.numeric(mean_dta_par$MEAN[filt])-sqrt(as.numeric(var_dta_par$MEAN[filt])),
          rev(as.numeric(mean_dta_par$MEAN[filt])+sqrt(as.numeric(var_dta_par$MEAN[filt])))),
        col=adjustcolor("darkorange",alpha.f = 0.5), xlab = "", ylab="",
        ylim=c(0,6),xlim = c(1766,2016), border = adjustcolor("darkorange",alpha.f = 0.5))

# interesting:
# MED,q, AREA
filt=which(var_dta_met$MEAS=="AREA"&var_dta_met$var=="q"&var_dta_met$REG=="EUR")

ylims=c(0,1)
nabreak=min(which(is.na(as.numeric(mean_dta_met$MEAN[filt]))))-1
if(nabreak==Inf){
  nabreak <- length(filt)
}
plot(mean_dta_met$yr[filt],as.numeric(mean_dta_met$MEAN[filt]), type="l", col="black",xlab ="year", ylab="check the filter, duh",  
     ylim=ylims,xlim = c(1766,2016),lwd=3)
polygon(c(mean_dta_met$yr[filt[1:nabreak]],rev(mean_dta_met$yr[filt[1:nabreak]])),
        c(as.numeric(mean_dta_met$MEAN[filt[1:nabreak]])-sqrt(as.numeric(var_dta_met$MEAN[filt[1:nabreak]])),
          rev(as.numeric(mean_dta_met$MEAN[filt[1:nabreak]])+sqrt(as.numeric(var_dta_met$MEAN[filt[1:nabreak]])))),
        col=adjustcolor("royalblue2",alpha.f = 0.3), xlab = "", ylab="",
        ylim=ylims,xlim = c(1766,2016), border = adjustcolor("royalblue2",alpha.f = 0.7))
if(nabreak < length(filt)){
  polygon(c(mean_dta_met$yr[filt[(nabreak+1):length(filt)]],rev(mean_dta_met$yr[filt[(nabreak+1):length(filt)]])),
          c(as.numeric(mean_dta_met$MEAN[filt[(nabreak+1):length(filt)]])-sqrt(as.numeric(var_dta_met$MEAN[filt[(nabreak+1):length(filt)]])),
            rev(as.numeric(mean_dta_met$MEAN[filt[(nabreak+1):length(filt)]])+sqrt(as.numeric(var_dta_met$MEAN[filt[(nabreak+1):length(filt)]])))),
          col=adjustcolor("royalblue2",alpha.f = 0.3), xlab = "", ylab="",
          ylim=ylims,xlim = c(1766,2016), border = adjustcolor("royalblue2",alpha.f = 0.7))
}
par(new=T)
nabreak=which(is.na(as.numeric(var_dta_par$MEAN[filt])))-1
if(min(nabreak)==Inf){
  nabreak <- length(filt)
}
min=1
#plot(mean_dta_par$yr[filt],as.numeric(mean_dta_par$MEAN[filt]), 
 #    type = "l", col="darkorange", xlab = "", ylab="",
#     ylim=ylims,xlim = c(1766,2016))
polygon(c(mean_dta_met$yr[filt[min:nabreak[1]]],rev(mean_dta_met$yr[filt[min:nabreak[1]]])),
        c(as.numeric(mean_dta_par$MEAN[filt[min:nabreak[1]]])-sqrt(as.numeric(var_dta_par$MEAN[filt[min:nabreak[1]]])),
          rev(as.numeric(mean_dta_par$MEAN[filt[min:nabreak[1]]])+sqrt(as.numeric(var_dta_par$MEAN[filt[min:nabreak[1]]])))),
        col=adjustcolor("darkorange",alpha.f = 0.3), xlab = "", ylab="",
        ylim=ylims,xlim = c(1766,2016), border = adjustcolor("darkorange", alpha.f = 0.7))
if(nabreak[1] < length(filt)){
  nabreak=c(nabreak,length(filt))
  for(i in 1:(length(nabreak)-1)){
    polygon(c(mean_dta_met$yr[filt[(nabreak[i]+1):nabreak[i+1]]],rev(mean_dta_met$yr[filt[(nabreak[i]+1):nabreak[i+1]]])),
            c(as.numeric(mean_dta_par$MEAN[filt[(nabreak[i]+1):nabreak[i+1]]])-sqrt(as.numeric(var_dta_par$MEAN[filt[(nabreak[i]+1):nabreak[i+1]]])),
              rev(as.numeric(mean_dta_par$MEAN[filt[(nabreak[i]+1):nabreak[i+1]]])+sqrt(as.numeric(var_dta_par$MEAN[filt[(nabreak[i]+1):nabreak[i+1]]])))),
            col=adjustcolor("darkorange",alpha.f = 0.3), xlab = "", ylab="",
            ylim=ylims,xlim = c(1766,2016), border = adjustcolor("darkorange", alpha.f = 0.7))
  }

}

leg.txt=c("MET","PAR")
legend("top",leg.txt, fill=c(adjustcolor("royalblue2",alpha.f = 0.3),adjustcolor("darkorange", alpha.f = 0.3)))
