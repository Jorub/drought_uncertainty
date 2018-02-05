## Range and Variance of severity and area for runoff and soil moisture
#read data
dta <- readRDS("extremity_ens_1_EUR.rds")
library(data.table)

out_test <- function(x){
  x=as.numeric(x)
  q <- quantile(as.numeric(x),probs = c(0.25,0.75),na.rm=T)
  diff_q <- diff(q)*1.5
  lims <- c(q[1]-diff_q,q[2]+diff_q)
  if(log_val <- any(x[which(!is.na(x))]<lims[1] | x[which(!is.na(x))]>lims[2])){
   ln_out <- length(x[x[which(!is.na(x))]<lims[1] | x[which(!is.na(x))]>lims[2]])
   return(ln_out)
  }else{return(0)}
}

var_dta_met <- c()
for(i in unique(dta$REG)){
  for(k in c("q","s")){
      for(j in unique(dta$yr)){
        tmp <- which(dta$REG==i&dta$var==k&dta$yr==j)
        var_dta_met<- rbind(var_dta_met,c(i,k,j,"SEVERITY",tapply(dta$SEVERITY[tmp],dta$MET[tmp],FUN=function(x){var(x[which(!is.nan(x))],na.rm=T)})),
                        c(i,k,j,"AREA",tapply(dta$AREA[tmp],dta$MET[tmp],FUN=function(x){var(x[which(!is.nan(x))],na.rm=T)})))
      }
  }
}
x <- matrix(as.numeric(var_dta_met[,5:14]),nrow=nrow(var_dta_met))
var_dta_met <- cbind(var_dta_met
                  ,apply(x,1,mean)
                  ,apply(x,1,function(x) out_test(x)))

var_dta_par <- c()
for(i in unique(dta$REG)){
  for(k in c("q","s")){
      for(j in unique(dta$yr)){
        tmp <- which(dta$REG==i&dta$var==k&dta$yr==j)
        var_dta_par <- rbind(var_dta_par,c(i,k,j,"SEVERITY",tapply(dta$SEVERITY[tmp],dta$PAR[tmp],FUN=function(x){var(x[which(!is.nan(x))],na.rm=T)})),
                        c(i,k,j,"AREA",tapply(dta$AREA[tmp],dta$PAR[tmp],FUN=function(x){var(x[which(!is.nan(x))],na.rm=T)})))
      }
  }
}

x <- matrix(as.numeric(var_dta_par[,5:14]),nrow=nrow(var_dta_par))
var_dta_par <- cbind(var_dta_par
                  ,apply(x,1,mean)
                  ,apply(x,1,function(x) out_test(x)))


mean_dta_met <- c()
for(i in unique(dta$REG)){
  for(k in c("q","s")){
    for(j in unique(dta$yr)){
      tmp <- which(dta$REG==i&dta$var==k&dta$yr==j)
      mean_dta_met<- rbind(mean_dta_met,c(i,k,j,"SEVERITY",tapply(dta$SEVERITY[tmp],dta$MET[tmp],FUN=function(x){mean(x[which(!is.nan(x))],na.rm=T)})),
                          c(i,k,j,"AREA",tapply(dta$AREA[tmp],dta$MET[tmp],FUN=function(x){mean(x[which(!is.nan(x))],na.rm=T)})))
    }
  }
}
x <- matrix(as.numeric(mean_dta_met[,5:14]),nrow=nrow(mean_dta_met))
var_dta_met <- cbind(mean_dta_met
                     ,apply(x,1,mean)
                     ,apply(x,1,function(x) out_test(x)))

mean_dta_par <- c()
for(i in unique(dta$REG)){
  for(k in c("q","s")){
    for(j in unique(dta$yr)){
      tmp <- which(dta$REG==i&dta$var==k&dta$yr==j)
      mean_dta_par <- rbind(mean_dta_par,c(i,k,j,"SEVERITY",tapply(dta$SEVERITY[tmp],dta$PAR[tmp],FUN=function(x){mean(x[which(!is.nan(x))],na.rm=T)})),
                           c(i,k,j,"AREA",tapply(dta$AREA[tmp],dta$PAR[tmp],FUN=function(x){mean(x[which(!is.nan(x))],na.rm=T)})))
    }
  }
}

x <- matrix(as.numeric(mean_dta_par[,5:14]),nrow=nrow(mean_dta_par))
mean_dta_par <- cbind(mean_dta_par
                     ,apply(x,1,mean)
                     ,apply(x,1,function(x) out_test(x)))

range_dta_met <- c()
for(i in unique(dta$REG)){
  for(k in c("q","s")){
    for(j in unique(dta$yr)){
      tmp <- which(dta$REG==i&dta$var==k&dta$yr==j)
      range_dta_met <- rbind(range_dta_met,c(i,k,j,"SEVERITY",tapply(dta$SEVERITY[tmp],dta$MET[tmp],FUN=function(x){rg=range(x[which(!is.nan(x))],na.rm=T);rg=rg[2]-rg[1]})),
                          c(i,k,j,"AREA",tapply(dta$AREA[tmp],dta$MET[tmp],FUN=function(x){rg=range(x[which(!is.nan(x))],na.rm=T);rg=rg[2]-rg[1]})))
    }
  }
}

x <- matrix(as.numeric(range_dta_met[,5:14]),nrow=nrow(range_dta_met))
range_dta_met <- cbind(range_dta_met
                    ,apply(x,1,mean)
                    ,apply(x,1,function(x) out_test(x)))

range_dta_par <- c()
for(i in unique(dta$REG)){
  for(k in c("q","s")){
    for(j in unique(dta$yr)){
      tmp <- which(dta$REG==i&dta$var==k&dta$yr==j)
      range_dta_par <- rbind(range_dta_par,c(i,k,j,"SEVERITY",tapply(dta$SEVERITY[tmp],dta$PAR[tmp],FUN=function(x){rg=range(x[which(!is.nan(x))],na.rm=T);rg=rg[2]-rg[1]})),
                         c(i,k,j,"AREA",tapply(dta$AREA[tmp],dta$PAR[tmp],FUN=function(x){rg=range(x[which(!is.nan(x))],na.rm=T);rg=rg[2]-rg[1]})))
    }
  }
}

x <- matrix(as.numeric(range_dta_par[,5:14]),nrow=nrow(range_dta_par))
range_dta_par <- cbind(range_dta_par
                    ,apply(x,1,mean)
                    ,apply(x,1,function(x) out_test(x)))

var_dta_met <- as.data.table(var_dta_met)
var_dta_par <- as.data.table(var_dta_par)
range_dta_met <- as.data.table(range_dta_met)
range_dta_par <- as.data.table(range_dta_par)
mean_dta_met <- as.data.table(mean_dta_met)
mean_dta_par <- as.data.table(mean_dta_par)
colnames(var_dta_met)[c(1:4,15:16)] <- c("REG","var","yr","MEAS","MEAN","OUT")
colnames(var_dta_par)[c(1:4,15:16)] <- c("REG","var","yr","MEAS","MEAN","OUT")
colnames(mean_dta_met)[c(1:4,15:16)] <- c("REG","var","yr","MEAS","MEAN","OUT")
colnames(mean_dta_par)[c(1:4,15:16)] <- c("REG","var","yr","MEAS","MEAN","OUT")
colnames(range_dta_met)[c(1:4,15:16)] <- c("REG","var","yr","MEAS","MEAN","OUT")
colnames(range_dta_par)[c(1:4,15:16)] <- c("REG","var","yr","MEAS","MEAN","OUT")