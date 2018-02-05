## Range and Variance of severity and area for runoff and soil moisture
#read data
dta <- readRDS("extremity_ens_1_EUR.rds")

var_dta_met <- c()
for(i in unique(dta$REG)){
  for(k in c("q","s")){
      tmp <- which(dta$REG==i&dta$var==k)
      var_dta_met<- rbind(var_dta_met,c(i,k,"SEVERITY",tapply(dta$SEVERITY[tmp],dta$MET[tmp],FUN=function(x){var(x[which(!is.nan(x))],na.rm=T)})),
                          c(i,k,j,"AREA",tapply(dta$AREA[tmp],dta$MET[tmp],FUN=function(x){var(x[which(!is.nan(x))],na.rm=T)})))
    }
}
x <- matrix(as.numeric(var_dta_met[,4:13]),nrow=12)
var_dta_met <- cbind(var_dta_met,apply(x,1,mean))

var_dta_par <- c()
for(i in unique(dta$REG)){
  for(k in c("q","s")){
    tmp <- which(dta$REG==i&dta$var==k)
    var_dta_par <- rbind(var_dta_par,c(i,k,"SEVERITY",tapply(dta$SEVERITY[tmp],dta$PAR[tmp],FUN=function(x){var(x[which(!is.nan(x))],na.rm=T)})),
                         c(i,k,"AREA",tapply(dta$AREA[tmp],dta$PAR[tmp],FUN=function(x){var(x[which(!is.nan(x))],na.rm=T)})))
  }
}

x <- matrix(as.numeric(var_dta_par[,4:13]),nrow=12)
var_dta_par <- cbind(var_dta_par,apply(x,1,mean))

range_dta_met <- c()
for(i in unique(dta$REG)){
  for(k in c("q","s")){
    tmp <- which(dta$REG==i&dta$var==k)
    range_dta_met <- rbind(range_dta_met,c(i,k,"SEVERITY",tapply(dta$SEVERITY[tmp],dta$MET[tmp],FUN=function(x){rg=range(x[which(!is.nan(x))],na.rm=T);rg=rg[2]-rg[1]})),
                           c(i,k,"AREA",tapply(dta$AREA[tmp],dta$MET[tmp],FUN=function(x){rg=range(x[which(!is.nan(x))],na.rm=T);rg=rg[2]-rg[1]})))
    
  }
}

x <- matrix(as.numeric(range_dta_met[,4:13]),nrow=12)
range_dta_met <- cbind(range_dta_met,apply(x,1,mean))

range_dta_par <- c()
for(i in unique(dta$REG)){
  for(k in c("q","s")){
    tmp <- which(dta$REG==i&dta$var==k)
    range_dta_par <- rbind(range_dta_par,c(i,k,"SEVERITY",tapply(dta$SEVERITY[tmp],dta$PAR[tmp],FUN=function(x){rg=range(x[which(!is.nan(x))],na.rm=T);rg=rg[2]-rg[1]})),
                           c(i,k,"AREA",tapply(dta$AREA[tmp],dta$PAR[tmp],FUN=function(x){rg=range(x[which(!is.nan(x))],na.rm=T);rg=rg[2]-rg[1]})))
  }
}

x <- matrix(as.numeric(range_dta_par[,4:13]),nrow=12)
range_dta_par <- cbind(range_dta_par,apply(x,1,mean))

var_dta_met <- as.data.table(var_dta_met)
var_dta_par <- as.data.table(var_dta_par)
range_dta_met <- as.data.table(range_dta_met)
range_dta_par <- as.data.table(range_dta_par)
colnames(var_dta_met)[c(1:3,14)] <- c("REG","var","MEAS","MEAN")
colnames(var_dta_par)[c(1:3,14)] <- c("REG","var","MEAS","MEAN")
colnames(range_dta_met)[c(1:3,14)] <- c("REG","var","MEAS","MEAN")
colnames(range_dta_par)[c(1:3,14)] <- c("REG","var","MEAS","MEAN")