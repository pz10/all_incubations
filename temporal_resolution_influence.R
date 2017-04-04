# output files
folder.out <- paste0(output_path, "/cum_fluxes_14/temporal_resolution_influence")
dir.create(folder.out)
myNO <- paste0(folder.out, "/NO_A_1st.dat")
myN2O <- paste0(folder.out, "/N2O_A_1st.dat")
myCO2 <- paste0(folder.out, "/CO2_A_1st.dat")


# flux files
A.flux.file <- paste0(input_path, "/A_IDASw/A_fluxes/A_fluxes.dat")
# B.flux.file <- paste0(input_path, "/B_IDASw/B_fluxes/B_fluxes.dat")
# C.flux.file <- paste0(input_path, "/C_IDASw/C_fluxes/C_fluxes.dat")
# D.flux.file <- paste0(input_path, "/D_IDASw/D_fluxes/D_fluxes.dat")
treatments <- paste0(input_path_RAW, "/_extra/treatments.dat")

# read data files
Aflux <- fread(input = A.flux.file)
# Bflux <- fread(input = B.flux.file) 
# Cflux <- fread(input = C.flux.file) 
# Dflux <- fread(input = D.flux.file)
treatments <- fread(input = treatments)

setkey(Aflux, time)
# setkey(Bflux, time)
# setkey(Cflux, time)
# setkey(Dflux, time)
setkey(Aflux, treatment, time)
# setkey(Bflux, treatment, time)
# setkey(Cflux, treatment, time)
# setkey(Dflux, treatment, time)

# initial parameters
step <- 0.125 # time points where to calculate cumulative values
lastday <- 14 # last time point to be calculated
minNO <- 0 #minimum flux to be taken into account
minN2O <- 0 #minimum flux to be taken into account
minCO2 <- 0 #minimum flux to be taken into account
minCH4 <- 0 #minimum flux to be taken into account

offsetNO <- 0
offsetN2O <- 25
offsetCO2 <- 15
offsetCH4 <- 100 # we want everything in
# 
# source("1st_event_negative_IN.R")
# source("2nd_event_negative_IN.R")
# source("3rd_event_negative_IN.R")


##################
# only 1st event
Aflux <- Aflux[days>0 & days<15,]
simulations <- 1000
separation <- 0.75

# NO

# 1. get original cum flux during 1st event
################################################################################
####
mycores <- sort(unique(Aflux$CORE))
mytimes <- seq(from = 0, to = 14, by =step)
data <- data.table(simulation=-99)

count <-0
for (i in unique(Aflux$CORE)){
        count <- count+1
        
        myflux <- Aflux[CORE==i]
        
        myzero <- myflux[1,]
        myzero[, days:=0]
        myzero[, NO:=0]
        myzero[, N2O:=0]
        myzero[, CO2:=0]
        myzero[, CH4:=0]
        
        mylast <- tail(myflux[!is.na(NO) & !is.na(N2O) & !is.na(CO2)],1)
        mylast[,days:=14]
        myflux <- rbindlist(list(myzero, myflux, mylast))
        
        
        
        if(count==1){
                flux <- myflux
        }else{
                flux <- rbindlist(list(flux, myflux))
        }

}

for(m in 1:18){
        mycore <- mycores[m]
        # select target data
        mydata <- flux[!is.na(NO)
                       & CORE==mycore
                       , list(days, treatment, NO)]
        # insert origin (days == 0), with value as last measurement before origin
        # myorigin <- mydata[!is.na(NO)
        #                         & days <= 0
        #                         & treatment==m
        #                         , list(days, treatment, NO)]
        # 
        # myorigin <- tail(myorigin, 1)
        # mydata <- rbind(myorigin, mydata)
        # mydata[1, days:= 0]
        
        # calculate cumulative fluxes ('cum') for target data
        mydata[,hours:= days*24]
        mydata[NO < minNO, NO:= 0]
        
        for(k in 2:nrow(mydata)){
                mydt <- mydata[1:k,]
                mydata[k,cum:= auc(mydt$hours,mydt$NO, thresh=0)]
        }
        mydata[1,cum:= 0]
        
        # # interpolation of cumulative fluxes at 3h intervals
        # mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
        # # insert output into 'myfluxes'
        # myfluxes[count:(count+mylength-1), paste0(letters[i],"NO"):= mycum$y]
        mycum <- mydata[days==14, cum]
        
        data[1,paste0("NO.",m):= mycum]
        
}
original <- copy(data)
################################################################################

# 2. make simulations
################################################################################
mycores <- sort(unique(Aflux$CORE))
mytimes <- seq(from = 0, to = 14, by =step)

data <- data.table(simulation=1:simulations)

count <-0
for(s in 1:simulations){
        for (i in unique(Aflux$CORE)){
                count <- count+1
                
                myflux <- Aflux[CORE==i]
                
                myzero <- myflux[1,]
                myzero[, days:=0]
                myzero[, NO:=0]
                myzero[, N2O:=0]
                myzero[, CO2:=0]
                myzero[, CH4:=0]
                
                mydays <- myflux$days
                
                # get 1st value
                myid <- min(mydays)
                mydays <- mydays[mydays>(myid+separation) | mydays<(myid-separation)]
                
                for(j in 1:9){
                        id <- sample(mydays, 1)
                        myid <- c(myid, id)
                        mydays <- mydays[mydays>(id+separation) | mydays<(id-separation)]
                }
                myid <- sort(myid)
                myflux <- myflux[days %in% myid,]
                
                mylast <- tail(myflux[!is.na(NO) & !is.na(N2O) & !is.na(CO2)],1)
                mylast[,days:=14]
                myflux <- rbindlist(list(myzero, myflux, mylast))
                
                
                
                if(count==1){
                        flux <- myflux
                }else{
                        flux <- rbindlist(list(flux, myflux))
                }
        }
        
        for(m in 1:18){
                mycore <- mycores[m]
                # select target data
                mydata <- flux[!is.na(NO)
                               & CORE==mycore
                               , list(days, treatment, NO)]
                # insert origin (days == 0), with value as last measurement before origin
                # myorigin <- mydata[!is.na(NO)
                #                         & days <= 0
                #                         & treatment==m
                #                         , list(days, treatment, NO)]
                # 
                # myorigin <- tail(myorigin, 1)
                # mydata <- rbind(myorigin, mydata)
                # mydata[1, days:= 0]
                
                # calculate cumulative fluxes ('cum') for target data
                mydata[,hours:= days*24]
                mydata[NO < minNO, NO:= 0]
                
                for(k in 2:nrow(mydata)){
                        mydt <- mydata[1:k,]
                        mydata[k,cum:= auc(mydt$hours,mydt$NO, thresh=0)]
                }
                mydata[1,cum:= 0]
                
                # # interpolation of cumulative fluxes at 3h intervals
                # mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
                # # insert output into 'myfluxes'
                # myfluxes[count:(count+mylength-1), paste0(letters[i],"NO"):= mycum$y]
                mycum <- mydata[days==14, cum]
                
                data[s,paste0("NO.",m):= mycum]
                
        }
        rm(flux)
        count <-0
        cat(paste(s," "))
}
data <- rbindlist(list(original, data))
################################################################################
#write into output folder
no.format <- c("simulation")
no.format <- names(data) %in% no.format
to.format <- names(data)[!no.format]

data[, (to.format):= lapply(.SD, function(x) x/1000), .SDcols = to.format] 
data[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]    
write.table(data, file= myNO, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")


################################################################################
################################################################################
################################################################################

# N2O

# 1. get original cum flux during 1st event
################################################################################
####
mycores <- sort(unique(Aflux$CORE))
mytimes <- seq(from = 0, to = 14, by =step)
data <- data.table(simulation=-99)

count <-0
for (i in unique(Aflux$CORE)){
        count <- count+1
        
        myflux <- Aflux[CORE==i]
        
        myzero <- myflux[1,]
        myzero[, days:=0]
        myzero[, NO:=0]
        myzero[, N2O:=0]
        myzero[, CO2:=0]
        myzero[, CH4:=0]
        
        mylast <- tail(myflux[!is.na(NO) & !is.na(N2O) & !is.na(CO2)],1)
        mylast[,days:=14]
        myflux <- rbindlist(list(myzero, myflux, mylast))
        
        
        
        if(count==1){
                flux <- myflux
        }else{
                flux <- rbindlist(list(flux, myflux))
        }
}

for(m in 1:18){
        mycore <- mycores[m]
        # select target data
        mydata <- flux[!is.na(N2O)
                       & CORE==mycore
                       , list(days, treatment, N2O)]
        # insert origin (days == 0), with value as last measurement before origin
        # myorigin <- mydata[!is.na(N2O)
        #                         & days <= 0
        #                         & treatment==m
        #                         , list(days, treatment, N2O)]
        # 
        # myorigin <- tail(myorigin, 1)
        # mydata <- rbind(myorigin, mydata)
        # mydata[1, days:= 0]
        
        # calculate cumulative fluxes ('cum') for target data
        mydata[,hours:= days*24]
        mydata[N2O < minN2O, N2O:= 0]
        
        for(k in 2:nrow(mydata)){
                mydt <- mydata[1:k,]
                mydata[k,cum:= auc(mydt$hours,mydt$N2O, thresh=0)]
        }
        mydata[1,cum:= 0]
        
        # # interpolation of cumulative fluxes at 3h intervals
        # mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
        # # insert output into 'myfluxes'
        # myfluxes[count:(count+mylength-1), paste0(letters[i],"N2O"):= mycum$y]
        mycum <- mydata[days==14, cum]
        
        data[1,paste0("N2O.",m):= mycum]
        
}
original <- copy(data)
################################################################################

# 2. make simulations
################################################################################
mycores <- sort(unique(Aflux$CORE))
mytimes <- seq(from = 0, to = 14, by =step)

data <- data.table(simulation=1:simulations)

count <-0
for(s in 1:simulations){
        for (i in unique(Aflux$CORE)){
                count <- count+1
                
                myflux <- Aflux[CORE==i]
                
                myzero <- myflux[1,]
                myzero[, days:=0]
                myzero[, NO:=0]
                myzero[, N2O:=0]
                myzero[, CO2:=0]
                myzero[, CH4:=0]
                
                mydays <- myflux$days
                
                # get 1st value
                myid <- min(mydays)
                mydays <- mydays[mydays>(myid+separation) | mydays<(myid-separation)]
                
                for(j in 1:9){
                        id <- sample(mydays, 1)
                        myid <- c(myid, id)
                        mydays <- mydays[mydays>(id+separation) | mydays<(id-separation)]
                }
                myid <- sort(myid)
                myflux <- myflux[days %in% myid,]
                
                mylast <- tail(myflux[!is.na(NO) & !is.na(N2O) & !is.na(CO2)],1)
                mylast[,days:=14]
                myflux <- rbindlist(list(myzero, myflux, mylast))
                
                
                
                if(count==1){
                        flux <- myflux
                }else{
                        flux <- rbindlist(list(flux, myflux))
                }
        }
        
        for(m in 1:18){
                mycore <- mycores[m]
                # select target data
                mydata <- flux[!is.na(N2O)
                               & CORE==mycore
                               , list(days, treatment, N2O)]
                # insert origin (days == 0), with value as last measurement before origin
                # myorigin <- mydata[!is.na(N2O)
                #                         & days <= 0
                #                         & treatment==m
                #                         , list(days, treatment, N2O)]
                # 
                # myorigin <- tail(myorigin, 1)
                # mydata <- rbind(myorigin, mydata)
                # mydata[1, days:= 0]
                
                # calculate cumulative fluxes ('cum') for target data
                mydata[,hours:= days*24]
                mydata[N2O < minN2O, N2O:= 0]
                
                for(k in 2:nrow(mydata)){
                        mydt <- mydata[1:k,]
                        mydata[k,cum:= auc(mydt$hours,mydt$N2O, thresh=0)]
                }
                mydata[1,cum:= 0]
                
                # # interpolation of cumulative fluxes at 3h intervals
                # mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
                # # insert output into 'myfluxes'
                # myfluxes[count:(count+mylength-1), paste0(letters[i],"N2O"):= mycum$y]
                mycum <- mydata[days==14, cum]
                
                data[s,paste0("N2O.",m):= mycum]
                
        }
        rm(flux)
        count <-0
        cat(paste(s," "))
}
data <- rbindlist(list(original, data))
################################################################################
#write into output folder
no.format <- c("simulation")
no.format <- names(data) %in% no.format
to.format <- names(data)[!no.format]

data[, (to.format):= lapply(.SD, function(x) x/1000), .SDcols = to.format] 
data[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]    
write.table(data, file= myN2O, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")


################################################################################
################################################################################
################################################################################

# CO2

# 1. get original cum flux during 1st event
################################################################################
####
mycores <- sort(unique(Aflux$CORE))
mytimes <- seq(from = 0, to = 14, by =step)
data <- data.table(simulation=-99)

count <-0
for (i in unique(Aflux$CORE)){
        count <- count+1
        
        myflux <- Aflux[CORE==i]
        
        myzero <- myflux[1,]
        myzero[, days:=0]
        myzero[, NO:=0]
        myzero[, N2O:=0]
        myzero[, CO2:=0]
        myzero[, CH4:=0]
        
        mylast <- tail(myflux[!is.na(NO) & !is.na(N2O) & !is.na(CO2)],1)
        mylast[,days:=14]
        myflux <- rbindlist(list(myzero, myflux, mylast))
        
        
        
        if(count==1){
                flux <- myflux
        }else{
                flux <- rbindlist(list(flux, myflux))
        }
}

for(m in 1:18){
        mycore <- mycores[m]
        # select target data
        mydata <- flux[!is.na(CO2)
                       & CORE==mycore
                       , list(days, treatment, CO2)]
        # insert origin (days == 0), with value as last measurement before origin
        # myorigin <- mydata[!is.na(CO2)
        #                         & days <= 0
        #                         & treatment==m
        #                         , list(days, treatment, CO2)]
        # 
        # myorigin <- tail(myorigin, 1)
        # mydata <- rbind(myorigin, mydata)
        # mydata[1, days:= 0]
        
        # calculate cumulative fluxes ('cum') for target data
        mydata[,hours:= days*24]
        mydata[CO2 < minCO2, CO2:= 0]
        
        for(k in 2:nrow(mydata)){
                mydt <- mydata[1:k,]
                mydata[k,cum:= auc(mydt$hours,mydt$CO2, thresh=0)]
        }
        mydata[1,cum:= 0]
        
        # # interpolation of cumulative fluxes at 3h intervals
        # mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
        # # insert output into 'myfluxes'
        # myfluxes[count:(count+mylength-1), paste0(letters[i],"CO2"):= mycum$y]
        mycum <- mydata[days==14, cum]
        
        data[1,paste0("CO2.",m):= mycum]
        
}
original <- copy(data)
################################################################################

# 2. make simulations
################################################################################
mycores <- sort(unique(Aflux$CORE))
mytimes <- seq(from = 0, to = 14, by =step)

data <- data.table(simulation=1:simulations)

count <-0
for(s in 1:simulations){
        for (i in unique(Aflux$CORE)){
                count <- count+1
                
                myflux <- Aflux[CORE==i]
                
                myzero <- myflux[1,]
                myzero[, days:=0]
                myzero[, NO:=0]
                myzero[, N2O:=0]
                myzero[, CO2:=0]
                myzero[, CH4:=0]
                
                mydays <- myflux$days
                
                # get 1st value
                myid <- min(mydays)
                mydays <- mydays[mydays>(myid+separation) | mydays<(myid-separation)]
                
                for(j in 1:9){
                        id <- sample(mydays, 1)
                        myid <- c(myid, id)
                        mydays <- mydays[mydays>(id+separation) | mydays<(id-separation)]
                }
                myid <- sort(myid)
                myflux <- myflux[days %in% myid,]
                
                mylast <- tail(myflux[!is.na(NO) & !is.na(N2O) & !is.na(CO2)],1)
                mylast[,days:=14]
                myflux <- rbindlist(list(myzero, myflux, mylast))
                
                
                
                if(count==1){
                        flux <- myflux
                }else{
                        flux <- rbindlist(list(flux, myflux))
                }
        }
        
        for(m in 1:18){
                mycore <- mycores[m]
                # select target data
                mydata <- flux[!is.na(CO2)
                               & CORE==mycore
                               , list(days, treatment, CO2)]
                # insert origin (days == 0), with value as last measurement before origin
                # myorigin <- mydata[!is.na(CO2)
                #                         & days <= 0
                #                         & treatment==m
                #                         , list(days, treatment, CO2)]
                # 
                # myorigin <- tail(myorigin, 1)
                # mydata <- rbind(myorigin, mydata)
                # mydata[1, days:= 0]
                
                # calculate cumulative fluxes ('cum') for target data
                mydata[,hours:= days*24]
                mydata[CO2 < minCO2, CO2:= 0]
                
                for(k in 2:nrow(mydata)){
                        mydt <- mydata[1:k,]
                        mydata[k,cum:= auc(mydt$hours,mydt$CO2, thresh=0)]
                }
                mydata[1,cum:= 0]
                
                # # interpolation of cumulative fluxes at 3h intervals
                # mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
                # # insert output into 'myfluxes'
                # myfluxes[count:(count+mylength-1), paste0(letters[i],"CO2"):= mycum$y]
                mycum <- mydata[days==14, cum]
                
                data[s,paste0("CO2.",m):= mycum]
                
        }
        rm(flux)
        count <-0
        cat(paste(s," "))
}
data <- rbindlist(list(original, data))
################################################################################
#write into output folder
no.format <- c("simulation")
no.format <- names(data) %in% no.format
to.format <- names(data)[!no.format]

data[, (to.format):= lapply(.SD, function(x) x/1000), .SDcols = to.format] 
data[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]    
write.table(data, file= myCO2, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")


# visualization
################################################################################
# NO

data <- fread(input = myNO)

with(data, boxplot(NO.1, main="NO.1"))
abline(h=data$NO.1[1], col="red")

with(data, boxplot(NO.2, main="NO.2"))
abline(h=data$NO.2[1], col="red")

with(data, boxplot(NO.3, main="NO.3"))
abline(h=data$NO.3[1], col="red")

with(data, boxplot(NO.4, main="NO.4"))
abline(h=data$NO.4[1], col="red")

with(data, boxplot(NO.5, main="NO.5"))
abline(h=data$NO.5[1], col="red")

with(data, boxplot(NO.6, main="NO.6"))
abline(h=data$NO.6[1], col="red")

with(data, boxplot(NO.7, main="NO.7"))
abline(h=data$NO.7[1], col="red")

with(data, boxplot(NO.8, main="NO.8"))
abline(h=data$NO.8[1], col="red")

with(data, boxplot(NO.9, main="NO.9"))
abline(h=data$NO.9[1], col="red")

with(data, boxplot(NO.10, main="NO.10"))
abline(h=data$NO.10[1], col="red")

with(data, boxplot(NO.11, main="NO.11"))
abline(h=data$NO.11[1], col="red")

with(data, boxplot(NO.12, main="NO.12"))
abline(h=data$NO.12[1], col="red")

with(data, boxplot(NO.13, main="NO.13"))
abline(h=data$NO.13[1], col="red")

with(data, boxplot(NO.14, main="NO.14"))
abline(h=data$NO.14[1], col="red")

with(data, boxplot(NO.15, main="NO.15"))
abline(h=data$NO.15[1], col="red")

with(data, boxplot(NO.16, main="NO.16"))
abline(h=data$NO.16[1], col="red")

with(data, boxplot(NO.17, main="NO.17"))
abline(h=data$NO.17[1], col="red")

with(data, boxplot(NO.18, main="NO.18"))
abline(h=data$NO.18[1], col="red")

##
no.format <- c("simulation")
no.format <- names(data) %in% no.format
to.format <- names(data)[!no.format]

mydata <- copy(data)
mydata[, (to.format):= lapply(.SD, function(x) 100*x/x[1]), .SDcols = to.format]

all <- with(mydata,c(NO.1, NO.2, NO.3, NO.4, NO.5, NO.6, NO.7, NO.8, NO.9,
         NO.10, NO.11, NO.12, NO.13, NO.14, NO.15, NO.16, NO.17, NO.18))

boxplot(all, main="NO all")
abline(h=100, col="red")
summary(all)
quantile(all, probs=c(0.05,0.25,0.5,0.75,0.95))

################################################################################
# N2O

data <- fread(input = myN2O)

with(data, boxplot(N2O.1, main="N2O.1"))
abline(h=data$N2O.1[1], col="red")

with(data, boxplot(N2O.2, main="N2O.2"))
abline(h=data$N2O.2[1], col="red")

with(data, boxplot(N2O.3, main="N2O.3"))
abline(h=data$N2O.3[1], col="red")

with(data, boxplot(N2O.4, main="N2O.4"))
abline(h=data$N2O.4[1], col="red")

with(data, boxplot(N2O.5, main="N2O.5"))
abline(h=data$N2O.5[1], col="red")

with(data, boxplot(N2O.6, main="N2O.6"))
abline(h=data$N2O.6[1], col="red")

with(data, boxplot(N2O.7, main="N2O.7"))
abline(h=data$N2O.7[1], col="red")

with(data, boxplot(N2O.8, main="N2O.8"))
abline(h=data$N2O.8[1], col="red")

with(data, boxplot(N2O.9, main="N2O.9"))
abline(h=data$N2O.9[1], col="red")

with(data, boxplot(N2O.10, main="N2O.10"))
abline(h=data$N2O.10[1], col="red")

with(data, boxplot(N2O.11, main="N2O.11"))
abline(h=data$N2O.11[1], col="red")

with(data, boxplot(N2O.12, main="N2O.12"))
abline(h=data$N2O.12[1], col="red")

with(data, boxplot(N2O.13, main="N2O.13"))
abline(h=data$N2O.13[1], col="red")

with(data, boxplot(N2O.14, main="N2O.14"))
abline(h=data$N2O.14[1], col="red")

with(data, boxplot(N2O.15, main="N2O.15"))
abline(h=data$N2O.15[1], col="red")

with(data, boxplot(N2O.16, main="N2O.16"))
abline(h=data$N2O.16[1], col="red")

with(data, boxplot(N2O.17, main="N2O.17"))
abline(h=data$N2O.17[1], col="red")

with(data, boxplot(N2O.18, main="N2O.18"))
abline(h=data$N2O.18[1], col="red")

##
N2O.format <- c("simulation")
N2O.format <- names(data) %in% N2O.format
to.format <- names(data)[!N2O.format]

mydata <- copy(data)
mydata[, (to.format):= lapply(.SD, function(x) 100*x/x[1]), .SDcols = to.format]

all <- with(mydata,c(N2O.1, N2O.2, N2O.3, N2O.4, N2O.5, N2O.6, N2O.7, N2O.8, N2O.9,
                     N2O.10, N2O.11, N2O.12, N2O.13, N2O.14, N2O.15, N2O.16, N2O.17, N2O.18))

boxplot(all, main="N2O all")
abline(h=100, col="red")
summary(all)
quantile(all, probs=c(0.05,0.25,0.5,0.75,0.95))

################################################################################
# CO2

data <- fread(input = myCO2)

with(data, boxplot(CO2.1, main="CO2.1"))
abline(h=data$CO2.1[1], col="red")

with(data, boxplot(CO2.2, main="CO2.2"))
abline(h=data$CO2.2[1], col="red")

with(data, boxplot(CO2.3, main="CO2.3"))
abline(h=data$CO2.3[1], col="red")

with(data, boxplot(CO2.4, main="CO2.4"))
abline(h=data$CO2.4[1], col="red")

with(data, boxplot(CO2.5, main="CO2.5"))
abline(h=data$CO2.5[1], col="red")

with(data, boxplot(CO2.6, main="CO2.6"))
abline(h=data$CO2.6[1], col="red")

with(data, boxplot(CO2.7, main="CO2.7"))
abline(h=data$CO2.7[1], col="red")

with(data, boxplot(CO2.8, main="CO2.8"))
abline(h=data$CO2.8[1], col="red")

with(data, boxplot(CO2.9, main="CO2.9"))
abline(h=data$CO2.9[1], col="red")

with(data, boxplot(CO2.10, main="CO2.10"))
abline(h=data$CO2.10[1], col="red")

with(data, boxplot(CO2.11, main="CO2.11"))
abline(h=data$CO2.11[1], col="red")

with(data, boxplot(CO2.12, main="CO2.12"))
abline(h=data$CO2.12[1], col="red")

with(data, boxplot(CO2.13, main="CO2.13"))
abline(h=data$CO2.13[1], col="red")

with(data, boxplot(CO2.14, main="CO2.14"))
abline(h=data$CO2.14[1], col="red")

with(data, boxplot(CO2.15, main="CO2.15"))
abline(h=data$CO2.15[1], col="red")

with(data, boxplot(CO2.16, main="CO2.16"))
abline(h=data$CO2.16[1], col="red")

with(data, boxplot(CO2.17, main="CO2.17"))
abline(h=data$CO2.17[1], col="red")

with(data, boxplot(CO2.18, main="CO2.18"))
abline(h=data$CO2.18[1], col="red")

##
CO2.format <- c("simulation")
CO2.format <- names(data) %in% CO2.format
to.format <- names(data)[!CO2.format]

mydata <- copy(data)
mydata[, (to.format):= lapply(.SD, function(x) 100*x/x[1]), .SDcols = to.format]

all <- with(mydata,c(CO2.1, CO2.2, CO2.3, CO2.4, CO2.5, CO2.6, CO2.7, CO2.8, CO2.9,
                     CO2.10, CO2.11, CO2.12, CO2.13, CO2.14, CO2.15, CO2.16, CO2.17, CO2.18))

boxplot(all, main="CO2 all")
abline(h=100, col="red")
summary(all)
quantile(all, probs=c(0.05,0.25,0.5,0.75,0.95))
