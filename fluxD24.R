# output files
folder.out <- paste0(output_path, "/cum_fluxes_14/temporal_resolution_influence")
dir.create(folder.out)

D24 <- paste0(folder.out, "/24h_fLux_increase.dat")



myNO <-
        Aflux[,.(
                species="NO",
                days= as.numeric(NA),
                CORE=unique(CORE),
                flux.D24=as.numeric(NA)
        ), by=.(fertilization=fertilizer, precipitation, tillage)]
myN2O <-
        Aflux[,.(
                species="N2O",
                days= as.numeric(NA),
                CORE=unique(CORE),
                flux.D24=as.numeric(NA)
        ), by=.(fertilization=fertilizer, precipitation, tillage)]
myCO2 <-
        Aflux[,.(
                species="CO2",
                days= as.numeric(NA),
                CORE=unique(CORE),
                flux.D24=as.numeric(NA)
        ), by=.(fertilization=fertilizer, precipitation, tillage)]

daily.vars <- rbindlist(list(myNO, myN2O, myCO2))
daily.vars <- daily.vars[,.(species, fertilization, precipitation, tillage, CORE, days, flux.D24)]
setkey(daily.vars, species, fertilization, precipitation, tillage)

my.times <- seq(from = step, to = 14, by =step)

aux <- data.table(days=my.times,
                  NO = as.numeric(NA),
                  N2O = as.numeric(NA),
                  CO2 = as.numeric(NA)
)

for (i in unique(Aflux$CORE)){
        mydata <- Aflux[CORE==i]
        for(times in my.times){
                iter <- mydata[days>=(times-1) & days<=times]
                if(length(iter$days)==0 & times<=1){
                        aux[days==times,NO:=NA]
                        aux[days==times,N2O:=NA]
                        aux[days==times,CO2:=NA]
                }else if(length(iter$days)!=0 & times<=1){
                        aux[days==times,NO:=tail(iter$NO,1)]
                        aux[days==times,N2O:=tail(iter$N2O,1)]
                        aux[days==times,CO2:=tail(iter$CO2,1)]
                }else{
                        aux[days==times,NO:=tail(iter$NO,1)-head(iter$NO,1)]
                        aux[days==times,N2O:=tail(iter$N2O,1)-head(iter$N2O,1)]
                        aux[days==times,CO2:=tail(iter$CO2,1)-head(iter$CO2,1)]   
                }


        }

        myNO <- aux[abs(NO)==max(abs(aux$NO), na.rm=T),.(days, NO)]
        myN2O <- aux[abs(N2O)==max(abs(aux$N2O), na.rm=T),.(days, N2O)]
        myCO2 <- aux[abs(CO2)==max(abs(aux$CO2), na.rm=T),.(days, CO2)]
        
        daily.vars[CORE==i & species=="NO", flux.D24:= myNO[1,NO]]
        daily.vars[CORE==i & species=="NO", days:= myNO[1,days]]
        
        daily.vars[CORE==i & species=="N2O", flux.D24:= myN2O[1,N2O]]
        daily.vars[CORE==i & species=="N2O", days:= myN2O[1,days]]
        
        daily.vars[CORE==i & species=="CO2", flux.D24:= myCO2[1,CO2]]
        daily.vars[CORE==i & species=="CO2", days:= myCO2[1,days]]
        
        
}

daily.vars[,abs.flux.D24:=abs(flux.D24)]

mydata <- copy(daily.vars)

no.format <- c("species", "fertilization", "precipitation", "tillage", "CORE", "days")
no.format <- names(mydata) %in% no.format
to.format <- names(mydata)[!no.format]
mydata[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format]
write.table(mydata, file= D24, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)


################################################################################
# 
# # flux files
# cum.flux.int <- paste0(input_path, "/_all_fluxes/cum_fluxes_full/all_events_cum_fluxes.dat")
# # 
# # treatments <- paste0(input_path_RAW, "/_extra/treatments.dat")
# 
# # read data files
# cum <- fread(input = cum.flux.int)
# cum[, NO:=aNO]
# cum[, N2O:=aN2O]
# cum[, CO2:=aCO2]
# cumNO <- cum[days<15, .(days, fertilization=fertilizer, precipitation, tillage, species="NO", cum.flux=NO)]
# cumN2O <- cum[days<15, .(days, fertilization=fertilizer, precipitation, tillage, species="N2O", cum.flux=N2O)]
# cumCO2 <- cum[days<15, .(days, fertilization=fertilizer, precipitation, tillage, species="CO2", cum.flux=CO2)]
# 
# cum <- rm()
# cum <- rbindlist(list(cumNO, cumN2O, cumCO2))
# 
# setkey(cum, fertilization, precipitation, tillage, species, days)
# setkey(cum, fertilization, precipitation, tillage, species)
# cum[,lag.flux:= shift(cum.flux, n=8L), by=.(fertilization, precipitation, tillage, species)]
# cum[,lag.time:= shift(days, n=8L), by=.(fertilization, precipitation, tillage, species)]
# cum[,lag.time:= (days-lag.time)*24]
# cum<-cum[lag.time==24,]
# cum[,flux.increase:= 1000*(cum.flux - lag.flux)/lag.time]
# cum[,max:= max(flux.increase, na.rm=T), by=.(fertilization, precipitation, tillage, species)]
# cum <- cum[max==flux.increase]
# 
# max.inc <- cum[,.(
#         days= days,
#         max.flux.increase = max(flux.increase, na.rm=T)
# ), by=.(species, fertilization, precipitation, tillage)
# ]
# setkey(max.inc, species, fertilization, precipitation, tillage)
# 
# # Bflux <- fread(input = B.flux.file) 
# # Cflux <- fread(input = C.flux.file) 
# # Dflux <- fread(input = D.flux.file)
# treatments <- fread(input = treatments)
# 
# setkey(Aflux, time)
# # setkey(Bflux, time)
# # setkey(Cflux, time)
# # setkey(Dflux, time)
# setkey(Aflux, treatment, time)
# # setkey(Bflux, treatment, time)
# 
