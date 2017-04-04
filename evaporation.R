# output files
folder.out <- paste0(output_path, "/cum_fluxes_14/_evaporation_by_event")
dir.create(folder.out)
# cum.flux.1st <- paste0(folder.out, "/1st_event_cum_fluxes.dat")
# cum.flux.2nd <- paste0(folder.out, "/2nd_event_cum_fluxes.dat")
# cum.flux.3rd <- paste0(folder.out, "/3rd_event_cum_fluxes.dat")
# dir.create(folder.out)

# flux files
A.flux.file <- paste0(input_path, "/A_IDASw/A_fluxes/A_fluxes.dat")
B.flux.file <- paste0(input_path, "/B_IDASw/B_fluxes/B_fluxes.dat")
C.flux.file <- paste0(input_path, "/C_IDASw/C_fluxes/C_fluxes.dat")
D.flux.file <- paste0(input_path, "/D_IDASw/D_fluxes/D_fluxes.dat")
treatments <- paste0(input_path_RAW, "/_extra/treatments.dat")

# read data files
Aflux <- fread(input = A.flux.file)
Bflux <- fread(input = B.flux.file) 
Cflux <- fread(input = C.flux.file) 
Dflux <- fread(input = D.flux.file)

Aflux[, incubation:="A"]
Bflux[, incubation:="B"]
Cflux[, incubation:="C"]
Dflux[, incubation:="D"]
data <- rbindlist(list(Aflux, Bflux, Cflux, Dflux))

# 
data <- data[,.(days.adjusted, days.adj.full, incubation,  CORE, fertilization = fertilizer, precipitation, tillage,
                water, addedwater)]

data[, event:= 0]
data[days.adjusted>=0, event:= 1]
data[days.adjusted>=15, event:= 2]
data[days.adjusted>=30, event:= 3]

data[,span:= days.adj.full - shift(days.adj.full,1), by= CORE]
data[, evap:= addedwater - shift(addedwater,1), by=CORE]
data[evap>0, evap:= NA]
data[, evap.rate:= evap/span]

data[evap>0, evap:= NA]
data[evap.rate < -25, evap.rate:=NA]
data[event==0, evap.rate:=NA]

evap <- data[,.(
        median = median(evap.rate, na.rm=T),
        mad = mad(evap.rate, na.rm=T),
        
        mean = mean(evap.rate, na.rm=T),
        sd = sd(evap.rate, na.rm=T)
), by= .(incubation, fertilization, precipitation, tillage)]

# 
evap[, .(
        mean = mean(median),
        sd = sd(median),
        se = sd(median)/sqrt(sum(!is.na(median)))
), by= .(precipitation)]

# 
evap[, .(
        mean = mean(median),
        sd = sd(median),
        se = sd(median)/sqrt(sum(!is.na(median)))
), by= .(tillage)]

# 
evap[, .(
        mean = mean(median)/area,
        sd = sd(median)/area,
        se = sd(median)/area/sqrt(sum(!is.na(median)))
), by= .(tillage)]
# 
evap[, .(
        mean = mean(median)/area/1000,
        sd = sd(median)/area/1000,
        se = sd(median)/area/1000/sqrt(sum(!is.na(median)))
), by= .(tillage)]
# 
evap[, .(
        mean = mean(median)/area/1000*30,
        sd = sd(median)/area/1000*30,
        se = sd(median)/area/1000/sqrt(sum(!is.na(median)))*30
), by= .(tillage)]

#################
evap.ev <- data[,.(
        median = median(evap.rate, na.rm=T),
        mad = mad(evap.rate, na.rm=T),
        
        mean = mean(evap.rate, na.rm=T),
        sd = sd(evap.rate, na.rm=T)
), by= .(event, incubation, fertilization, precipitation, tillage)]

evap.ev[, .(
        mean = mean(median),
        sd = sd(median),
        se = sd(median)/sqrt(sum(!is.na(median)))
), by= .(event, precipitation)]

################################################################################

month <- evap[incubation=="D", .(
        mean = mean(median)/area/1000*30,
        sd = sd(median)/area/1000*30,
        se = sd(median)/area/1000/sqrt(sum(!is.na(median)))*30
)] 

month <- evap[, .(
        mean = mean(median)/area/1000*30,
        sd = sd(median)/area/1000*30,
        se = sd(median)/area/1000/sqrt(sum(!is.na(median)))*30
)] 






# treatments <- fread(input = treatments)

# events
A.e <- paste0(input_path_RAW, "/A_extra/A_rain_events_time.dat")
B.e <- paste0(input_path_RAW, "/B_extra/B_rain_events_time.dat")
C.e <- paste0(input_path_RAW, "/C_extra/C_rain_events_time.dat")
D.e <- paste0(input_path_RAW, "/D_extra/D_rain_events_time.dat")

A.ev <- fread(A.e)
B.ev <- fread(B.e)
C.ev <- fread(C.e)
D.ev <- fread(D.e)

A.ev[,RainEvent1:= as.POSIXct(RainEvent1, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
A.ev[,RainEvent2:= as.POSIXct(RainEvent2, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
A.ev[,RainEvent3:= as.POSIXct(RainEvent3, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

B.ev[,RainEvent1:= as.POSIXct(RainEvent1, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
B.ev[,RainEvent2:= as.POSIXct(RainEvent2, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
B.ev[,RainEvent3:= as.POSIXct(RainEvent3, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

C.ev[,RainEvent1:= as.POSIXct(RainEvent1, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
C.ev[,RainEvent2:= as.POSIXct(RainEvent2, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
C.ev[,RainEvent3:= as.POSIXct(RainEvent3, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

D.ev[,RainEvent1:= as.POSIXct(RainEvent1, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
D.ev[,RainEvent2:= as.POSIXct(RainEvent2, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
D.ev[,RainEvent3:= as.POSIXct(RainEvent3, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

data <- copy(Aflux)
D.ev[,RainEvent1:= as.POSIXct(RainEvent1, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
# data <- data[,.(days3rdEvent, CORE, fertilizer, precipitation, tillage,
#                 weight, drysoil, drysoil_est, drysoil_dif,  water, addedwater)]
# data[CORE=="QCL_CORE_09",][1:100]
# test <- data[days3rdEvent>=14 & days3rdEvent<14.125,]
# test[,evap:= 1118.55 - addedwater]
# test[,mean(evap), by=.(precipitation)]
# test[,mean(evap), by=.(tillage)]

###
data <- data[,.(days.adjusted, days.adj.full, CORE, fertilizer, precipitation, tillage,
                water, addedwater)]
data[, CORE:= substr(CORE, 10,12)]
data[, CORE:= as.numeric(CORE)]
data[, event:= 0]
data[days.adjusted>=0, event:= 1]
data[days.adjusted>=15, event:= 2]
data[days.adjusted>=30, event:= 3]

data[,span:= days.adj.full - shift(days.adj.full,1), by= CORE]
data[, evap:= addedwater - shift(addedwater,1), by=CORE]
data[evap>0, evap:= NA]
data[, evap.rate:= evap/span]

plot(data[CORE==10, sort(evap.rate)])

for(i in 1:18){
        with(data[CORE==i], plot(evap.rate ~ days.adj.full, main=i))      
}
with(data, plot(evap.rate ~ days.adj.full, ylim=c(-50,0)))

with(data[precipitation=="incr"], plot(evap.rate ~ days.adj.full, ylim=c(-30,0), main = "incr"))
with(data[precipitation=="decr"], plot(evap.rate ~ days.adj.full, ylim=c(-30,0), main = "decr"))
with(data[precipitation=="cons"], plot(evap.rate ~ days.adj.full, ylim=c(-30,0), main = "cons"))

with(data[precipitation=="incr"], plot(evap.rate ~ days.adj.full, ylim=c(-30,0), main = "incr"))
with(data[precipitation=="decr"], points(evap.rate ~ days.adj.full, ylim=c(-30,0), main = "decr", col="red"))
with(data[precipitation=="cons"], points(evap.rate ~ days.adj.full, ylim=c(-30,0), main = "cons", col="blue"))

data[evap.rate < -10, evap.rate:=NA]
data[,.(
        median = median(evap.rate, na.rm=T),
        mad = mad(evap.rate, na.rm=T),
        
        mean = mean(evap.rate, na.rm=T),
        sd = sd(evap.rate, na.rm=T),
        se = sd(evap.rate, na.rm=T)/sqrt
        ), by= precipitation]

data[,.(
        median = median(evap.rate, na.rm=T),
        mad = mad(evap.rate, na.rm=T),
        
        mean = mean(evap.rate, na.rm=T),
        sd = sd(evap.rate, na.rm=T)
), by= .(CORE, fertilizer, precipitation, tillage)]

