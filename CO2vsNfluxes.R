# output files
folder.out <- paste0(output_path, "/CO2vsNflux")
dir.create(folder.out)

NO.folder <- paste0(folder.out, "/CO2vsNO")
dir.create(NO.folder)

N2O.folder <- paste0(folder.out, "/CO2vsN2O")
dir.create(N2O.folder)

NON2O.folder <- paste0(folder.out, "/CO2vs_NOplusN2O")
dir.create(NON2O.folder)

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
treatments <- fread(input = treatments)

##
Aflux <- Aflux[,.(
        incubation= "A",
        days = days.adjusted,
        fertilizer, precipitation, tillage, treatment,
        NO, N2O, CO2, CH4
)]
Aflux[,event:= as.numeric(NA)]
Aflux[days>=0 & days<=14, event:=1]
Aflux[days>=15 & days<=29, event:=2]
Aflux[days>=30 & days<=44, event:=3]
Aflux <- Aflux[!is.na(event)]

##
Bflux <- Bflux[,.(
        incubation= "B",
        days = days.adjusted,
        fertilizer, precipitation, tillage, treatment,
        NO, N2O, CO2, CH4
)]
Bflux[,event:= as.numeric(NA)]
Bflux[days>=0 & days<=14, event:=1]
Bflux[days>=15 & days<=29, event:=2]
Bflux[days>=30 & days<=44, event:=3]
Bflux <- Bflux[!is.na(event)]


##
Cflux <- Cflux[,.(
        incubation= "C",
        days = days.adjusted,
        fertilizer, precipitation, tillage, treatment,
        NO, N2O, CO2, CH4
)]
Cflux[,event:= as.numeric(NA)]
Cflux[days>=0 & days<=14, event:=1]
Cflux[days>=15 & days<=29, event:=2]
Cflux[days>=30 & days<=44, event:=3]
Cflux <- Cflux[!is.na(event)]


##
Dflux <- Dflux[,.(
        incubation= "D",
        days = days.adjusted,
        fertilizer, precipitation, tillage, treatment,
        NO, N2O, CO2, CH4
)]
Dflux[,event:= as.numeric(NA)]
Dflux[days>=0 & days<=14, event:=1]
Dflux[days>=15 & days<=29, event:=2]
Dflux[days>=30 & days<=44, event:=3]
Dflux <- Dflux[!is.na(event)]

flux <- rbindlist(list(Aflux, Bflux, Cflux, Dflux))
setkey(flux, incubation, fertilizer, precipitation, tillage, treatment, days)

# add NO +N2O column
flux[,NON2O:= NO + N2O]
flux[, fertilizer:= factor(fertilizer,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))]
flux[, precipitation:= factor(precipitation,levels = c("cons", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing"))]
flux[, tillage:= factor(tillage, levels = c("NT", "T"), labels=c("no tillage", "traditional tillage"))]
flux[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]

setkey(flux, incubation, fertilizer, precipitation, tillage, treatment, days)
################################################################################
### get ranks for CO2, NO, N2O
################################################################################
flux[, qCO2:= as.numeric(NA)]
flux[, qCO2.event:= as.numeric(NA)]

flux[, qNO:= as.numeric(NA)]
flux[, qNO.event:= as.numeric(NA)]

flux[, qN2O:= as.numeric(NA)]
flux[, qN2O.event:= as.numeric(NA)]

flux[, qNON2O:= as.numeric(NA)]
flux[, qNON2O.event:= as.numeric(NA)]

#######################
## ranks per event
setkey(flux, incubation, treatment, event)

# CO2
f <- flux[,.(
        count = sum(!is.na(CO2)),
        rank = rank(CO2, na.last = "keep")
     ), by= .(incubation, treatment, event)]
f[,myq:= rank/count*100]
flux[,qCO2.event:= f$myq]

# NO
f <- flux[,.(
        count = sum(!is.na(NO)),
        rank = rank(NO, na.last = "keep")
), by= .(incubation, treatment, event)]
f[,myq:= rank/count*100]
flux[,qNO.event:= f$myq]

# N2O
f <- flux[,.(
        count = sum(!is.na(N2O)),
        rank = rank(N2O, na.last = "keep")
), by= .(incubation, treatment, event)]
f[,myq:= rank/count*100]
flux[,qN2O.event:= f$myq]

# NO + N2O
f <- flux[,.(
        count = sum(!is.na(NON2O)),
        rank = rank(NON2O, na.last = "keep")
), by= .(incubation, treatment, event)]
f[,myq:= rank/count*100]
flux[,qNON2O.event:= f$myq]

#######################
## ranks all incubation (3 events together)
setkey(flux, incubation, treatment)

# CO2
f <- flux[,.(
        count = sum(!is.na(CO2)),
        rank = rank(CO2, na.last = "keep")
), by= .(incubation, treatment)]
f[,myq:= rank/count*100]
flux[,qCO2:= f$myq]

# NO
f <- flux[,.(
        count = sum(!is.na(NO)),
        rank = rank(NO, na.last = "keep")
), by= .(incubation, treatment)]
f[,myq:= rank/count*100]
flux[,qNO:= f$myq]

# N2O
f <- flux[,.(
        count = sum(!is.na(N2O)),
        rank = rank(N2O, na.last = "keep")
), by= .(incubation, treatment)]
f[,myq:= rank/count*100]
flux[,qN2O:= f$myq]

# NO + N2O
f <- flux[,.(
        count = sum(!is.na(NON2O)),
        rank = rank(NON2O, na.last = "keep")
), by= .(incubation, treatment)]
f[,myq:= rank/count*100]
flux[,qNON2O:= f$myq]

# mydata <- copy(flux)
# setnames(flux, "fertilizer", "fertilization")
# mydata <- mydata[,.(days, fertilization, precipitation, tillage, incubation, event,
#                     NO, N2O, CO2, CH4, NON2O,
#                     qCO2, qNO, qN2O, qNON2O,
#                     qCO2.event, qNO.event, qN2O.event, qNON2O.event)]
# setkey(mydata, fertilization, precipitation, tillage, incubation, event, days)
# no.format <- names(mydata)[names(mydata) %in% c("incabation", "treatment", "fertilization", "precipitation", "tillage", "event", "days")]
# to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
# mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
# mydata[,days:= formatC(days, format = "f", digits = 6)]
# myfile <- paste0(folder.out, "/instant_flux_quantiles.dat")
# write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

################################################################################
### get summary statistics (median, IQR) for qCO2 intervals
################################################################################
## NO
edge <- 10
estimates <- data.table(qNO.event = seq(from=edge, to= 100 - edge, by=1))
long <- nrow(estimates)
setkey(estimates, qNO.event)

estimates[, CO2.q25 := as.numeric(NA)]
estimates[, CO2.q50 := as.numeric(NA)]
estimates[, CO2.q75 := as.numeric(NA)]

###by event
# tillage vs event
NO.till.ev <- data.table(
        event = rep(c(1:3), each=long*2),
        tillage = rep(c("NT", "T"), each = long, times = 3),
        qNO.event = rep(estimates$qNO.event, times = 3*2)
)
setkey(NO.till.ev, qNO.event)
NO.till.ev <- NO.till.ev[estimates]
NO.till.ev[, tillage:= factor(tillage, levels = c("NT", "T"), labels=c("no tillage", "traditional tillage"))]
NO.till.ev[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]
setkey(NO.till.ev,event, tillage)
setkey(flux,event, tillage)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNO.event){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNO.event > qmin & qNO.event< qmax,.(
                CO2.q25 = quantile(qCO2.event,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2.event,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2.event,0.75, na.rm=T)
        ), by=.(event, tillage)]
        NO.till.ev[qNO.event == i,(calculation):= mydata[,calculation, with=F]]
}

# rain vs event
NO.rain.ev <- data.table(
        event = rep(c(1:3), each=long*3),
        precipitation = rep(c("cons", "incr", "decr"), each = long, times = 3),
        qNO.event = rep(estimates$qNO.event, times = 3*3)
)
setkey(NO.rain.ev, qNO.event)
NO.rain.ev <- NO.rain.ev[estimates]
NO.rain.ev[, precipitation:= factor(precipitation,levels = c("cons", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing"))]
NO.rain.ev[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]
setkey(NO.rain.ev,event, precipitation)
setkey(flux,event, precipitation)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNO.event){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNO.event > qmin & qNO.event< qmax,.(
                CO2.q25 = quantile(qCO2.event,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2.event,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2.event,0.75, na.rm=T)
        ), by=.(event, precipitation)]
        NO.rain.ev[qNO.event == i,(calculation):= mydata[,calculation, with=F]]
}

# fert vs event
NO.fert.ev <- data.table(
        event = rep(c(1:3), each=long*3),
        fertilizer = rep(c(0, 50, 100), each = long, times = 3),
        qNO.event = rep(estimates$qNO.event, times = 3*3)
)
setkey(NO.fert.ev, qNO.event)
NO.fert.ev <- NO.fert.ev[estimates]
NO.fert.ev[, fertilizer:= factor(fertilizer,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))]
NO.fert.ev[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]
setkey(NO.fert.ev,event, fertilizer)
setkey(flux,event, fertilizer)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNO.event){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNO.event > qmin & qNO.event< qmax,.(
                CO2.q25 = quantile(qCO2.event,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2.event,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2.event,0.75, na.rm=T)
        ), by=.(event, fertilizer)]
        NO.fert.ev[qNO.event == i,(calculation):= mydata[,calculation, with=F]]
}

### whole incubation (all 3 events together)
# tillage
NO.till <- data.table(
        tillage = rep(c("NT", "T"), each = long),
        qNO = rep(estimates$qNO, times = 2)
)
setkey(NO.till, qNO)
NO.till <- NO.till[estimates]
NO.till[, tillage:= factor(tillage, levels = c("NT", "T"), labels=c("no tillage", "traditional tillage"))]
setkey(NO.till, tillage)
setkey(flux, tillage)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNO){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNO > qmin & qNO< qmax,.(
                CO2.q25 = quantile(qCO2,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2,0.75, na.rm=T)
        ), by=.( tillage)]
        NO.till[qNO == i,(calculation):= mydata[,calculation, with=F]]
}

# rain
NO.rain <- data.table(
        precipitation = rep(c("cons", "incr", "decr"), each = long),
        qNO = rep(estimates$qNO, times = 3)
)
setkey(NO.rain, qNO)
NO.rain <- NO.rain[estimates]
NO.rain[, precipitation:= factor(precipitation,levels = c("cons", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing"))]
setkey(NO.rain, precipitation)
setkey(flux, precipitation)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNO){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNO > qmin & qNO< qmax,.(
                CO2.q25 = quantile(qCO2,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2,0.75, na.rm=T)
        ), by=.( precipitation)]
        NO.rain[qNO == i,(calculation):= mydata[,calculation, with=F]]
}

# fert
NO.fert <- data.table(
        fertilizer = rep(c(0, 50, 100), each = long),
        qNO = rep(estimates$qNO, times = 3)
)
setkey(NO.fert, qNO)
NO.fert <- NO.fert[estimates]
NO.fert[, fertilizer:= factor(fertilizer,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))]
setkey(NO.fert, fertilizer)
setkey(flux, fertilizer)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNO){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNO > qmin & qNO< qmax,.(
                CO2.q25 = quantile(qCO2,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2,0.75, na.rm=T)
        ), by=.( fertilizer)]
        NO.fert[qNO == i,(calculation):= mydata[,calculation, with=F]]
}

################################################################################
## N2O
edge <- 10
estimates <- data.table(qN2O.event = seq(from=edge, to= 100 - edge, by=1))
long <- nrow(estimates)
setkey(estimates, qN2O.event)

estimates[, CO2.q25 := as.numeric(NA)]
estimates[, CO2.q50 := as.numeric(NA)]
estimates[, CO2.q75 := as.numeric(NA)]

###by event
# tillage vs event
N2O.till.ev <- data.table(
        event = rep(c(1:3), each=long*2),
        tillage = rep(c("NT", "T"), each = long, times = 3),
        qN2O.event = rep(estimates$qN2O.event, times = 3*2)
)
setkey(N2O.till.ev, qN2O.event)
N2O.till.ev <- N2O.till.ev[estimates]
N2O.till.ev[, tillage:= factor(tillage, levels = c("NT", "T"), labels=c("no tillage", "traditional tillage"))]
N2O.till.ev[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]
setkey(N2O.till.ev,event, tillage)
setkey(flux,event, tillage)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qN2O.event){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qN2O.event > qmin & qN2O.event< qmax,.(
                CO2.q25 = quantile(qCO2.event,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2.event,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2.event,0.75, na.rm=T)
        ), by=.(event, tillage)]
        N2O.till.ev[qN2O.event == i,(calculation):= mydata[,calculation, with=F]]
}

# rain vs event
N2O.rain.ev <- data.table(
        event = rep(c(1:3), each=long*3),
        precipitation = rep(c("cons", "incr", "decr"), each = long, times = 3),
        qN2O.event = rep(estimates$qN2O.event, times = 3*3)
)
setkey(N2O.rain.ev, qN2O.event)
N2O.rain.ev <- N2O.rain.ev[estimates]
N2O.rain.ev[, precipitation:= factor(precipitation,levels = c("cons", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing"))]
N2O.rain.ev[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]
setkey(N2O.rain.ev,event, precipitation)
setkey(flux,event, precipitation)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qN2O.event){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qN2O.event > qmin & qN2O.event< qmax,.(
                CO2.q25 = quantile(qCO2.event,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2.event,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2.event,0.75, na.rm=T)
        ), by=.(event, precipitation)]
        N2O.rain.ev[qN2O.event == i,(calculation):= mydata[,calculation, with=F]]
}

# fert vs event
N2O.fert.ev <- data.table(
        event = rep(c(1:3), each=long*3),
        fertilizer = rep(c(0, 50, 100), each = long, times = 3),
        qN2O.event = rep(estimates$qN2O.event, times = 3*3)
)
setkey(N2O.fert.ev, qN2O.event)
N2O.fert.ev <- N2O.fert.ev[estimates]
N2O.fert.ev[, fertilizer:= factor(fertilizer,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))]
N2O.fert.ev[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]
setkey(N2O.fert.ev,event, fertilizer)
setkey(flux,event, fertilizer)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qN2O.event){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qN2O.event > qmin & qN2O.event< qmax,.(
                CO2.q25 = quantile(qCO2.event,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2.event,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2.event,0.75, na.rm=T)
        ), by=.(event, fertilizer)]
        N2O.fert.ev[qN2O.event == i,(calculation):= mydata[,calculation, with=F]]
}

### whole incubation (all 3 events together)
# tillage
N2O.till <- data.table(
        tillage = rep(c("NT", "T"), each = long),
        qN2O = rep(estimates$qN2O, times = 2)
)
setkey(N2O.till, qN2O)
N2O.till <- N2O.till[estimates]
N2O.till[, tillage:= factor(tillage, levels = c("NT", "T"), labels=c("no tillage", "traditional tillage"))]
setkey(N2O.till, tillage)
setkey(flux, tillage)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qN2O){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qN2O > qmin & qN2O< qmax,.(
                CO2.q25 = quantile(qCO2,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2,0.75, na.rm=T)
        ), by=.( tillage)]
        N2O.till[qN2O == i,(calculation):= mydata[,calculation, with=F]]
}

# rain
N2O.rain <- data.table(
        precipitation = rep(c("cons", "incr", "decr"), each = long),
        qN2O = rep(estimates$qN2O, times = 3)
)
setkey(N2O.rain, qN2O)
N2O.rain <- N2O.rain[estimates]
N2O.rain[, precipitation:= factor(precipitation,levels = c("cons", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing"))]
setkey(N2O.rain, precipitation)
setkey(flux, precipitation)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qN2O){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qN2O > qmin & qN2O< qmax,.(
                CO2.q25 = quantile(qCO2,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2,0.75, na.rm=T)
        ), by=.( precipitation)]
        N2O.rain[qN2O == i,(calculation):= mydata[,calculation, with=F]]
}

# fert
N2O.fert <- data.table(
        fertilizer = rep(c(0, 50, 100), each = long),
        qN2O = rep(estimates$qN2O, times = 3)
)
setkey(N2O.fert, qN2O)
N2O.fert <- N2O.fert[estimates]
N2O.fert[, fertilizer:= factor(fertilizer,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))]
setkey(N2O.fert, fertilizer)
setkey(flux, fertilizer)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qN2O){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qN2O > qmin & qN2O< qmax,.(
                CO2.q25 = quantile(qCO2,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2,0.75, na.rm=T)
        ), by=.( fertilizer)]
        N2O.fert[qN2O == i,(calculation):= mydata[,calculation, with=F]]
}

################################################################################
## NON2O
edge <- 10
estimates <- data.table(qNON2O.event = seq(from=edge, to= 100 - edge, by=1))
long <- nrow(estimates)
setkey(estimates, qNON2O.event)

estimates[, CO2.q25 := as.numeric(NA)]
estimates[, CO2.q50 := as.numeric(NA)]
estimates[, CO2.q75 := as.numeric(NA)]

###by event
# tillage vs event
NON2O.till.ev <- data.table(
        event = rep(c(1:3), each=long*2),
        tillage = rep(c("NT", "T"), each = long, times = 3),
        qNON2O.event = rep(estimates$qNON2O.event, times = 3*2)
)
setkey(NON2O.till.ev, qNON2O.event)
NON2O.till.ev <- NON2O.till.ev[estimates]
NON2O.till.ev[, tillage:= factor(tillage, levels = c("NT", "T"), labels=c("no tillage", "traditional tillage"))]
NON2O.till.ev[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]
setkey(NON2O.till.ev,event, tillage)
setkey(flux,event, tillage)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNON2O.event){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNON2O.event > qmin & qNON2O.event< qmax,.(
                CO2.q25 = quantile(qCO2.event,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2.event,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2.event,0.75, na.rm=T)
        ), by=.(event, tillage)]
        NON2O.till.ev[qNON2O.event == i,(calculation):= mydata[,calculation, with=F]]
}

# rain vs event
NON2O.rain.ev <- data.table(
        event = rep(c(1:3), each=long*3),
        precipitation = rep(c("cons", "incr", "decr"), each = long, times = 3),
        qNON2O.event = rep(estimates$qNON2O.event, times = 3*3)
)
setkey(NON2O.rain.ev, qNON2O.event)
NON2O.rain.ev <- NON2O.rain.ev[estimates]
NON2O.rain.ev[, precipitation:= factor(precipitation,levels = c("cons", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing"))]
NON2O.rain.ev[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]
setkey(NON2O.rain.ev,event, precipitation)
setkey(flux,event, precipitation)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNON2O.event){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNON2O.event > qmin & qNON2O.event< qmax,.(
                CO2.q25 = quantile(qCO2.event,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2.event,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2.event,0.75, na.rm=T)
        ), by=.(event, precipitation)]
        NON2O.rain.ev[qNON2O.event == i,(calculation):= mydata[,calculation, with=F]]
}

# fert vs event
NON2O.fert.ev <- data.table(
        event = rep(c(1:3), each=long*3),
        fertilizer = rep(c(0, 50, 100), each = long, times = 3),
        qNON2O.event = rep(estimates$qNON2O.event, times = 3*3)
)
setkey(NON2O.fert.ev, qNON2O.event)
NON2O.fert.ev <- NON2O.fert.ev[estimates]
NON2O.fert.ev[, fertilizer:= factor(fertilizer,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))]
NON2O.fert.ev[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]
setkey(NON2O.fert.ev,event, fertilizer)
setkey(flux,event, fertilizer)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNON2O.event){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNON2O.event > qmin & qNON2O.event< qmax,.(
                CO2.q25 = quantile(qCO2.event,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2.event,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2.event,0.75, na.rm=T)
        ), by=.(event, fertilizer)]
        NON2O.fert.ev[qNON2O.event == i,(calculation):= mydata[,calculation, with=F]]
}

### whole incubation (all 3 events together)
# tillage
NON2O.till <- data.table(
        tillage = rep(c("NT", "T"), each = long),
        qNON2O = rep(estimates$qNON2O, times = 2)
)
setkey(NON2O.till, qNON2O)
NON2O.till <- NON2O.till[estimates]
NON2O.till[, tillage:= factor(tillage, levels = c("NT", "T"), labels=c("no tillage", "traditional tillage"))]
setkey(NON2O.till, tillage)
setkey(flux, tillage)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNON2O){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNON2O > qmin & qNON2O< qmax,.(
                CO2.q25 = quantile(qCO2,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2,0.75, na.rm=T)
        ), by=.( tillage)]
        NON2O.till[qNON2O == i,(calculation):= mydata[,calculation, with=F]]
}

# rain
NON2O.rain <- data.table(
        precipitation = rep(c("cons", "incr", "decr"), each = long),
        qNON2O = rep(estimates$qNON2O, times = 3)
)
setkey(NON2O.rain, qNON2O)
NON2O.rain <- NON2O.rain[estimates]
NON2O.rain[, precipitation:= factor(precipitation,levels = c("cons", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing"))]
setkey(NON2O.rain, precipitation)
setkey(flux, precipitation)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNON2O){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNON2O > qmin & qNON2O< qmax,.(
                CO2.q25 = quantile(qCO2,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2,0.75, na.rm=T)
        ), by=.( precipitation)]
        NON2O.rain[qNON2O == i,(calculation):= mydata[,calculation, with=F]]
}

# fert
NON2O.fert <- data.table(
        fertilizer = rep(c(0, 50, 100), each = long),
        qNON2O = rep(estimates$qNON2O, times = 3)
)
setkey(NON2O.fert, qNON2O)
NON2O.fert <- NON2O.fert[estimates]
NON2O.fert[, fertilizer:= factor(fertilizer,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))]
setkey(NON2O.fert, fertilizer)
setkey(flux, fertilizer)

calculation <- c("CO2.q25", "CO2.q50", "CO2.q75")
for(i in estimates$qNON2O){
        qmin <- i-10
        qmax <- i+10
        mydata <- flux[qNON2O > qmin & qNON2O< qmax,.(
                CO2.q25 = quantile(qCO2,0.25, na.rm=T),
                CO2.q50 = quantile(qCO2,0.5, na.rm=T),
                CO2.q75 = quantile(qCO2,0.75, na.rm=T)
        ), by=.( fertilizer)]
        NON2O.fert[qNON2O == i,(calculation):= mydata[,calculation, with=F]]
}
################################################################################
#### slope and r2 (not implemented; see rain.event No plot-geom_smooth())

# fert vs event
r2.fert.ev <- flux[,.(
        slope = lm(qCO2.event ~ qNO.event)[[1]][[2]],
        r2 = summary( lm(qCO2.event ~ qNO.event) )$r.squared
), by= .(fertilizer, event)]
r2.fert.ev[, slope:= formatC(slope, digits=2, format = "f")]
r2.fert.ev[, r2:= formatC(r2, digits=2, format = "f")]

# rain vs event
r2.rain.ev <- flux[,.(
        slope = lm(qCO2.event ~ qNO.event)[[1]][[2]],
        r2 = summary( lm(qCO2.event ~ qNO.event) )$r.squared
), by= .(precipitation, event)]
r2.rain.ev[, slope:= formatC(slope, digits=2, format = "f")]
r2.rain.ev[, r2:= formatC(r2, digits=2, format = "f")]

# tillage vs event
r2.till.ev <- flux[,.(
        slope = lm(qCO2.event ~ qNO.event)[[1]][[2]],
        r2 = summary( lm(qCO2.event ~ qNO.event) )$r.squared
), by= .(tillage, event)]
r2.till.ev[, slope:= formatC(slope, digits=2, format = "f")]
r2.till.ev[, r2:= formatC(r2, digits=2, format = "f")]

###
# fert
r2.fert <- flux[,.(
        slope = lm(qCO2 ~ qNO)[[1]][[2]],
        r2 = summary( lm(qCO2 ~ qNO) )$r.squared
), by= .(fertilizer)]
r2.fert[, slope:= formatC(slope, digits=2, format = "f")]
r2.fert[, r2:= formatC(r2, digits=2, format = "f")]

# rain
r2.rain <- flux[,.(
        slope = lm(qCO2 ~ qNO)[[1]][[2]],
        r2 = summary( lm(qCO2 ~ qNO) )$r.squared
), by= .(precipitation)]
r2.rain[, slope:= formatC(slope, digits=2, format = "f")]
r2.rain[, r2:= formatC(r2, digits=2, format = "f")]

# tillage
r2.till <- flux[,.(
        slope = lm(qCO2 ~ qNO)[[1]][[2]],
        r2 = summary( lm(qCO2 ~ qNO) )$r.squared
), by= .(tillage)]
r2.till[, slope:= formatC(slope, digits=2, format = "f")]
r2.till[, r2:= formatC(r2, digits=2, format = "f")]

################################################################################
### plot
### NO
########################
## by event
############
# fertilizer
g.1 <- ggplot(flux, aes(x= qNO.event, y = qCO2.event))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab("NO quantile [%]")
        # + ylab(expression('NO [ µg-N/m'^2*'/h ]'))
        # + ylab(expression('NO'[2] * ' / dt     [ppm / min]'))
        #         + xlab("")
        #         + labs(title = "(a)")
        #         + coord_cartesian(xlim= c(0, 100))
        # + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + facet_grid(fertilizer ~ event)
        + geom_ribbon(data=NO.fert.ev, aes(x = qNO.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NO.fert.ev, aes(x = qNO.event, y = CO2.q50), size=0.5, col="red")
#         + geom_ribbon(data=myNO, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
#         + geom_line(data=myNO, aes(x = days, y = mymean), size=0.5, col="red")
)


myplot <- paste0(NO.folder, "/qCO2_vs_qNO_event_fert.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# rain
g.1 <- ggplot(flux, aes(x= qNO.event, y = qCO2.event))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab("NO quantile [%]")
)

g.1 <- (g.1
        + facet_grid(precipitation ~ event)
        + geom_ribbon(data=NO.rain.ev, aes(x = qNO.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NO.rain.ev, aes(x = qNO.event, y = CO2.q50), size=0.5, col="red")
        # + geom_smooth(method='lm',formula=y~x, se=FALSE, col="black", size=0.5, linetype=2)
)

myplot <- paste0(NO.folder, "/qCO2_vs_qNO_event_rain.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# tillage
g.1 <- ggplot(flux, aes(x= qNO.event, y = qCO2.event))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab("NO quantile [%]")
)

g.1 <- (g.1
        + facet_grid(tillage ~ event)
        + geom_ribbon(data=NO.till.ev, aes(x = qNO.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NO.till.ev, aes(x = qNO.event, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(NO.folder, "/qCO2_vs_qNO_event_tillage.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

########################
## all incubation (3 events together)
############
# fertilizer
g.1 <- ggplot(flux, aes(x= qNO, y = qCO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                # strip.text.x = element_blank(),
                legend.position =   "none",
                strip.background =   element_blank(),
                # strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab("NO quantile [%]")
        # + ylab(expression('NO [ µg-N/m'^2*'/h ]'))
        # + ylab(expression('NO'[2] * ' / dt     [ppm / min]'))
        #         + xlab("")
        #         + labs(title = "(a)")
        #         + coord_cartesian(xlim= c(0, 100))
        # + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + facet_grid(~fertilizer)
        + geom_ribbon(data=NO.fert, aes(x = qNO, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NO.fert, aes(x = qNO, y = CO2.q50), size=0.5, col="red")
        #         + geom_ribbon(data=myNO, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        #         + geom_line(data=myNO, aes(x = days, y = mymean), size=0.5, col="red")
)


myplot <- paste0(NO.folder, "/qCO2_vs_qNO__fert.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# rain
g.1 <- ggplot(flux, aes(x= qNO, y = qCO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab("NO quantile [%]")
)

g.1 <- (g.1
        + facet_grid(~precipitation)
        + geom_ribbon(data=NO.rain, aes(x = qNO, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NO.rain, aes(x = qNO, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(NO.folder, "/qCO2_vs_qNO__rain.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# tillage
g.1 <- ggplot(flux, aes(x= qNO, y = qCO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab("NO quantile [%]")
)

g.1 <- (g.1
        + facet_grid(~tillage)
        + geom_ribbon(data=NO.till, aes(x = qNO, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NO.till, aes(x = qNO, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(NO.folder, "/qCO2_vs_qNO__tillage.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

################################################################################
### N2O
########################
## by event
############
# fertilizer
g.1 <- ggplot(flux, aes(x= qN2O.event, y = qCO2.event))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('N'[2]*'O quantile [%]'))
        # + ylab(expression('N2O [ µg-N/m'^2*'/h ]'))
        # + ylab(expression('N2O'[2] * ' / dt     [ppm / min]'))
        #         + xlab("")
        #         + labs(title = "(a)")
        #         + coord_cartesian(xlim= c(0, 100))
        # + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + facet_grid(fertilizer ~ event)
        + geom_ribbon(data=N2O.fert.ev, aes(x = qN2O.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=N2O.fert.ev, aes(x = qN2O.event, y = CO2.q50), size=0.5, col="red")
        #         + geom_ribbon(data=myN2O, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        #         + geom_line(data=myN2O, aes(x = days, y = mymean), size=0.5, col="red")
)


myplot <- paste0(N2O.folder, "/qCO2_vs_qN2O_event_fert.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# rain
g.1 <- ggplot(flux, aes(x= qN2O.event, y = qCO2.event))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('N'[2]*'O quantile [%]'))
)

g.1 <- (g.1
        + facet_grid(precipitation ~ event)
        + geom_ribbon(data=N2O.rain.ev, aes(x = qN2O.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=N2O.rain.ev, aes(x = qN2O.event, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(N2O.folder, "/qCO2_vs_qN2O_event_rain.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# tillage
g.1 <- ggplot(flux, aes(x= qN2O.event, y = qCO2.event))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('N'[2]*'O quantile [%]'))
)

g.1 <- (g.1
        + facet_grid(tillage ~ event)
        + geom_ribbon(data=N2O.till.ev, aes(x = qN2O.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=N2O.till.ev, aes(x = qN2O.event, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(N2O.folder, "/qCO2_vs_qN2O_event_tillage.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

########################
## all incubation (3 events together)
############
# fertilizer
g.1 <- ggplot(flux, aes(x= qN2O, y = qCO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                # strip.text.x = element_blank(),
                legend.position =   "none",
                strip.background =   element_blank(),
                # strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('N'[2]*'O quantile [%]'))
        # + ylab(expression('N2O [ µg-N/m'^2*'/h ]'))
        # + ylab(expression('N2O'[2] * ' / dt     [ppm / min]'))
        #         + xlab("")
        #         + labs(title = "(a)")
        #         + coord_cartesian(xlim= c(0, 100))
        # + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + facet_grid(~fertilizer)
        + geom_ribbon(data=N2O.fert, aes(x = qN2O, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=N2O.fert, aes(x = qN2O, y = CO2.q50), size=0.5, col="red")
        #         + geom_ribbon(data=myN2O, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        #         + geom_line(data=myN2O, aes(x = days, y = mymean), size=0.5, col="red")
)


myplot <- paste0(N2O.folder, "/qCO2_vs_qN2O__fert.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# rain
g.1 <- ggplot(flux, aes(x= qN2O, y = qCO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('N'[2]*'O quantile [%]'))
)

g.1 <- (g.1
        + facet_grid(~precipitation)
        + geom_ribbon(data=N2O.rain, aes(x = qN2O, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=N2O.rain, aes(x = qN2O, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(N2O.folder, "/qCO2_vs_qN2O__rain.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# tillage
g.1 <- ggplot(flux, aes(x= qN2O, y = qCO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('N'[2]*'O quantile [%]'))
)

g.1 <- (g.1
        + facet_grid(~tillage)
        + geom_ribbon(data=N2O.till, aes(x = qN2O, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=N2O.till, aes(x = qN2O, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(N2O.folder, "/qCO2_vs_qN2O__tillage.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

################################################################################
### NO + N2O
########################
## by event
############
# fertilizer
g.1 <- ggplot(flux, aes(x= qNON2O.event, y = qCO2.event))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('NO + N'[2]*'O quantile [%]'))
)

g.1 <- (g.1
        + facet_grid(fertilizer ~ event)
        + geom_ribbon(data=NON2O.fert.ev, aes(x = qNON2O.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NON2O.fert.ev, aes(x = qNON2O.event, y = CO2.q50), size=0.5, col="red")
)


myplot <- paste0(NON2O.folder, "/qCO2_vs_qNOplusN2O_event_fert.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# rain
g.1 <- ggplot(flux, aes(x= qNON2O.event, y = qCO2.event))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('NO + N'[2]*'O quantile [%]'))
)

g.1 <- (g.1
        + facet_grid(precipitation ~ event)
        + geom_ribbon(data=NON2O.rain.ev, aes(x = qNON2O.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NON2O.rain.ev, aes(x = qNON2O.event, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(NON2O.folder, "/qCO2_vs_qNOplusN2O_event_rain.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# tillage
g.1 <- ggplot(flux, aes(x= qNON2O.event, y = qCO2.event))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('NO + N'[2]*'O quantile [%]'))
)

g.1 <- (g.1
        + facet_grid(tillage ~ event)
        + geom_ribbon(data=NON2O.till.ev, aes(x = qNON2O.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NON2O.till.ev, aes(x = qNON2O.event, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(NON2O.folder, "/qCO2_vs_qNOplusN2O_event_tillage.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

########################
## all incubation (3 events together)
############
# fertilizer
g.1 <- ggplot(flux, aes(x= qNON2O, y = qCO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                # strip.text.x = element_blank(),
                legend.position =   "none",
                strip.background =   element_blank(),
                # strip.text.x =       element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('NO + N'[2]*'O quantile [%]'))
        # + ylab(expression('NON2O [ µg-N/m'^2*'/h ]'))
        # + ylab(expression('NON2O'[2] * ' / dt     [ppm / min]'))
        #         + xlab("")
        #         + labs(title = "(a)")
        #         + coord_cartesian(xlim= c(0, 100))
        # + scale_x_continuous(breaks = c(0, 10, 20, 30, 40))
        #       + ggtitle(paste0("Flow change during sampling: \n influence on outlet air concentration"))
)

g.1 <- (g.1
        + facet_grid(~fertilizer)
        + geom_ribbon(data=NON2O.fert, aes(x = qNON2O, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NON2O.fert, aes(x = qNON2O, y = CO2.q50), size=0.5, col="red")
        #         + geom_ribbon(data=myNON2O, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        #         + geom_line(data=myNON2O, aes(x = days, y = mymean), size=0.5, col="red")
)


myplot <- paste0(NON2O.folder, "/qCO2_vs_qNOplusN2O__fert.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# rain
g.1 <- ggplot(flux, aes(x= qNON2O, y = qCO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('NO + N'[2]*'O quantile [%]'))
)

g.1 <- (g.1
        + facet_grid(~precipitation)
        + geom_ribbon(data=NON2O.rain, aes(x = qNON2O, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NON2O.rain, aes(x = qNON2O, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(NON2O.folder, "/qCO2_vs_qNOplusN2O__rain.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# tillage
g.1 <- ggplot(flux, aes(x= qNON2O, y = qCO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' quantile [%]'))
        + xlab(expression('NO + N'[2]*'O quantile [%]'))
)

g.1 <- (g.1
        + facet_grid(~tillage)
        + geom_ribbon(data=NON2O.till, aes(x = qNON2O, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        + geom_line(data=NON2O.till, aes(x = qNON2O, y = CO2.q50), size=0.5, col="red")
)

myplot <- paste0(NON2O.folder, "/qCO2_vs_qNOplusN2O__tillage.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()









# axis inverted
# ## NO
# edge <- 10
# estimates <- data.table(qCO2 = seq(from=edge, to= 100 - edge, by=1))
# long <- nrow(estimates)
# setkey(estimates, qCO2)
# 
# estimates[, NO.q25 := as.numeric(NA)]
# estimates[, NO.q50 := as.numeric(NA)]
# estimates[, NO.q75 := as.numeric(NA)]
# 
# estimates[, N2O.q25 := as.numeric(NA)]
# estimates[, N2O.q50 := as.numeric(NA)]
# estimates[, N2O.q75 := as.numeric(NA)]
# 
# # tillage vs event
# NO.till.ev <- data.table(
#         event = rep(c(1:3), each=long*2),
#         tillage = rep(c("NT", "T"), each = long, times = 3),
#         qCO2 = rep(estimates$qCO2, times = 3*2)
# )
# setkey(NO.till.ev, qCO2)
# NO.till.ev <- NO.till.ev[estimates]
# setkey(NO.till.ev,event, tillage)
# setkey(flux,event, tillage)
# 
# calculation <- c("NO.q25", "NO.q50", "NO.q75",
#                  "N2O.q25", "N2O.q50", "N2O.q75")
# for(i in estimates$qCO2){
#         qmin <- i-10
#         qmax <- i+10
#         mydata <- flux[qCO2 > qmin & qCO2< qmax,.(
#                 NO.q25 = quantile(qNO.event,0.25, na.rm=T),
#                 NO.q50 = quantile(qNO.event,0.5, na.rm=T),
#                 NO.q75 = quantile(qNO.event,0.75, na.rm=T),
#                 N2O.q25 = quantile(qN2O.event,0.25, na.rm=T),
#                 N2O.q50 = quantile(qN2O.event,0.5, na.rm=T),
#                 N2O.q75 = quantile(qN2O.event,0.75, na.rm=T)
#         ), by=.(event, tillage)]
#         NO.till.ev[qCO2 == i,(calculation):= mydata[,calculation, with=F]]
# }
# 
# ############
# # tillage
# g.1 <- ggplot(flux, aes(x= qCO2.event, y = qNO.event))
# g.1 <- (g.1
#         + theme_bw(base_size = 8)
#         + theme(axis.ticks = element_line(size = 0.1),
#                 legend.position =   "none",
#                 strip.background =   element_blank(),
#                 plot.title = element_text(hjust = 0)
#         )
#         + xlab("CO2 quantile [%]")
#         + ylab("NO quantile [%]")
# )
# 
# g.1 <- (g.1
#         + facet_grid(tillage ~ event)
#         + geom_ribbon(data=NO.till.ev, aes(x = qCO2, y = NO.q50, ymin=NO.q25, ymax=NO.q75, fill= "red"), alpha=0.5)
#         + geom_point(size=0.5, colour="black", alpha= 0.6)
#         + geom_line(data=NO.till.ev, aes(x = qCO2, y = NO.q50), size=0.5, col="red")
# )
# 
# myplot <- paste0(NO.folder, "/_qCO2_vs_qNO_event_tillage.png")
# png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
# print(g.1)
# dev.off()