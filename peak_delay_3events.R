folder.out <-paste0(output_path, "/cum_fluxes_14/peak_delay_by_event")
dir.create(folder.out)

myfolder <- paste0(output_path, "/cum_fluxes_full")
myfile <- paste0(myfolder, "/every_event_cum_fluxes.dat")
data <- fread(myfile)


################################################################################
### A incubation
cum.A <- copy(data)
cum.A[, NO:= aNO]
cum.A[, N2O:= aN2O]
cum.A[, CO2:= aCO2]
cum.A[, CH4:= aCH4]
# cum.A[, todelete:=NULL, with=FALSE]
cum.A[, incubation:= "A"]

### B incubation
cum.B <- copy(data)
cum.B[, NO:= bNO]
cum.B[, N2O:= bN2O]
cum.B[, CO2:= bCO2]
cum.B[, CH4:= bCH4]
# cum.B[, todelete:=NULL, with=FALSE]
cum.B[, incubation:= "B"]
# cum.B[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### C incubation
cum.C <- copy(data)
cum.C[, NO:= cNO]
cum.C[, N2O:= cN2O]
cum.C[, CO2:= cCO2]
cum.C[, CH4:= cCH4]
# cum.C[, todelete:=NULL, with=FALSE]
cum.C[, incubation:= "C"]
# cum.C[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### D incubation
cum.D <- copy(data)
cum.D[, NO:= dNO]
cum.D[, N2O:= dN2O]
cum.D[, CO2:= dCO2]
cum.D[, CH4:= dCH4]
# cum.D[, todelete:=NULL, with=FALSE]
cum.D[, incubation:= "D"]
# cum.D[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### all incubation binding
data <- rbind(cum.A, cum.B, cum.C, cum.D)
data[,event:= as.numeric(NA)]
data[days>=0 & days<15, event:=1]
data[days>=15 & days<30, event:=2]
data[days>=30 & days<45, event:=3]

data[days>=15 & days<30, days:=days -15]
data[days>=30 & days<45, days:=days -30]


data <- data[,.(event, days, incubation, treatment, fertilizer, precipitation, tillage,
                NO, N2O, CO2, CH4)]
setnames(data, "fertilizer", "fertilization")
################################################################################

data[,last.NO:= tail(NO,1), by= .(event, incubation, fertilization, precipitation, tillage)]
data[,last.N2O:= tail(N2O,1), by= .(event, incubation, fertilization, precipitation, tillage)]
data[,last.CO2:= tail(CO2,1), by= .(event, incubation, fertilization, precipitation, tillage)]

# data[NO> (last.NO/2), .(days = head(days,1)), by= .(incubation, fertilization, precipitation, tillage)]



# peak delay summaries
setkey(data, fertilization, precipitation, tillage, incubation, event)

peak.delay <- data[, .(
        NO.delay = approx(x=NO, y=days, xout=last.NO[1]/2)$y,
        N2O.delay = approx(x=N2O, y=days, xout=last.N2O[1]/2)$y,
        CO2.delay = approx(x=CO2, y=days, xout=last.CO2[1]/2)$y
), by= .(incubation, fertilization, precipitation, tillage, event)]

delay.treat <- peak.delay[, .(
        NO.delay = mean(NO.delay, na.rm=T),
        sd.NO.delay = sd(NO.delay, na.rm=T),
        se.NO.delay = sd(NO.delay, na.rm=T)/sqrt(sum(!is.na(NO.delay))),
        
        N2O.delay = mean(N2O.delay, na.rm=T),
        sd.N2O.delay = sd(N2O.delay, na.rm=T),
        se.N2O.delay = sd(N2O.delay, na.rm=T)/sqrt(sum(!is.na(N2O.delay))),
        
        CO2.delay = mean(CO2.delay, na.rm=T),
        sd.CO2.delay = sd(CO2.delay, na.rm=T),
        se.CO2.delay = sd(CO2.delay, na.rm=T)/sqrt(sum(!is.na(CO2.delay)))
), by= .(fertilization, precipitation, tillage, event)]

delay.till <- peak.delay[, .(
        NO.delay = mean(NO.delay, na.rm=T),
        sd.NO.delay = sd(NO.delay, na.rm=T),
        se.NO.delay = sd(NO.delay, na.rm=T)/sqrt(sum(!is.na(NO.delay))),
        
        N2O.delay = mean(N2O.delay, na.rm=T),
        sd.N2O.delay = sd(N2O.delay, na.rm=T),
        se.N2O.delay = sd(N2O.delay, na.rm=T)/sqrt(sum(!is.na(N2O.delay))),
        
        CO2.delay = mean(CO2.delay, na.rm=T),
        sd.CO2.delay = sd(CO2.delay, na.rm=T),
        se.CO2.delay = sd(CO2.delay, na.rm=T)/sqrt(sum(!is.na(CO2.delay)))
), by= .(tillage, event)]

delay.rain <- peak.delay[, .(
        NO.delay = mean(NO.delay, na.rm=T),
        sd.NO.delay = sd(NO.delay, na.rm=T),
        se.NO.delay = sd(NO.delay, na.rm=T)/sqrt(sum(!is.na(NO.delay))),
        
        N2O.delay = mean(N2O.delay, na.rm=T),
        sd.N2O.delay = sd(N2O.delay, na.rm=T),
        se.N2O.delay = sd(N2O.delay, na.rm=T)/sqrt(sum(!is.na(N2O.delay))),
        
        CO2.delay = mean(CO2.delay, na.rm=T),
        sd.CO2.delay = sd(CO2.delay, na.rm=T),
        se.CO2.delay = sd(CO2.delay, na.rm=T)/sqrt(sum(!is.na(CO2.delay)))
), by= .(precipitation, event)]

delay.fert <- peak.delay[, .(
        NO.delay = mean(NO.delay, na.rm=T),
        sd.NO.delay = sd(NO.delay, na.rm=T),
        se.NO.delay = sd(NO.delay, na.rm=T)/sqrt(sum(!is.na(NO.delay))),
        
        N2O.delay = mean(N2O.delay, na.rm=T),
        sd.N2O.delay = sd(N2O.delay, na.rm=T),
        se.N2O.delay = sd(N2O.delay, na.rm=T)/sqrt(sum(!is.na(N2O.delay))),
        
        CO2.delay = mean(CO2.delay, na.rm=T),
        sd.CO2.delay = sd(CO2.delay, na.rm=T),
        se.CO2.delay = sd(CO2.delay, na.rm=T)/sqrt(sum(!is.na(CO2.delay)))
), by= .(fertilization, event)]

delay.total <- peak.delay[, .(
        NO.delay = mean(NO.delay, na.rm=T),
        sd.NO.delay = sd(NO.delay, na.rm=T),
        se.NO.delay = sd(NO.delay, na.rm=T)/sqrt(sum(!is.na(NO.delay))),
        
        N2O.delay = mean(N2O.delay, na.rm=T),
        sd.N2O.delay = sd(N2O.delay, na.rm=T),
        se.N2O.delay = sd(N2O.delay, na.rm=T)/sqrt(sum(!is.na(N2O.delay))),
        
        CO2.delay = mean(CO2.delay, na.rm=T),
        sd.CO2.delay = sd(CO2.delay, na.rm=T),
        se.CO2.delay = sd(CO2.delay, na.rm=T)/sqrt(sum(!is.na(CO2.delay)))
)]


# write.summaries
mydata <- copy (peak.delay)
no.format <- names(mydata)[names(mydata) %in% c("incabation", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.out, "/peak_delay_by_core.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (delay.total)
no.format <- names(mydata)[names(mydata) %in% c("incabation", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.out, "/peak_delay.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (delay.fert)
no.format <- names(mydata)[names(mydata) %in% c("incabation", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.out, "/peak_delay_by_fert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (delay.rain)
no.format <- names(mydata)[names(mydata) %in% c("incabation", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.out, "/peak_delay_by_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (delay.till)
no.format <- names(mydata)[names(mydata) %in% c("incabation", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.out, "/peak_delay_by_tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (delay.treat)
no.format <- names(mydata)[names(mydata) %in% c("incabation", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.out, "/peak_delay_by_treatment.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

###
with(peak.delay, boxplot(NO.delay ~tillage))
with(peak.delay, boxplot(NO.delay ~ event + tillage))
with(peak.delay, boxplot(NO.delay ~tillage + event))

with(peak.delay, boxplot(NO.delay ~fertilization))
with(peak.delay, boxplot(NO.delay ~event + fertilization))
with(peak.delay, boxplot(NO.delay ~fertilization + event))

with(peak.delay, boxplot(NO.delay ~precipitation))
with(peak.delay, boxplot(NO.delay ~event + precipitation))
with(peak.delay, boxplot(NO.delay ~precipitation + event))

###
with(peak.delay, boxplot(N2O.delay ~tillage))
with(peak.delay, boxplot(N2O.delay ~ event + tillage))
with(peak.delay, boxplot(N2O.delay ~tillage + event))

with(peak.delay, boxplot(N2O.delay ~fertilization))
with(peak.delay, boxplot(N2O.delay ~event + fertilization))
with(peak.delay, boxplot(N2O.delay ~fertilization + event))

with(peak.delay, boxplot(N2O.delay ~precipitation))
with(peak.delay, boxplot(N2O.delay ~event + precipitation))
with(peak.delay, boxplot(N2O.delay ~precipitation + event))

###
with(peak.delay, boxplot(CO2.delay ~tillage))
with(peak.delay, boxplot(CO2.delay ~ event + tillage))
with(peak.delay, boxplot(CO2.delay ~tillage + event))

with(peak.delay, boxplot(CO2.delay ~fertilization))
with(peak.delay, boxplot(CO2.delay ~event + fertilization))
with(peak.delay, boxplot(CO2.delay ~fertilization + event))

with(peak.delay, boxplot(CO2.delay ~precipitation))
with(peak.delay, boxplot(CO2.delay ~event + precipitation))
with(peak.delay, boxplot(CO2.delay ~precipitation + event))
