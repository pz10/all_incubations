my.folder.out <-paste0(output_path, "/cum_fluxes_14/cumFlux_by_event")
dir.create(my.folder.out)

d1 <- fread(input = cum.flux.1st.14)
d2 <- fread(input = cum.flux.2nd.14)
d3 <- fread(input = cum.flux.3rd.14)

d1[,event:=1]
d2[,event:=2]
d3[,event:=3]

dd <- rbindlist(list(d1, d2, d3))

################################################################################
### A incubation
cum.A <- copy(dd)
cum.A[, NO:= aNO]
cum.A[, N2O:= aN2O]
cum.A[, CO2:= aCO2]
cum.A[, CH4:= aCH4]
# cum.A[, todelete:=NULL, with=FALSE]
cum.A[, incubation:= "A"]

### B incubation
cum.B <- copy(dd)
cum.B[, NO:= bNO]
cum.B[, N2O:= bN2O]
cum.B[, CO2:= bCO2]
cum.B[, CH4:= bCH4]
# cum.B[, todelete:=NULL, with=FALSE]
cum.B[, incubation:= "B"]
# cum.B[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### C incubation
cum.C <- copy(dd)
cum.C[, NO:= cNO]
cum.C[, N2O:= cN2O]
cum.C[, CO2:= cCO2]
cum.C[, CH4:= cCH4]
# cum.C[, todelete:=NULL, with=FALSE]
cum.C[, incubation:= "C"]
# cum.C[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### D incubation
cum.D <- copy(dd)
cum.D[, NO:= dNO]
cum.D[, N2O:= dN2O]
cum.D[, CO2:= dCO2]
cum.D[, CH4:= dCH4]
# cum.D[, todelete:=NULL, with=FALSE]
cum.D[, incubation:= "D"]
# cum.D[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### all incubation binding
dd <- rbind(cum.A, cum.B, cum.C, cum.D)
dd <- dd[,.(treatment, fertilizer, precipitation, tillage, event, incubation,
                NO, N2O, CO2, CH4)]
setnames(dd, "fertilizer", "fertilization")
dd[incubation=="D", CH4:= NA]

################################################################################
# merge NH3.event data
setkey(dd, fertilization, precipitation, tillage, event, incubation)
setkey(NH3.event, fertilization, precipitation, tillage, event, incubation)
dd <- NH3.event[dd]

# mean NH3 value for D incubation
dd[, NH3.filled:= NH3]
dd[, aux:= sum(NH3, na.rm=T)/sum(!is.na(NH3)), by=treatment]
dd[incubation=="D", NH3.filled:= aux]
dd[, aux:=NULL]

################################################################################
dd[precipitation == "c", precipitation:= "h"]

################################################################################
# `%+na%` <- function(x,y) {ifelse( is.na(x), y, ifelse( is.na(y), x, x+y) )}
# dd[,NO.N2O:= NO %+na% N2O]
# dd[,NO.N2O.NH3:= NO %+na% N2O %+na% NH3]
# dd[,NO.N2O.NH3filled:= NO %+na% N2O %+na% NH3.filled]

dd[,NO.N2O:= NO + N2O]
dd[,NO.N2O.NH3:= NO + N2O + NH3]
dd[,NO.N2O.NH3filled:= NO + N2O + NH3.filled]

setkey(dd, fertilization, precipitation, tillage, event, incubation, treatment)

# summaries
s.total <- dd[,.(
        NO = mean(NO, na.rm=T),
        NO.se = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.se = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O))),
        
        NH3 = mean(NH3, na.rm=T),
        NH3.se = sd(NH3, na.rm=T)/sqrt(sum(!is.na(NH3))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.se = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2))),
        
        CH4 = mean(CH4, na.rm=T),
        CH4.se = sd(CH4, na.rm=T)/sqrt(sum(!is.na(CH4))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.se = sd(NO.N2O, na.rm=T)/sqrt(sum(!is.na(NO.N2O))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.se = sd(NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3))),
        
        NO.N2O.NH3filled = mean(NO.N2O.NH3filled, na.rm=T),
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled)))
        
), by=.(event)]

s.core <- dd[,.(
        NO = NO,
        N2O = N2O,
        NH3 = NH3,
        CO2 = CO2,
        CH4 = CH4,
        NO.N2O = NO.N2O,
        NO.N2O.NH3 = NO.N2O.NH3,
        NO.N2O.NH3filled = NO.N2O.NH3filled
), by=.(fertilization, precipitation, tillage, incubation, treatment, event)]

s.treat <- dd[,.(
        NO = mean(NO, na.rm=T),
        NO.se = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.se = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O))),
        
        NH3 = mean(NH3, na.rm=T),
        NH3.se = sd(NH3, na.rm=T)/sqrt(sum(!is.na(NH3))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.se = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2))),
        
        CH4 = mean(CH4, na.rm=T),
        CH4.se = sd(CH4, na.rm=T)/sqrt(sum(!is.na(CH4))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.se = sd(NO.N2O, na.rm=T)/sqrt(sum(!is.na(NO.N2O))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.se = sd(NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3))),
        
        NO.N2O.NH3filled = mean(NO.N2O.NH3filled, na.rm=T),
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled)))
        
), by=.(treatment, event)]

s.till <- dd[,.(
        NO = mean(NO, na.rm=T),
        NO.se = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.se = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O))),
        
        NH3 = mean(NH3, na.rm=T),
        NH3.se = sd(NH3, na.rm=T)/sqrt(sum(!is.na(NH3))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.se = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2))),
        
        CH4 = mean(CH4, na.rm=T),
        CH4.se = sd(CH4, na.rm=T)/sqrt(sum(!is.na(CH4))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.se = sd(NO.N2O, na.rm=T)/sqrt(sum(!is.na(NO.N2O))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.se = sd(NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3))),
        
        NO.N2O.NH3filled = mean(NO.N2O.NH3filled, na.rm=T),
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled)))
        
), by=.(tillage, event)]

s.fert <- dd[,.(
        NO = mean(NO, na.rm=T),
        NO.se = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.se = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O))),
        
        NH3 = mean(NH3, na.rm=T),
        NH3.se = sd(NH3, na.rm=T)/sqrt(sum(!is.na(NH3))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.se = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2))),
        
        CH4 = mean(CH4, na.rm=T),
        CH4.se = sd(CH4, na.rm=T)/sqrt(sum(!is.na(CH4))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.se = sd(NO.N2O, na.rm=T)/sqrt(sum(!is.na(NO.N2O))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.se = sd(NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3))),
        
        NO.N2O.NH3filled = mean(NO.N2O.NH3filled, na.rm=T),
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled)))
        
), by=.(fertilization, event)]

s.rain <- dd[,.(
        NO = mean(NO, na.rm=T),
        NO.se = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.se = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O))),
        
        NH3 = mean(NH3, na.rm=T),
        NH3.se = sd(NH3, na.rm=T)/sqrt(sum(!is.na(NH3))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.se = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2))),
        
        CH4 = mean(CH4, na.rm=T),
        CH4.se = sd(CH4, na.rm=T)/sqrt(sum(!is.na(CH4))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.se = sd(NO.N2O, na.rm=T)/sqrt(sum(!is.na(NO.N2O))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.se = sd(NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3))),
        
        NO.N2O.NH3filled = mean(NO.N2O.NH3filled, na.rm=T),
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled)))
        
), by=.(precipitation, event)]

s.fert.till <- dd[,.(
        NO = mean(NO, na.rm=T),
        NO.se = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.se = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O))),
        
        NH3 = mean(NH3, na.rm=T),
        NH3.se = sd(NH3, na.rm=T)/sqrt(sum(!is.na(NH3))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.se = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2))),
        
        CH4 = mean(CH4, na.rm=T),
        CH4.se = sd(CH4, na.rm=T)/sqrt(sum(!is.na(CH4))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.se = sd(NO.N2O, na.rm=T)/sqrt(sum(!is.na(NO.N2O))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.se = sd(NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3))),
        
        NO.N2O.NH3filled = mean(NO.N2O.NH3filled, na.rm=T),
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled)))
        
), by=.(fertilization, tillage, event)]

s.fert.rain <- dd[,.(
        NO = mean(NO, na.rm=T),
        NO.se = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.se = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O))),
        
        NH3 = mean(NH3, na.rm=T),
        NH3.se = sd(NH3, na.rm=T)/sqrt(sum(!is.na(NH3))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.se = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2))),
        
        CH4 = mean(CH4, na.rm=T),
        CH4.se = sd(CH4, na.rm=T)/sqrt(sum(!is.na(CH4))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.se = sd(NO.N2O, na.rm=T)/sqrt(sum(!is.na(NO.N2O))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.se = sd(NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3))),
        
        NO.N2O.NH3filled = mean(NO.N2O.NH3filled, na.rm=T),
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled)))
        
), by=.(fertilization, precipitation, event)]

s.till.rain <- dd[,.(
        NO = mean(NO, na.rm=T),
        NO.se = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO))),
        
        N2O = mean(N2O, na.rm=T),
        N2O.se = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O))),
        
        NH3 = mean(NH3, na.rm=T),
        NH3.se = sd(NH3, na.rm=T)/sqrt(sum(!is.na(NH3))),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.se = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2))),
        
        CH4 = mean(CH4, na.rm=T),
        CH4.se = sd(CH4, na.rm=T)/sqrt(sum(!is.na(CH4))),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.se = sd(NO.N2O, na.rm=T)/sqrt(sum(!is.na(NO.N2O))),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.se = sd(NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3))),
        
        NO.N2O.NH3filled = mean(NO.N2O.NH3filled, na.rm=T),
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled)))
        
), by=.(tillage, precipitation, event)]


# write.summaries
mydata <- copy (s.core)
no.format <- names(mydata)[names(mydata) %in% c("incabation", "treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(my.folder.out, "/EventCumFlux_bycore.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.treat)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(my.folder.out, "/EventCumFlux_bytreat.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.till)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(my.folder.out, "/EventCumFlux_bytillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.fert)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(my.folder.out, "/EventCumFlux_byfert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.rain)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(my.folder.out, "/EventCumFlux_byrain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.fert.till)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(my.folder.out, "/EventCumFlux_by_fert_till.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.fert.rain)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(my.folder.out, "/EventCumFlux_by_fert_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.till.rain)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(my.folder.out, "/EventCumFlux_by_till_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.total)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(my.folder.out, "/EventCumFlux.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

rm(mydata)

