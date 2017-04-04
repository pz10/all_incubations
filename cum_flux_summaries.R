# get NO+N2O and NO+N2O+NH3 columns 
setkey(data, treatment)
data[,NH3:= as.numeric(NH3)]
data[is.na(NH3), NH3:= NA]
data[is.na(CH4), CH4:= NA]

data[, NH3.filled:= NH3]
data[, aux:= sum(NH3, na.rm=T)/sum(!is.na(NH3)), by=treatment]
data[incubation=="D", NH3.filled:= aux]
data[, aux:=NULL]
# data[incubation %in% LETTERS[1:3], .(NH3 = mean(NH3, na.rm=T)), by=treatment]

# `%+na%` <- function(x,y) {ifelse( is.na(x), y, ifelse( is.na(y), x, x+y) )}
# data[,NO.N2O:= NO %+na% N2O]
# data[,NO.N2O.NH3:= NO %+na% N2O %+na% NH3]
# data[,NO.N2O.NH3filled:= NO %+na% N2O %+na% NH3.filled]

data[,NO.N2O:= NO + N2O]
data[,NO.N2O.NH3:= NO + N2O + NH3]
data[,NO.N2O.NH3filled:= NO + N2O + NH3.filled]

setkey(data, incubation, fertilization, precipitation, tillage)


# summaries
s.total <- data[,.(
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

)]

s.core <- data[,.(
        NO = NO,
        N2O = N2O,
        NH3 = NH3,
        CO2 = CO2,
        CH4 = CH4,
        NO.N2O = NO.N2O,
        NO.N2O.NH3 = NO.N2O.NH3,
        NO.N2O.NH3filled = NO.N2O.NH3filled
), by=.(fertilization, precipitation, tillage, incubation, treatment)]

s.treat <- data[,.(
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
        
), by=.(fertilization, precipitation, tillage)]

s.till <- data[,.(
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
        
), by=tillage]

s.fert <- data[,.(
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
        
), by=fertilization]

s.rain <- data[,.(
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
        
), by=precipitation]

s.fert.till <- data[,.(
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
        
), by=.(fertilization, tillage)]

s.fert.rain <- data[,.(
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
        
), by=.(fertilization, precipitation)]

s.till.rain <- data[,.(
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
        
), by=.(tillage, precipitation)]

################################################################################
# write.summaries
mydata <- copy (s.core)
no.format <- names(mydata)[names(mydata) %in% c("incabation", "treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.in, "/cumFlux_bycore.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.treat)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.in, "/cumFlux_bytreat.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.till)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.in, "/cumFlux_bytillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.fert)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.in, "/cumFlux_byfert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.rain)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.in, "/cumFlux_byrain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.fert.till)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.in, "/cumFlux_by_fert_till.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.fert.rain)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.in, "/cumFlux_by_fert_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.till.rain)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.in, "/cumFlux_by_till_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.total)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.in, "/cumFlux.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

################################################################################
################################################################################
# fertilization effect
fert50 <- data[fertilization == 50,.(
        incubation, precipitation, tillage,
        NO.50 = NO,
        N2O.50 = N2O,
        NH3.50 = NH3,
        NO.N2O.50 = NO.N2O,
        NO.N2O.NH3.50 = NO.N2O.NH3,
        CO2.50 = CO2
)]
fert100 <- data[fertilization == 100,.(
        incubation, precipitation, tillage,
        NO.100 = NO,
        N2O.100 = N2O,
        NH3.100 = NH3,
        NO.N2O.100 = NO.N2O,
        NO.N2O.NH3.100 = NO.N2O.NH3,
        CO2.100 = CO2
)]
setkey(fert50, incubation, precipitation, tillage)
setkey(fert100, incubation, precipitation, tillage)
fert <- fert100[fert50]

fert[,NO.50to100:= NO.50/NO.100]
fert[,N2O.50to100:= N2O.50/N2O.100]
fert[,NH3.50to100:= NH3.50/NH3.100]
fert[,NO.N2O.50to100:= NO.N2O.50/NO.N2O.100]
fert[,NO.N2O.NH3.50to100:= NO.N2O.NH3.50/NO.N2O.NH3.100]
fert[,CO2.50to100:= CO2.50/CO2.100]

fert[,NO.100to50:= NO.100/NO.50]
fert[,N2O.100to50:= N2O.100/N2O.50]
fert[,NH3.100to50:= NH3.100/NH3.50]
fert[,NO.N2O.100to50:= NO.N2O.100/NO.N2O.50]
fert[,NO.N2O.NH3.100to50:= NO.N2O.NH3.100/NO.N2O.NH3.50]
fert[,CO2.100to50:= CO2.100/CO2.50]


fert.tillage <- fert[,.(
        NO.50to100 = mean(NO.50to100, na.rm=T),
        NO.50to100.se = sd(NO.50to100, na.rm=T)/sqrt(sum(!is.na(NO.50to100))),
        
        N2O.50to100 = mean(N2O.50to100, na.rm=T),
        N2O.50to100.se = sd(N2O.50to100, na.rm=T)/sqrt(sum(!is.na(N2O.50to100))),
        
        NH3.50to100 = mean(NH3.50to100, na.rm=T),
        NH3.50to100.se = sd(NH3.50to100, na.rm=T)/sqrt(sum(!is.na(NH3.50to100))),
        
        NO.N2O.50to100 = mean(NO.N2O.50to100, na.rm=T),
        NO.N2O.50to100.se = sd(NO.N2O.50to100, na.rm=T)/sqrt(sum(!is.na(NO.N2O.50to100))),
        
        NO.N2O.NH3.50to100 = mean(NO.N2O.NH3.50to100, na.rm=T),
        NON2ONH3.50to100.se = sd(NO.N2O.NH3.50to100, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.50to100))),
        
        CO2.50to100 = mean(CO2.50to100, na.rm=T),
        CO2.50to100.se = sd(CO2.50to100, na.rm=T)/sqrt(sum(!is.na(CO2.50to100))),
        
        
        NO.100to50 = mean(NO.100to50, na.rm=T),
        NO.100to50.se = sd(NO.100to50, na.rm=T)/sqrt(sum(!is.na(NO.100to50))),
        
        N2O.100to50 = mean(N2O.100to50, na.rm=T),
        N2O.100to50.se = sd(N2O.100to50, na.rm=T)/sqrt(sum(!is.na(N2O.100to50))),
        
        NH3.100to50 = mean(NH3.100to50, na.rm=T),
        NH3.100to50.se = sd(NH3.100to50, na.rm=T)/sqrt(sum(!is.na(NH3.100to50))),
        
        NO.N2O.100to50 = mean(NO.N2O.100to50, na.rm=T),
        NO.N2O.100to50.se = sd(NO.N2O.100to50, na.rm=T)/sqrt(sum(!is.na(NO.N2O.100to50))),
        
        NO.N2O.NH3.100to50 = mean(NO.N2O.NH3.100to50, na.rm=T),
        NON2ONH3.100to50.se = sd(NO.N2O.NH3.100to50, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.100to50))),
        
        CO2.100to50 = mean(CO2.100to50, na.rm=T),
        CO2.100to50.se = sd(CO2.100to50, na.rm=T)/sqrt(sum(!is.na(CO2.100to50)))
        
), by=.(tillage)]

fert.rain <- fert[,.(
        NO.50to100 = mean(NO.50to100, na.rm=T),
        NO.50to100.se = sd(NO.50to100, na.rm=T)/sqrt(sum(!is.na(NO.50to100))),
        
        N2O.50to100 = mean(N2O.50to100, na.rm=T),
        N2O.50to100.se = sd(N2O.50to100, na.rm=T)/sqrt(sum(!is.na(N2O.50to100))),
        
        NH3.50to100 = mean(NH3.50to100, na.rm=T),
        NH3.50to100.se = sd(NH3.50to100, na.rm=T)/sqrt(sum(!is.na(NH3.50to100))),
        
        NO.N2O.50to100 = mean(NO.N2O.50to100, na.rm=T),
        NO.N2O.50to100.se = sd(NO.N2O.50to100, na.rm=T)/sqrt(sum(!is.na(NO.N2O.50to100))),
        
        NO.N2O.NH3.50to100 = mean(NO.N2O.NH3.50to100, na.rm=T),
        NON2ONH3.50to100.se = sd(NO.N2O.NH3.50to100, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.50to100))),
        
        CO2.50to100 = mean(CO2.50to100, na.rm=T),
        CO2.50to100.se = sd(CO2.50to100, na.rm=T)/sqrt(sum(!is.na(CO2.50to100))),
        
        
        NO.100to50 = mean(NO.100to50, na.rm=T),
        NO.100to50.se = sd(NO.100to50, na.rm=T)/sqrt(sum(!is.na(NO.100to50))),
        
        N2O.100to50 = mean(N2O.100to50, na.rm=T),
        N2O.100to50.se = sd(N2O.100to50, na.rm=T)/sqrt(sum(!is.na(N2O.100to50))),
        
        NH3.100to50 = mean(NH3.100to50, na.rm=T),
        NH3.100to50.se = sd(NH3.100to50, na.rm=T)/sqrt(sum(!is.na(NH3.100to50))),
        
        NO.N2O.100to50 = mean(NO.N2O.100to50, na.rm=T),
        NO.N2O.100to50.se = sd(NO.N2O.100to50, na.rm=T)/sqrt(sum(!is.na(NO.N2O.100to50))),
        
        NO.N2O.NH3.100to50 = mean(NO.N2O.NH3.100to50, na.rm=T),
        NON2ONH3.100to50.se = sd(NO.N2O.NH3.100to50, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.100to50))),
        
        CO2.100to50 = mean(CO2.100to50, na.rm=T),
        CO2.100to50.se = sd(CO2.100to50, na.rm=T)/sqrt(sum(!is.na(CO2.100to50)))
        
), by=.(precipitation)]

fert.rain.tillage <- fert[,.(
        NO.50to100 = mean(NO.50to100, na.rm=T),
        NO.50to100.se = sd(NO.50to100, na.rm=T)/sqrt(sum(!is.na(NO.50to100))),
        
        N2O.50to100 = mean(N2O.50to100, na.rm=T),
        N2O.50to100.se = sd(N2O.50to100, na.rm=T)/sqrt(sum(!is.na(N2O.50to100))),
        
        NH3.50to100 = mean(NH3.50to100, na.rm=T),
        NH3.50to100.se = sd(NH3.50to100, na.rm=T)/sqrt(sum(!is.na(NH3.50to100))),
        
        NO.N2O.50to100 = mean(NO.N2O.50to100, na.rm=T),
        NO.N2O.50to100.se = sd(NO.N2O.50to100, na.rm=T)/sqrt(sum(!is.na(NO.N2O.50to100))),
        
        NO.N2O.NH3.50to100 = mean(NO.N2O.NH3.50to100, na.rm=T),
        NON2ONH3.50to100.se = sd(NO.N2O.NH3.50to100, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.50to100))),
        
        CO2.50to100 = mean(CO2.50to100, na.rm=T),
        CO2.50to100.se = sd(CO2.50to100, na.rm=T)/sqrt(sum(!is.na(CO2.50to100))),
        
        
        NO.100to50 = mean(NO.100to50, na.rm=T),
        NO.100to50.se = sd(NO.100to50, na.rm=T)/sqrt(sum(!is.na(NO.100to50))),
        
        N2O.100to50 = mean(N2O.100to50, na.rm=T),
        N2O.100to50.se = sd(N2O.100to50, na.rm=T)/sqrt(sum(!is.na(N2O.100to50))),
        
        NH3.100to50 = mean(NH3.100to50, na.rm=T),
        NH3.100to50.se = sd(NH3.100to50, na.rm=T)/sqrt(sum(!is.na(NH3.100to50))),
        
        NO.N2O.100to50 = mean(NO.N2O.100to50, na.rm=T),
        NO.N2O.100to50.se = sd(NO.N2O.100to50, na.rm=T)/sqrt(sum(!is.na(NO.N2O.100to50))),
        
        NO.N2O.NH3.100to50 = mean(NO.N2O.NH3.100to50, na.rm=T),
        NON2ONH3.100to50.se = sd(NO.N2O.NH3.100to50, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.100to50))),
        
        CO2.100to50 = mean(CO2.100to50, na.rm=T),
        CO2.100to50.se = sd(CO2.100to50, na.rm=T)/sqrt(sum(!is.na(CO2.100to50)))
        
), by=.(precipitation, tillage)]

fert.core <- copy(fert)

fert <- fert[,.(
        NO.50to100 = mean(NO.50to100, na.rm=T),
        NO.50to100.se = sd(NO.50to100, na.rm=T)/sqrt(sum(!is.na(NO.50to100))),
        
        N2O.50to100 = mean(N2O.50to100, na.rm=T),
        N2O.50to100.se = sd(N2O.50to100, na.rm=T)/sqrt(sum(!is.na(N2O.50to100))),
        
        NH3.50to100 = mean(NH3.50to100, na.rm=T),
        NH3.50to100.se = sd(NH3.50to100, na.rm=T)/sqrt(sum(!is.na(NH3.50to100))),
        
        NO.N2O.50to100 = mean(NO.N2O.50to100, na.rm=T),
        NO.N2O.50to100.se = sd(NO.N2O.50to100, na.rm=T)/sqrt(sum(!is.na(NO.N2O.50to100))),
        
        NO.N2O.NH3.50to100 = mean(NO.N2O.NH3.50to100, na.rm=T),
        NON2ONH3.50to100.se = sd(NO.N2O.NH3.50to100, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.50to100))),
        
        CO2.50to100 = mean(CO2.50to100, na.rm=T),
        CO2.50to100.se = sd(CO2.50to100, na.rm=T)/sqrt(sum(!is.na(CO2.50to100))),
        
        
        NO.100to50 = mean(NO.100to50, na.rm=T),
        NO.100to50.se = sd(NO.100to50, na.rm=T)/sqrt(sum(!is.na(NO.100to50))),
        
        N2O.100to50 = mean(N2O.100to50, na.rm=T),
        N2O.100to50.se = sd(N2O.100to50, na.rm=T)/sqrt(sum(!is.na(N2O.100to50))),
        
        NH3.100to50 = mean(NH3.100to50, na.rm=T),
        NH3.100to50.se = sd(NH3.100to50, na.rm=T)/sqrt(sum(!is.na(NH3.100to50))),
        
        NO.N2O.100to50 = mean(NO.N2O.100to50, na.rm=T),
        NO.N2O.100to50.se = sd(NO.N2O.100to50, na.rm=T)/sqrt(sum(!is.na(NO.N2O.100to50))),
        
        NO.N2O.NH3.100to50 = mean(NO.N2O.NH3.100to50, na.rm=T),
        NON2ONH3.100to50.se = sd(NO.N2O.NH3.100to50, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3.100to50))),
        
        CO2.100to50 = mean(CO2.100to50, na.rm=T),
        CO2.100to50.se = sd(CO2.100to50, na.rm=T)/sqrt(sum(!is.na(CO2.100to50)))
        
)]

################################################################################
# write.summaries
mydata <- copy (fert.core)
no.format <- names(mydata)[names(mydata) %in% c("incabation", "treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.effect, "/fert_effect_bycore.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (fert.tillage)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.effect, "/fert_effect_bytillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (fert.rain)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.effect, "/fert_effect_byrain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (fert.rain.tillage)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.effect, "/fert_effect_by_till_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (fert)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.effect, "/fert_effect.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)
################################################################################
################################################################################


rm(mydata)
source("cum_flux_summaries_GWP.R")
source("cum_flux_summaries_byEvent.R")
source("fert_FIE.R")
source("fert_Rr.R")

