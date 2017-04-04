# cum flux files
cum.treat <- paste0(input_path, "/_all_fluxes/cum_fluxes_14/cumFlux_bytreat.dat")
cum.core <- paste0(input_path, "/_all_fluxes/cum_fluxes_14/cumFlux_bycore.dat")

dir.create(folder.fert.effect, recursive = T)

core <- fread(cum.core)
data <- fread(cum.treat)

data <- data[fertilization==0, .(precipitation, tillage,
                                 fert0.NO=NO, fert0.N2O=N2O, fert0.NH3=NH3,
                                 fert0.CO2=CO2, fert0.CH4=CH4,
                                 fert0.NO.N2O=NO.N2O, fert0.NO.N2O.NH3=NO.N2O.NH3,
                                 fert0.NO.N2O.NH3.f=NO.N2O.NH3filled)]

setkey(data, precipitation, tillage)
setkey(core, precipitation, tillage)


data <- data[core]
data <- data[fertilization>0]

data[,FIE.NO:= 100*(NO-fert0.NO)/100/fertilization] # (/100): from mg-N m-2 to kg-N ha-1; (*100): get %
data[,FIE.N2O:= (N2O-fert0.N2O)/fertilization]
data[,FIE.NH3:= (NH3-fert0.NH3)/fertilization]
data[,FIE.CO2:= (CO2-fert0.CO2)/fertilization]
data[,FIE.CH4:= (CH4-fert0.CH4)/fertilization]
data[,FIE.NO.N2O:= (NO.N2O-fert0.NO.N2O)/fertilization]
data[,FIE.NO.N2O.NH3:= (NO.N2O.NH3-fert0.NO.N2O.NH3)/fertilization]
data[,FIE.NO.N2O.NH3filled:= (NO.N2O.NH3filled-fert0.NO.N2O.NH3.f)/fertilization]

setkey(data, fertilization, precipitation, tillage)

# summaries
s.FPT <- data[,.(FIE.NO= mean(FIE.NO, na.rm=T),
        FIE.NO.se= sd(FIE.NO, na.rm=T)/sqrt(sum(!is.na(FIE.NO))),
        
        FIE.N2O= mean(FIE.N2O, na.rm=T),
        FIE.N2O.se= sd(FIE.N2O, na.rm=T)/sqrt(sum(!is.na(FIE.N2O))),
        
        FIE.NH3= mean(FIE.NH3, na.rm=T),
        FIE.NH3.se= sd(FIE.NH3, na.rm=T)/sqrt(sum(!is.na(FIE.NH3))),
        
        FIE.CO2= mean(FIE.CO2, na.rm=T),
        FIE.CO2.se= sd(FIE.CO2, na.rm=T)/sqrt(sum(!is.na(FIE.CO2))),
        
        FIE.CH4= mean(FIE.CH4, na.rm=T),
        FIE.CH4.se= sd(FIE.CH4, na.rm=T)/sqrt(sum(!is.na(FIE.CH4))),
        
        FIE.NO.N2O= mean(FIE.NO.N2O, na.rm=T),
        FIE.NO.N2O.se= sd(FIE.NO.N2O, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O))),
        
        FIE.NO.N2O.NH3= mean(FIE.NO.N2O.NH3, na.rm=T),
        FIE.NO.N2O.NH3.se= sd(FIE.NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O.NH3))),
        
        FIE.NO.N2O.NH3filled= mean(FIE.NO.N2O.NH3filled, na.rm=T),
        FIE.NO.N2O.NH3filled.se= sd(FIE.NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O.NH3filled)))
        
        ), by=.(fertilization, precipitation, tillage)]

s.FP <- data[,.(FIE.NO= mean(FIE.NO, na.rm=T),
           FIE.NO.se= sd(FIE.NO, na.rm=T)/sqrt(sum(!is.na(FIE.NO))),
           
           FIE.N2O= mean(FIE.N2O, na.rm=T),
           FIE.N2O.se= sd(FIE.N2O, na.rm=T)/sqrt(sum(!is.na(FIE.N2O))),
           
           FIE.NH3= mean(FIE.NH3, na.rm=T),
           FIE.NH3.se= sd(FIE.NH3, na.rm=T)/sqrt(sum(!is.na(FIE.NH3))),
           
           FIE.CO2= mean(FIE.CO2, na.rm=T),
           FIE.CO2.se= sd(FIE.CO2, na.rm=T)/sqrt(sum(!is.na(FIE.CO2))),
           
           FIE.CH4= mean(FIE.CH4, na.rm=T),
           FIE.CH4.se= sd(FIE.CH4, na.rm=T)/sqrt(sum(!is.na(FIE.CH4))),
           
           FIE.NO.N2O= mean(FIE.NO.N2O, na.rm=T),
           FIE.NO.N2O.se= sd(FIE.NO.N2O, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O))),
           
           FIE.NO.N2O.NH3= mean(FIE.NO.N2O.NH3, na.rm=T),
           FIE.NO.N2O.NH3.se= sd(FIE.NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O.NH3))),
           
           FIE.NO.N2O.NH3filled= mean(FIE.NO.N2O.NH3filled, na.rm=T),
           FIE.NO.N2O.NH3filled.se= sd(FIE.NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O.NH3filled)))
           
), by=.(fertilization, precipitation)]

s.FT <- data[,.(FIE.NO= mean(FIE.NO, na.rm=T),
           FIE.NO.se= sd(FIE.NO, na.rm=T)/sqrt(sum(!is.na(FIE.NO))),
           
           FIE.N2O= mean(FIE.N2O, na.rm=T),
           FIE.N2O.se= sd(FIE.N2O, na.rm=T)/sqrt(sum(!is.na(FIE.N2O))),
           
           FIE.NH3= mean(FIE.NH3, na.rm=T),
           FIE.NH3.se= sd(FIE.NH3, na.rm=T)/sqrt(sum(!is.na(FIE.NH3))),
           
           FIE.CO2= mean(FIE.CO2, na.rm=T),
           FIE.CO2.se= sd(FIE.CO2, na.rm=T)/sqrt(sum(!is.na(FIE.CO2))),
           
           FIE.CH4= mean(FIE.CH4, na.rm=T),
           FIE.CH4.se= sd(FIE.CH4, na.rm=T)/sqrt(sum(!is.na(FIE.CH4))),
           
           FIE.NO.N2O= mean(FIE.NO.N2O, na.rm=T),
           FIE.NO.N2O.se= sd(FIE.NO.N2O, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O))),
           
           FIE.NO.N2O.NH3= mean(FIE.NO.N2O.NH3, na.rm=T),
           FIE.NO.N2O.NH3.se= sd(FIE.NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O.NH3))),
           
           FIE.NO.N2O.NH3filled= mean(FIE.NO.N2O.NH3filled, na.rm=T),
           FIE.NO.N2O.NH3filled.se= sd(FIE.NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O.NH3filled)))
           
), by=.(fertilization, tillage)]

s.F <- data[,.(FIE.NO= mean(FIE.NO, na.rm=T),
                FIE.NO.se= sd(FIE.NO, na.rm=T)/sqrt(sum(!is.na(FIE.NO))),
                
                FIE.N2O= mean(FIE.N2O, na.rm=T),
                FIE.N2O.se= sd(FIE.N2O, na.rm=T)/sqrt(sum(!is.na(FIE.N2O))),
                
                FIE.NH3= mean(FIE.NH3, na.rm=T),
                FIE.NH3.se= sd(FIE.NH3, na.rm=T)/sqrt(sum(!is.na(FIE.NH3))),
                
                FIE.CO2= mean(FIE.CO2, na.rm=T),
                FIE.CO2.se= sd(FIE.CO2, na.rm=T)/sqrt(sum(!is.na(FIE.CO2))),
                
                FIE.CH4= mean(FIE.CH4, na.rm=T),
                FIE.CH4.se= sd(FIE.CH4, na.rm=T)/sqrt(sum(!is.na(FIE.CH4))),
                
                FIE.NO.N2O= mean(FIE.NO.N2O, na.rm=T),
                FIE.NO.N2O.se= sd(FIE.NO.N2O, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O))),
                
                FIE.NO.N2O.NH3= mean(FIE.NO.N2O.NH3, na.rm=T),
                FIE.NO.N2O.NH3.se= sd(FIE.NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O.NH3))),
                
                FIE.NO.N2O.NH3filled= mean(FIE.NO.N2O.NH3filled, na.rm=T),
                FIE.NO.N2O.NH3filled.se= sd(FIE.NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(FIE.NO.N2O.NH3filled)))
                
), by=.(fertilization)]

# write summariesSummary.numeric_version(# write.summaries
mydata <- copy (s.FPT)
no.format <- names(mydata)[names(mydata) %in% c("fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.FIE, "/FIE_by_fert_rain_till.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


mydata <- copy (s.FP)
no.format <- names(mydata)[names(mydata) %in% c("fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.FIE, "/FIE_by_fert_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


mydata <- copy (s.FT)
no.format <- names(mydata)[names(mydata) %in% c("fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.FIE, "/FIE_by_fert_till.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


mydata <- copy (s.F)
no.format <- names(mydata)[names(mydata) %in% c("fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.FIE, "/FIE_by_fert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)