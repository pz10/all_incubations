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
# data <- data[fertilization>0]

data[,Rr.NO:= NO/fert0.NO] # response ratio as in Liu Greaver 2009
data[,Rr.N2O:= N2O/fert0.N2O]
data[,Rr.NH3:= NH3/fert0.NH3]
data[,Rr.CO2:= CO2/fert0.CO2]
data[,Rr.CH4:= CH4/fert0.CH4]
data[,Rr.NO.N2O:= NO.N2O/fert0.NO.N2O]
data[,Rr.NO.N2O.NH3:= NO.N2O.NH3/fert0.NO.N2O.NH3]
data[,Rr.NO.N2O.NH3filled:= NO.N2O.NH3filled/fert0.NO.N2O.NH3.f]

setkey(data, fertilization, precipitation, tillage)

# summaries
s.FPT <- data[,.(Rr.NO= mean(Rr.NO, na.rm=T),
                 Rr.NO.se= sd(Rr.NO, na.rm=T)/sqrt(sum(!is.na(Rr.NO))),
                 
                 Rr.N2O= mean(Rr.N2O, na.rm=T),
                 Rr.N2O.se= sd(Rr.N2O, na.rm=T)/sqrt(sum(!is.na(Rr.N2O))),
                 
                 Rr.NH3= mean(Rr.NH3, na.rm=T),
                 Rr.NH3.se= sd(Rr.NH3, na.rm=T)/sqrt(sum(!is.na(Rr.NH3))),
                 
                 Rr.CO2= mean(Rr.CO2, na.rm=T),
                 Rr.CO2.se= sd(Rr.CO2, na.rm=T)/sqrt(sum(!is.na(Rr.CO2))),
                 
                 Rr.CH4= mean(Rr.CH4, na.rm=T),
                 Rr.CH4.se= sd(Rr.CH4, na.rm=T)/sqrt(sum(!is.na(Rr.CH4))),
                 
                 Rr.NO.N2O= mean(Rr.NO.N2O, na.rm=T),
                 Rr.NO.N2O.se= sd(Rr.NO.N2O, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O))),
                 
                 Rr.NO.N2O.NH3= mean(Rr.NO.N2O.NH3, na.rm=T),
                 Rr.NO.N2O.NH3.se= sd(Rr.NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O.NH3))),
                 
                 Rr.NO.N2O.NH3filled= mean(Rr.NO.N2O.NH3filled, na.rm=T),
                 Rr.NO.N2O.NH3filled.se= sd(Rr.NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O.NH3filled)))
                 
), by=.(fertilization, precipitation, tillage)]

s.FP <- data[,.(Rr.NO= mean(Rr.NO, na.rm=T),
                Rr.NO.se= sd(Rr.NO, na.rm=T)/sqrt(sum(!is.na(Rr.NO))),
                
                Rr.N2O= mean(Rr.N2O, na.rm=T),
                Rr.N2O.se= sd(Rr.N2O, na.rm=T)/sqrt(sum(!is.na(Rr.N2O))),
                
                Rr.NH3= mean(Rr.NH3, na.rm=T),
                Rr.NH3.se= sd(Rr.NH3, na.rm=T)/sqrt(sum(!is.na(Rr.NH3))),
                
                Rr.CO2= mean(Rr.CO2, na.rm=T),
                Rr.CO2.se= sd(Rr.CO2, na.rm=T)/sqrt(sum(!is.na(Rr.CO2))),
                
                Rr.CH4= mean(Rr.CH4, na.rm=T),
                Rr.CH4.se= sd(Rr.CH4, na.rm=T)/sqrt(sum(!is.na(Rr.CH4))),
                
                Rr.NO.N2O= mean(Rr.NO.N2O, na.rm=T),
                Rr.NO.N2O.se= sd(Rr.NO.N2O, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O))),
                
                Rr.NO.N2O.NH3= mean(Rr.NO.N2O.NH3, na.rm=T),
                Rr.NO.N2O.NH3.se= sd(Rr.NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O.NH3))),
                
                Rr.NO.N2O.NH3filled= mean(Rr.NO.N2O.NH3filled, na.rm=T),
                Rr.NO.N2O.NH3filled.se= sd(Rr.NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O.NH3filled)))
                
), by=.(fertilization, precipitation)]

s.FT <- data[,.(Rr.NO= mean(Rr.NO, na.rm=T),
                Rr.NO.se= sd(Rr.NO, na.rm=T)/sqrt(sum(!is.na(Rr.NO))),
                
                Rr.N2O= mean(Rr.N2O, na.rm=T),
                Rr.N2O.se= sd(Rr.N2O, na.rm=T)/sqrt(sum(!is.na(Rr.N2O))),
                
                Rr.NH3= mean(Rr.NH3, na.rm=T),
                Rr.NH3.se= sd(Rr.NH3, na.rm=T)/sqrt(sum(!is.na(Rr.NH3))),
                
                Rr.CO2= mean(Rr.CO2, na.rm=T),
                Rr.CO2.se= sd(Rr.CO2, na.rm=T)/sqrt(sum(!is.na(Rr.CO2))),
                
                Rr.CH4= mean(Rr.CH4, na.rm=T),
                Rr.CH4.se= sd(Rr.CH4, na.rm=T)/sqrt(sum(!is.na(Rr.CH4))),
                
                Rr.NO.N2O= mean(Rr.NO.N2O, na.rm=T),
                Rr.NO.N2O.se= sd(Rr.NO.N2O, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O))),
                
                Rr.NO.N2O.NH3= mean(Rr.NO.N2O.NH3, na.rm=T),
                Rr.NO.N2O.NH3.se= sd(Rr.NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O.NH3))),
                
                Rr.NO.N2O.NH3filled= mean(Rr.NO.N2O.NH3filled, na.rm=T),
                Rr.NO.N2O.NH3filled.se= sd(Rr.NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O.NH3filled)))
                
), by=.(fertilization, tillage)]

s.F <- data[,.(Rr.NO= mean(Rr.NO, na.rm=T),
               Rr.NO.se= sd(Rr.NO, na.rm=T)/sqrt(sum(!is.na(Rr.NO))),
               
               Rr.N2O= mean(Rr.N2O, na.rm=T),
               Rr.N2O.se= sd(Rr.N2O, na.rm=T)/sqrt(sum(!is.na(Rr.N2O))),
               
               Rr.NH3= mean(Rr.NH3, na.rm=T),
               Rr.NH3.se= sd(Rr.NH3, na.rm=T)/sqrt(sum(!is.na(Rr.NH3))),
               
               Rr.CO2= mean(Rr.CO2, na.rm=T),
               Rr.CO2.se= sd(Rr.CO2, na.rm=T)/sqrt(sum(!is.na(Rr.CO2))),
               
               Rr.CH4= mean(Rr.CH4, na.rm=T),
               Rr.CH4.se= sd(Rr.CH4, na.rm=T)/sqrt(sum(!is.na(Rr.CH4))),
               
               Rr.NO.N2O= mean(Rr.NO.N2O, na.rm=T),
               Rr.NO.N2O.se= sd(Rr.NO.N2O, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O))),
               
               Rr.NO.N2O.NH3= mean(Rr.NO.N2O.NH3, na.rm=T),
               Rr.NO.N2O.NH3.se= sd(Rr.NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O.NH3))),
               
               Rr.NO.N2O.NH3filled= mean(Rr.NO.N2O.NH3filled, na.rm=T),
               Rr.NO.N2O.NH3filled.se= sd(Rr.NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(Rr.NO.N2O.NH3filled)))
               
), by=.(fertilization)]

# write summariesSummary.numeric_version(# write.summaries
mydata <- copy (s.FPT)
no.format <- names(mydata)[names(mydata) %in% c("fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.Rr, "/Rr_by_fert_rain_till.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


mydata <- copy (s.FP)
no.format <- names(mydata)[names(mydata) %in% c("fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.Rr, "/Rr_by_fert_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


mydata <- copy (s.FT)
no.format <- names(mydata)[names(mydata) %in% c("fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.Rr, "/Rr_by_fert_till.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


mydata <- copy (s.F)
no.format <- names(mydata)[names(mydata) %in% c("fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.fert.Rr, "/Rr_by_fert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)