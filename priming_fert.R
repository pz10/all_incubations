# output files
folder.out <- paste0(output_path, "/priming")


# input files
folder.in <- paste0(output_path, "/cum_fluxes_14")

################################################################################################################################################################
################################################################################
# instant flux files
cumfile <- paste0(folder.in, "/cumFlux_bycore.dat")


# read data files
data <- fread(input = cumfile)


fert <- data[,.(
        NO = mean(NO, na.rm=T),
        NO.to0 = as.numeric(NA),
        NO.minus0 = as.numeric(NA),
        sd.NO = sd(NO, na.rm=T),
        se.NO = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO))),
        count = sum(!is.na(NO)),
        
        N2O = mean(N2O, na.rm=T),
        N2O.to0 = as.numeric(NA),
        N2O.minus0 = as.numeric(NA),
        sd.N2O = sd(N2O, na.rm=T),
        se.N2O = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O))),
        count = sum(!is.na(N2O)),
        
        NH3 = mean(NH3, na.rm=T),
        NH3.to0 = as.numeric(NA),
        NH3.minus0 = as.numeric(NA),
        sd.NH3 = sd(NH3, na.rm=T),
        se.NH3 = sd(NH3, na.rm=T)/sqrt(sum(!is.na(NH3))),
        count = sum(!is.na(NH3)),
        
        NO.N2O = mean(NO.N2O, na.rm=T),
        NO.N2O.to0 = as.numeric(NA),
        NO.N2O.minus0 = as.numeric(NA),
        sd.NO.N2O = sd(NO.N2O, na.rm=T),
        se.NO.N2O = sd(NO.N2O, na.rm=T)/sqrt(sum(!is.na(NO.N2O))),
        count = sum(!is.na(NO.N2O)),
        
        NO.N2O.NH3 = mean(NO.N2O.NH3, na.rm=T),
        NO.N2O.NH3.to0 = as.numeric(NA),
        NO.N2O.NH3.minus0 = as.numeric(NA),
        sd.NO.N2O.NH3 = sd(NO.N2O.NH3, na.rm=T),
        se.NO.N2O.NH3 = sd(NO.N2O.NH3, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3))),
        count = sum(!is.na(NO.N2O.NH3)),
        
        CO2 = mean(CO2, na.rm=T),
        CO2.to0 = as.numeric(NA),
        CO2.minus0 = as.numeric(NA),
        sd.CO2 = sd(CO2, na.rm=T),
        se.CO2 = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2))),
        count = sum(!is.na(CO2))

), by=fertilization]


fert[,NO.to0:= NO/NO[1]]
fert[,N2O.to0:= N2O/N2O[1]]
fert[,NO.N2O.to0:= NO.N2O/NO.N2O[1]]
fert[,NH3.to0:= NH3/NH3[1]]
fert[,NO.N2O.NH3.to0:= NO.N2O.NH3/NO.N2O.NH3[1]]
fert[,CO2.to0:= CO2/CO2[1]]


fert[,NO.minus0:= NO-NO[1]]
fert[,N2O.minus0:= N2O-N2O[1]]
fert[,NO.N2O.minus0:= NO.N2O-NO.N2O[1]]
fert[,NH3.minus0:= NH3-NH3[1]]
fert[,NO.N2O.NH3.minus0:= NO.N2O.NH3-NO.N2O.NH3[1]]
fert[,CO2.minus0:= CO2-CO2[1]]

mydata <- copy (fert)
no.format <- c("fertilization", "count")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.out, "/Fertilization_priming.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)
