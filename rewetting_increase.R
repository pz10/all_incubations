# cum flux files
cumflux <- paste0(input_path, "/_all_fluxes/cum_fluxes_full/1st_event_cum_fluxes.dat")
ins.flux <- paste0(input_path, "/_all_fluxes/eclaire_data_delivery/ECLAIRE.D2.2.dat")

data <- fread(cumflux)
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

### all incubation binding
data <- rbind(cum.A, cum.B, cum.C, cum.D)
data <- data[,.(days, incubation, treatment, fertilizer, precipitation, tillage,
                NO, N2O, CO2, CH4)]
setnames(data, "fertilizer", "fertilization")
data[incubation=="D", CH4:= NA]

setkey(data, incubation, fertilization, precipitation, tillage, treatment)


span <- 3 #in h
spanD <- span/24
cum.span <- data[days==spanD]
cum.span[, ins.NO:= 1000*NO/span*2] #in the triangle, you need twice the mean
cum.span[, ins.N2O:= 1000*N2O/span*2]
cum.span[, ins.CO2:= 1000*CO2/span*2]

mean(cum.span$ins.NO)
sd(cum.span$ins.NO)/sqrt(72)

mean(cum.span$ins.N2O)
sd(cum.span$ins.N2O)/sqrt(72)

mean(cum.span$ins.CO2)
sd(cum.span$ins.CO2)/sqrt(72)

################################################################################
flux <- fread(ins.flux)
flux[, CORE:= paste(treatment, replicate, sep="-")]
setkey(flux, CORE)

max.NO <- 
        flux[,.(replicate=replicate[1],
                treatment=treatment[1],
                fertilizer=fertilizer[1],
                precipitation=precipitation[1],
                tillage=tillage[1],
                days,
                NO,
                NO.max=max(NO, na.rm=T)
        ), by=CORE]
max.NO<-max.NO[NO==NO.max]

max.N2O <- 
        flux[days<15,.(replicate=replicate[1],
                treatment=treatment[1],
                fertilizer=fertilizer[1],
                precipitation=precipitation[1],
                tillage=tillage[1],
                days,
                N2O,
                N2O.max=max(N2O, na.rm=T)
        ), by=CORE]
max.N2O<-max.N2O[N2O==N2O.max]

max.CO2 <- 
        flux[days<15,.(replicate=replicate[1],
                treatment=treatment[1],
                fertilizer=fertilizer[1],
                precipitation=precipitation[1],
                tillage=tillage[1],
                days,
                CO2,
                CO2.max=max(CO2, na.rm=T)
        ), by=CORE]
max.CO2<-max.CO2[CO2==CO2.max]

###############
#NO 
max.NO[,.(days=mean(days),
          days.se= sd(days)/sqrt(sum(!is.na(days))),
          NO=mean(NO),
          NO.se= sd(NO)/sqrt(sum(!is.na(NO)))
)]
max.NO[,.(days=mean(days),
          days.se= sd(days)/sqrt(sum(!is.na(days)))
), by=.(fertilizer)]

max.NO[,.(days=mean(days),
          days.se= sd(days)/sqrt(sum(!is.na(days))),
          NO=mean(NO),
          NO.se= sd(NO)/sqrt(sum(!is.na(NO)))
), by=.(precipitation, fertilizer)]

###############
#N2O 
max.N2O[,.(days=mean(days),
          days.se= sd(days)/sqrt(sum(!is.na(days))),
          N2O=mean(N2O),
          N2O.se= sd(N2O)/sqrt(sum(!is.na(N2O)))
          
)]
max.N2O[,.(days=mean(days),
          days.se= sd(days)/sqrt(sum(!is.na(days))),
          N2O=mean(N2O),
          N2O.se= sd(N2O)/sqrt(sum(!is.na(N2O)))
), by=.(fertilizer)]

max.N2O[,.(days=mean(days),
          days.se= sd(days)/sqrt(sum(!is.na(days))),
          N2O=mean(N2O),
          N2O.se= sd(N2O)/sqrt(sum(!is.na(N2O)))
), by=.(precipitation, fertilizer)]


###############
#CO2 
max.CO2[,.(days=mean(days),
          days.se= sd(days)/sqrt(sum(!is.na(days))),
          CO2=mean(CO2),
          CO2.se= sd(CO2)/sqrt(sum(!is.na(CO2)))
)]
max.CO2[,.(days=mean(days),
           days.se= sd(days)/sqrt(sum(!is.na(days))),
           CO2=mean(CO2),
           CO2.se= sd(CO2)/sqrt(sum(!is.na(CO2)))
), by=.(fertilizer)]

max.CO2[,.(days=mean(days),
          days.se= sd(days)/sqrt(sum(!is.na(days))),
          CO2=mean(CO2),
          CO2.se= sd(CO2)/sqrt(sum(!is.na(CO2)))
), by=.(precipitation, fertilizer)]

#################################
boxplot(max.NO$days, ylim=c(0,15), main="NO peak delay [days]")
boxplot(max.N2O[precipitation=="decr",days], ylim=c(0,15), main="N2O peak delay [days]")
boxplot(max.CO2$days, ylim=c(0,15), main="CO2 peak delay [days]")
