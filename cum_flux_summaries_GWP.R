mydata <- copy(data)

# calcultate GWP CO2 eq
mydata[, CH4.4replicates:= CH4]
mydata[is.na(CH4.4replicates), CH4.4replicates:= 0]

gwpCH4 <- 25
gwpN2O <- 298
mydata[,GWP.CO2:= CO2]
mydata[,GWP.CH4:= CH4.4replicates/1000*16/12 * gwpCH4 * 12/44]
mydata[,GWP.N2O:= N2O/1000*44/28 * gwpN2O * 12/44]
mydata[,GWP.all:= GWP.CO2 + GWP.CH4 + GWP.N2O]
mydata[,GWP.CO2eq.mgCo2m2h:= GWP.all / 14/3/24 * 1000 * 44/12]

# summaries
s.total <- mydata[,.(
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
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled))),
        
        
        GWP.CO2 = mean(GWP.CO2, na.rm=T),
        GWP.CO2.se = sd(GWP.CO2, na.rm=T)/sqrt(sum(!is.na(GWP.CO2))),
        
        GWP.CH4 = mean(GWP.CH4, na.rm=T),
        GWP.CH4.se = sd(GWP.CH4, na.rm=T)/sqrt(sum(!is.na(GWP.CH4))),
        
        GWP.N2O = mean(GWP.N2O, na.rm=T),
        GWP.N2O.se = sd(GWP.N2O, na.rm=T)/sqrt(sum(!is.na(GWP.N2O))),
        
        GWP.all = mean(GWP.all, na.rm=T),
        GWP.all.se = sd(GWP.all, na.rm=T)/sqrt(sum(!is.na(GWP.all))),
        
        GWP.CO2eq.mgCo2m2h = mean(GWP.CO2eq.mgCo2m2h, na.rm=T),
        GWP.CO2eq.mgCo2m2h.se = sd(GWP.CO2eq.mgCo2m2h, na.rm=T)/sqrt(sum(!is.na(GWP.CO2eq.mgCo2m2h)))
        
)]

s.core <- mydata[,.(
        NO = NO,
        N2O = N2O,
        NH3 = NH3,
        CO2 = CO2,
        CH4 = CH4,
        NO.N2O = NO.N2O,
        NO.N2O.NH3 = NO.N2O.NH3,
        NO.N2O.NH3filled = NO.N2O.NH3filled,
        
        GWP.CO2 = GWP.CO2,
        GWP.CH4 = GWP.CH4,
        GWP.N2O = GWP.N2O,
        GWP.all = GWP.all,
        GWP.CO2eq.mgCo2m2h = GWP.CO2eq.mgCo2m2h

), by=.(fertilization, precipitation, tillage, incubation, treatment)]

s.treat <- mydata[,.(
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
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled))),
        
        
        GWP.CO2 = mean(GWP.CO2, na.rm=T),
        GWP.CO2.se = sd(GWP.CO2, na.rm=T)/sqrt(sum(!is.na(GWP.CO2))),
        
        GWP.CH4 = mean(GWP.CH4, na.rm=T),
        GWP.CH4.se = sd(GWP.CH4, na.rm=T)/sqrt(sum(!is.na(GWP.CH4))),
        
        GWP.N2O = mean(GWP.N2O, na.rm=T),
        GWP.N2O.se = sd(GWP.N2O, na.rm=T)/sqrt(sum(!is.na(GWP.N2O))),
        
        GWP.all = mean(GWP.all, na.rm=T),
        GWP.all.se = sd(GWP.all, na.rm=T)/sqrt(sum(!is.na(GWP.all))),
        
        GWP.CO2eq.mgCo2m2h = mean(GWP.CO2eq.mgCo2m2h, na.rm=T),
        GWP.CO2eq.mgCo2m2h.se = sd(GWP.CO2eq.mgCo2m2h, na.rm=T)/sqrt(sum(!is.na(GWP.CO2eq.mgCo2m2h)))
        
), by=treatment]

s.till <- mydata[,.(
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
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled))),
        
        
        GWP.CO2 = mean(GWP.CO2, na.rm=T),
        GWP.CO2.se = sd(GWP.CO2, na.rm=T)/sqrt(sum(!is.na(GWP.CO2))),
        
        GWP.CH4 = mean(GWP.CH4, na.rm=T),
        GWP.CH4.se = sd(GWP.CH4, na.rm=T)/sqrt(sum(!is.na(GWP.CH4))),
        
        GWP.N2O = mean(GWP.N2O, na.rm=T),
        GWP.N2O.se = sd(GWP.N2O, na.rm=T)/sqrt(sum(!is.na(GWP.N2O))),
        
        GWP.all = mean(GWP.all, na.rm=T),
        GWP.all.se = sd(GWP.all, na.rm=T)/sqrt(sum(!is.na(GWP.all))),
        
        GWP.CO2eq.mgCo2m2h = mean(GWP.CO2eq.mgCo2m2h, na.rm=T),
        GWP.CO2eq.mgCo2m2h.se = sd(GWP.CO2eq.mgCo2m2h, na.rm=T)/sqrt(sum(!is.na(GWP.CO2eq.mgCo2m2h)))
        
), by=tillage]

s.fert <- mydata[,.(
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
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled))),
        
        
        GWP.CO2 = mean(GWP.CO2, na.rm=T),
        GWP.CO2.se = sd(GWP.CO2, na.rm=T)/sqrt(sum(!is.na(GWP.CO2))),
        
        GWP.CH4 = mean(GWP.CH4, na.rm=T),
        GWP.CH4.se = sd(GWP.CH4, na.rm=T)/sqrt(sum(!is.na(GWP.CH4))),
        
        GWP.N2O = mean(GWP.N2O, na.rm=T),
        GWP.N2O.se = sd(GWP.N2O, na.rm=T)/sqrt(sum(!is.na(GWP.N2O))),
        
        GWP.all = mean(GWP.all, na.rm=T),
        GWP.all.se = sd(GWP.all, na.rm=T)/sqrt(sum(!is.na(GWP.all))),
        
        GWP.CO2eq.mgCo2m2h = mean(GWP.CO2eq.mgCo2m2h, na.rm=T),
        GWP.CO2eq.mgCo2m2h.se = sd(GWP.CO2eq.mgCo2m2h, na.rm=T)/sqrt(sum(!is.na(GWP.CO2eq.mgCo2m2h)))
        
), by=fertilization]

s.rain <- mydata[,.(
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
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled))),
        
        
        GWP.CO2 = mean(GWP.CO2, na.rm=T),
        GWP.CO2.se = sd(GWP.CO2, na.rm=T)/sqrt(sum(!is.na(GWP.CO2))),
        
        GWP.CH4 = mean(GWP.CH4, na.rm=T),
        GWP.CH4.se = sd(GWP.CH4, na.rm=T)/sqrt(sum(!is.na(GWP.CH4))),
        
        GWP.N2O = mean(GWP.N2O, na.rm=T),
        GWP.N2O.se = sd(GWP.N2O, na.rm=T)/sqrt(sum(!is.na(GWP.N2O))),
        
        GWP.all = mean(GWP.all, na.rm=T),
        GWP.all.se = sd(GWP.all, na.rm=T)/sqrt(sum(!is.na(GWP.all))),
        
        GWP.CO2eq.mgCo2m2h = mean(GWP.CO2eq.mgCo2m2h, na.rm=T),
        GWP.CO2eq.mgCo2m2h.se = sd(GWP.CO2eq.mgCo2m2h, na.rm=T)/sqrt(sum(!is.na(GWP.CO2eq.mgCo2m2h)))
        
), by=precipitation]

s.fert.till <- mydata[,.(
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
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled))),
        
        
        GWP.CO2 = mean(GWP.CO2, na.rm=T),
        GWP.CO2.se = sd(GWP.CO2, na.rm=T)/sqrt(sum(!is.na(GWP.CO2))),
        
        GWP.CH4 = mean(GWP.CH4, na.rm=T),
        GWP.CH4.se = sd(GWP.CH4, na.rm=T)/sqrt(sum(!is.na(GWP.CH4))),
        
        GWP.N2O = mean(GWP.N2O, na.rm=T),
        GWP.N2O.se = sd(GWP.N2O, na.rm=T)/sqrt(sum(!is.na(GWP.N2O))),
        
        GWP.all = mean(GWP.all, na.rm=T),
        GWP.all.se = sd(GWP.all, na.rm=T)/sqrt(sum(!is.na(GWP.all))),
        
        GWP.CO2eq.mgCo2m2h = mean(GWP.CO2eq.mgCo2m2h, na.rm=T),
        GWP.CO2eq.mgCo2m2h.se = sd(GWP.CO2eq.mgCo2m2h, na.rm=T)/sqrt(sum(!is.na(GWP.CO2eq.mgCo2m2h)))
        
), by=.(fertilization, tillage)]

s.fert.rain <- mydata[,.(
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
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled))),
        
        
        GWP.CO2 = mean(GWP.CO2, na.rm=T),
        GWP.CO2.se = sd(GWP.CO2, na.rm=T)/sqrt(sum(!is.na(GWP.CO2))),
        
        GWP.CH4 = mean(GWP.CH4, na.rm=T),
        GWP.CH4.se = sd(GWP.CH4, na.rm=T)/sqrt(sum(!is.na(GWP.CH4))),
        
        GWP.N2O = mean(GWP.N2O, na.rm=T),
        GWP.N2O.se = sd(GWP.N2O, na.rm=T)/sqrt(sum(!is.na(GWP.N2O))),
        
        GWP.all = mean(GWP.all, na.rm=T),
        GWP.all.se = sd(GWP.all, na.rm=T)/sqrt(sum(!is.na(GWP.all))),
        
        GWP.CO2eq.mgCo2m2h = mean(GWP.CO2eq.mgCo2m2h, na.rm=T),
        GWP.CO2eq.mgCo2m2h.se = sd(GWP.CO2eq.mgCo2m2h, na.rm=T)/sqrt(sum(!is.na(GWP.CO2eq.mgCo2m2h)))
        
), by=.(fertilization, precipitation)]

s.till.rain <- mydata[,.(
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
        NO.N2O.NH3filled.se = sd(NO.N2O.NH3filled, na.rm=T)/sqrt(sum(!is.na(NO.N2O.NH3filled))),
        
        
        GWP.CO2 = mean(GWP.CO2, na.rm=T),
        GWP.CO2.se = sd(GWP.CO2, na.rm=T)/sqrt(sum(!is.na(GWP.CO2))),
        
        GWP.CH4 = mean(GWP.CH4, na.rm=T),
        GWP.CH4.se = sd(GWP.CH4, na.rm=T)/sqrt(sum(!is.na(GWP.CH4))),
        
        GWP.N2O = mean(GWP.N2O, na.rm=T),
        GWP.N2O.se = sd(GWP.N2O, na.rm=T)/sqrt(sum(!is.na(GWP.N2O))),
        
        GWP.all = mean(GWP.all, na.rm=T),
        GWP.all.se = sd(GWP.all, na.rm=T)/sqrt(sum(!is.na(GWP.all))),
        
        GWP.CO2eq.mgCo2m2h = mean(GWP.CO2eq.mgCo2m2h, na.rm=T),
        GWP.CO2eq.mgCo2m2h.se = sd(GWP.CO2eq.mgCo2m2h, na.rm=T)/sqrt(sum(!is.na(GWP.CO2eq.mgCo2m2h)))
        
), by=.(tillage, precipitation)]

################################################################################
# write.summaries
mydata <- copy (s.core)
no.format <- names(mydata)[names(mydata) %in% c("incabation", "treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(no.format)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.GWP, "/GWPcumFlux_bycore.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.treat)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.GWP, "/GWPcumFlux_bytreat.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.till)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.GWP, "/GWPcumFlux_bytillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.fert)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.GWP, "/GWPcumFlux_byfert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.rain)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.GWP, "/GWPcumFlux_byrain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.fert.till)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.GWP, "/GWPcumFlux_by_fert_till.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.fert.rain)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.GWP, "/GWPcumFlux_by_fert_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.till.rain)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.GWP, "/GWPcumFlux_by_till_rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

mydata <- copy (s.total)
to.format2<- c(grep(".se",names(mydata), value=T))
no.format <- names(mydata)[names(mydata) %in% c("treatment", "fertilization", "precipitation", "tillage")]
to.format4 <- names(mydata)[! names(mydata) %in% c(to.format2, no.format)]
mydata[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 2)), .SDcols = to.format2]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.GWP, "/GWPcumFlux.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

###
rm(mydata)