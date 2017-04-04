# output files
folder.out <- paste0(output_path, "/WFPS/WFPSvsN_cumEvent")
dir.create(folder.out)

# input files
folder.in <- paste0(output_path, "/cum_fluxes_full")
cum.flux <- paste0(folder.in, "/every_event_cum_fluxes.dat")
folder.in <- paste0(output_path, "/cum_fluxes_full")

################################################################################################################################################################
################################################################################
# instant flux files
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
Aflux[, incubation:="A"]
Bflux[, incubation:="B"]
Cflux[, incubation:="C"]
Dflux[, incubation:="D"]

flux <- rbindlist(list(Aflux, Bflux, Cflux, Dflux))
flux <- flux[,.(
        days,
        incubation, treatment, fertilizer, precipitation, tillage,
        NO, N2O, CO2, CH4,
        WFPS = WFPS_0
)]
flux[,event:= as.numeric(NA)]
flux[days>0 & days<14,event:= 1]
flux[days>15 & days<29,event:= 2]
flux[days>30 & days<44,event:= 3]
flux <- flux[!is.na(event)]

################################################################################
### get ranks for CO2, NO, N2O
################################################################################
flux[, qCO2:= as.numeric(NA)]
flux[, qNO:= as.numeric(NA)]
flux[, qN2O:= as.numeric(NA)]
flux[, qNON2O:= as.numeric(NA)]
#######################
## ranks per event
setkey(flux, incubation, treatment, event)

# CO2
f <- flux[,.(
        count = sum(!is.na(CO2)),
        rank = rank(CO2, na.last = "keep")
), by= .(incubation, treatment, event)]
f[,myq:= rank/count*100]
flux[,qCO2:= f$myq]

# NO
f <- flux[,.(
        count = sum(!is.na(NO)),
        rank = rank(NO, na.last = "keep")
), by= .(incubation, treatment, event)]
f[,myq:= rank/count*100]
flux[,qNO:= f$myq]

# N2O
f <- flux[,.(
        count = sum(!is.na(N2O)),
        rank = rank(N2O, na.last = "keep")
), by= .(incubation, treatment, event)]
f[,myq:= rank/count*100]
flux[,qN2O:= f$myq]

# NO + N2O
flux[,NON2O:= NO +N2O]
f <- flux[,.(
        count = sum(!is.na(NON2O)),
        rank = rank(NON2O, na.last = "keep")
), by= .(incubation, treatment, event)]
f[,myq:= rank/count*100]
flux[,qNON2O:= f$myq]
################################################################################
# get qCO2 at qN>80
# setkey(flux, incubation, treatment, event, qCO2)

flux[,qCO2.NO80:= qCO2]
flux[qNO<80, qCO2.NO80:=NA]

flux[,qCO2.N2O80:= qCO2]
flux[qN2O<80, qCO2.N2O80:=NA]

flux[,qCO2.NON2O80:= qCO2]
flux[qNON2O<80, qCO2.NON2O80:=NA]

# absolute values
flux[,CO2.NO80:= CO2]
flux[qNO<80, CO2.NO80:=NA]

flux[,CO2.N2O80:= CO2]
flux[qN2O<80, CO2.N2O80:=NA]

flux[,CO2.NON2O80:= CO2]
flux[qNON2O<80, CO2.NON2O80:=NA]

################################################################################
setkey(flux, incubation, treatment, event)
flux<- flux[,.(
        WFPS = mean(WFPS, na.rm=T),
        WFPS_rg = quantile(WFPS, probs=0.9, na.rm=T) - quantile(WFPS, probs=0.1, na.rm=T),
        qCO2.NO80 = median(qCO2.NO80, na.rm=T),
        qCO2.N2O80 = median(qCO2.N2O80, na.rm=T),
        qCO2.NON2O80 = median(qCO2.NON2O80, na.rm=T),
        
        CO2.NO80 = median(CO2.NO80, na.rm=T),
        CO2.N2O80 = median(CO2.N2O80, na.rm=T),
        CO2.NON2O80 = median(CO2.NON2O80, na.rm=T)
), by=.(incubation, treatment, event)]
flux.inc <- copy(flux)
flux<- flux[,.(
        WFPS = mean(WFPS, na.rm=T),
        WFPS_se = sd(WFPS, na.rm=T)/sqrt(sum(!is.na(WFPS))),
        WFPS_rg = mean(WFPS_rg, na.rm=T),
        WFPS.rg_se = sd(WFPS_rg, na.rm=T)/sqrt(sum(!is.na(WFPS_rg)))
), by=.(treatment, event)]

################################################################################################################################################################
################################################################################
cum <- fread(cum.flux)
cum <- cum[days %in% c(14,29,44)]

cum.a <- cum[,.(
        days,
        incubation = "A",
        treatment, fertilizer, precipitation, tillage,
        NO = aNO,
        N2O = aN2O,
        CO2 = aCO2,
        CH4 = aCH4
)]

cum.b <- cum[,.(
        days,
        incubation = "B",
        treatment, fertilizer, precipitation, tillage,
        NO = bNO,
        N2O = bN2O,
        CO2 = bCO2,
        CH4 = bCH4
)]

cum.c <- cum[,.(
        days,
        incubation = "C",
        treatment, fertilizer, precipitation, tillage,
        NO = cNO,
        N2O = cN2O,
        CO2 = cCO2,
        CH4 = cCH4
)]

cum.d <- cum[,.(
        days,
        incubation = "D",
        treatment, fertilizer, precipitation, tillage,
        NO = dNO,
        N2O = dN2O,
        CO2 = dCO2,
        CH4 = dCH4
)]

cum <- rbindlist(list(cum.a, cum.b, cum.c, cum.d))
cum[,event:= as.numeric(NA)]
cum[days==14,event:= 1]
cum[days==29,event:= 2]
cum[days==44,event:= 3]

cum[,NO:=NO/14/24*1000]
cum[,N2O:=N2O/14/24*1000]
cum[,CO2:=CO2/14/24*1000]
cum[,CH4:=CH4/14/24*1000]


cum[,NOtoN2O:= NO/N2O]
cum[,N2OtoNO:= N2O/NO]

cum[,NOtoN:= NO/(NO + N2O)]
cum[,N2OtoN:= N2O/(NO + N2O)]

cum.inc <- copy(cum)

cum<- cum[,.(
        NOtoN = mean(NOtoN, na.rm=T),
        NOtoN.se = sqrt( mean(NOtoN, na.rm=T) * (1-mean(NOtoN, na.rm=T)) / sum(!is.na(NOtoN))),
        NOtoN.rg = quantile(NOtoN, probs=0.9, na.rm=T) - quantile(NOtoN, probs=0.1, na.rm=T),
        
        N2OtoN = mean(N2OtoN, na.rm=T),
        N2OtoN.se = sqrt( mean(N2OtoN, na.rm=T) * (1-mean(N2OtoN, na.rm=T)) / sum(!is.na(N2OtoN))),
        N2OtoN.rg = quantile(N2OtoN, probs=0.9, na.rm=T) - quantile(N2OtoN, probs=0.1, na.rm=T)
), by=.(treatment, event)]
# cum<- cum[,.(
#         NOtoN2O = mean(NOtoN2O, na.rm=T),
#         NOtoN2O.se = sqrt( mean(NOtoN2O, na.rm=T) * (1-mean(NOtoN2O, na.rm=T)) / sum(!is.na(NOtoN2O))),
#         NOtoN2O.rg = quantile(NOtoN2O, probs=0.9, na.rm=T) - quantile(NOtoN2O, probs=0.1, na.rm=T),
#         
#         N2OtoNO = mean(N2OtoNO, na.rm=T),
#         N2OtoNO.se = sqrt(mean(N2OtoNO, na.rm=T) * (1-mean(N2OtoNO, na.rm=T)) / sum(!is.na(N2OtoNO))),
#         N2OtoNO.rg = quantile(N2OtoNO, probs=0.9, na.rm=T) - quantile(N2OtoNO, probs=0.1, na.rm=T)
# ), by=.(treatment, event)]


################################################################################################################################################################
################################################################################
# add other N forms for a N mass-balance or other comparisons

folder.in <- paste0(output_path, "/N_balance")
Nbal <- paste0(folder.in, "/N_balance_kgNha.dat")
Nbal <- fread(Nbal)

Nbal[,NO:=NULL]
Nbal[,N2O:=NULL]
Nbal[,CO2:=NULL]
Nbal[,CH4:=NULL]
Nbal[,fertilization:=NULL]
Nbal[,precipitation:=NULL]
Nbal[,tillage:=NULL]

Nbal[,nitrate:= i.NO3 + added.NO3]
Nbal[,ammonium:= i.NH4 + added.NH4]
# with(Nbal,boxplot(i.NO3))

Nbal.inc <- copy(Nbal) 

################################################################################################################################################################
################################################################################
#######
setkey(cum, treatment, event)
setkey(flux, treatment, event)

setkey(cum.inc, treatment, event, incubation)
setkey(flux.inc, treatment, event, incubation)

data <- cum[flux]
data.inc <- cum.inc[flux.inc]

setkey(data.inc, treatment, incubation)
setkey(Nbal.inc, treatment, incubation)
data.inc <- data.inc[Nbal.inc]
################################################################################################################################################################
################################################################################
### get detection limits for 14 days and 14*3 days
# NO (0.5 µg-N/m2/h)
dl.NO.14 <- 14*24*0.5/1000
dl.NO.44 <- 44*24*0.5/1000

# N2O (1.9 µg-N/m2/h)
dl.N2O.14 <- 14*24*1.9/1000
dl.N2O.44 <- 44*24*1.9/1000

##
data.inc[NO<dl.NO.14]
data.inc[N2O<dl.N2O.14]
data.inc[NO<dl.NO.14 | N2O<dl.N2O.14]

data.inc.rm <- data.inc[!(NO<dl.NO.14 | N2O<dl.N2O.14)]


# ammount of data deleted if we removed low values
nrow(data.inc[NO<dl.NO.14 | N2O<dl.N2O.14]) / nrow(data.inc)

################################################################################
###
with(data, boxplot(NOtoN.se))
with(data, boxplot(NOtoN.rg))

with(data, plot(NOtoN~WFPS))
with(data.inc, plot(NOtoN~WFPS, pch=19))
with(data.inc, plot(NOtoN2O~WFPS, pch=19))
with(data.inc, plot(log10(N2OtoNO)~WFPS, pch=19))
with(data.inc, plot(log10(NOtoN2O)~WFPS, pch=19))


###
with(data.inc, boxplot(log(NO)))
with(data.inc, boxplot(log(N2O)))

with(data.inc, boxplot(NO))
with(data.inc[NO<0.2], boxplot(NO))
data.inc[NO<0.1]
with(data.inc, boxplot(log(NO)))

with(data.inc, boxplot(N2O))
with(data.inc[N2O<10], boxplot(N2O))
with(data.inc[N2O<2], boxplot(N2O))
with(data.inc[N2O<1], boxplot(N2O))
with(data.inc[N2O<0.2], boxplot(N2O))

with(data.inc[event==1], plot(log10(N2OtoNO)~WFPS, pch=19))
with(data.inc[event==2], plot(log10(N2OtoNO)~WFPS, pch=19))
with(data.inc[event==3], plot(log10(N2OtoNO)~WFPS, pch=19))

with(data.inc, plot(CO2~log10(NO+N2O), pch=19))
with(data.inc, plot(CO2~log10(NO), pch=19))
with(data.inc, plot(CO2~log10(N2O), pch=19))

with(data.inc.rm, plot(CO2~log10(NO+N2O), pch=19))
with(data.inc[NO>dl.NO.14,], plot(CO2~log10(NO), pch=19))
with(data.inc[N2O>dl.N2O.14,], plot(CO2~log10(N2O), pch=19))


with(data.inc[event==1], plot(CO2~log10(NO+N2O), pch=19))
with(data.inc[event==2], plot(CO2~log10(NO+N2O), pch=19))
with(data.inc[event==3], plot(CO2~log10(NO+N2O), pch=19))
with(data.inc[event==1], plot(CO2~log10(NO+N2O), pch=19))
with(data.inc[event==1], plot(CO2~log10(NO), pch=19))
with(data.inc[event==1], plot(CO2~log10(N2O), pch=19))

with(data.inc, plot(qCO2.NON2O80~log10(NO+N2O), pch=19))
with(data.inc, plot(qCO2.NON2O80~log10(NO+N2O), pch=19))
with(data.inc, plot(qCO2.NO80~log10(NO), pch=19))
with(data.inc, plot(qCO2.N2O80~log10(N2O), pch=19))

with(data.inc[event==1], plot(qCO2.NON2O80~log10(NO+N2O), pch=19))
with(data.inc[event==1], plot(qCO2.NO80~log10(NO), pch=19))
with(data.inc[event==1], plot(qCO2.N2O80~log10(N2O), pch=19))

with(data.inc, plot(CO2~log(NO+N2O), pch=19))
with(data.inc, plot(CO2~log(NO), pch=19))
with(data.inc, plot(CO2~log(N2O), pch=19))


##################################
data.inc[,myevent:=as.character(NA)]
data.inc[event==1,myevent:="1st event"]
data.inc[event %in% c(2,3),myevent:="2nd and 3rd events"]

data.inc.rm[,myevent:=as.character(NA)]
data.inc.rm[event==1,myevent:="1st event"]
data.inc.rm[event %in% c(2,3),myevent:="2nd and 3rd events"]

setkey(data.inc, fertilizer, precipitation, tillage, incubation)
col.rain <- c("grey", "deepskyblue","dodgerblue4")
col.fert <- c("grey","olivedrab2", "olivedrab4")
col.till <- c("black","red")

# col.event <- c("#AC391E","#AC691E", "#AC841E")
col.event <- c("black", "grey")
col.inc <- c("grey20", "grey40", "grey60", "grey80")
#######################################
#######################################
g.0 <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.0 <- (g.0
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab("WFPS [%]")
        + ylab(expression('N'[2]*'O / NO'))
        # + scale_x_continuous(breaks = c(0, 15, 30, 45))
        # + coord_cartesian(xlim=c(0,100))
        + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x)))
        + annotation_logticks(sides = "lr")
)

g.0 <- (g.0
        # + facet_grid(precipitation ~ tillage)
        + geom_point(aes())
        + geom_hline(yintercept=1, col="red")
        + scale_colour_manual(values=col.fert)
        + geom_smooth(method='lm',formula=y~x, se=FALSE, col="red", size=0.75)
        #         + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        #         + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.0
myplot <- paste0(folder.out, "/NO_to_N2O_vs_WFPS.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.0)
dev.off()

#######################################
g.fert <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.fert <- (g.fert
           + theme_bw(base_size = 8)
           + theme(axis.ticks = element_line(size = 0.1),
                   legend.position =   "none",
                   strip.background =   element_blank(),
                   plot.title = element_text(hjust = 0)
           )
           + xlab("WFPS [%]")
           + ylab(expression('N'[2]*'O / NO'))
           # + scale_x_continuous(breaks = c(0, 15, 30, 45))
           # + coord_cartesian(ylim=c(0,1500))
           + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                           labels = trans_format("log10", math_format(10^.x)))
           + annotation_logticks(sides = "lr")
)

g.fert <- (g.fert
           # + facet_grid(precipitation ~ tillage)
           + geom_point(aes(colour=factor(fertilizer)))
           + geom_hline(yintercept=1, col="red")
           + scale_colour_manual(values=col.fert)
           + geom_smooth(data= data.inc[fertilizer==0],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.fert[1])
           + geom_smooth(data= data.inc[fertilizer==50],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.fert[2])
           + geom_smooth(data= data.inc[fertilizer==100],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.fert[3])
           #         + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
           #         + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.fert
myplot <- paste0(folder.out, "/NO_to_N2O_vs_WFPS_byfert.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.fert)
dev.off()

#######################################
g.till <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.till <- (g.till
           + theme_bw(base_size = 8)
           + theme(axis.ticks = element_line(size = 0.1),
                   legend.position =   "none",
                   strip.background =   element_blank(),
                   plot.title = element_text(hjust = 0)
           )
           + xlab("WFPS [%]")
           + ylab(expression('N'[2]*'O / NO'))
           # + scale_x_continuous(breaks = c(0, 15, 30, 45))
           # + coord_cartesian(ylim=c(0,1500))
           + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                           labels = trans_format("log10", math_format(10^.x)))
           + annotation_logticks(sides = "lr")
)

g.till <- (g.till
           # + facet_grid(precipitation ~ tillage)
           + geom_point(aes(colour=factor(tillage)), size=1.5)
           + geom_hline(yintercept=1, col="red")
           + scale_colour_manual(values=col.till)
           + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
           
           + geom_smooth(data= data.inc[tillage=="NT"],method='lm',formula=y~x, se=F, size=0.75, color=col.till[1])
           + geom_smooth(data= data.inc[tillage=="TT"],method='lm',formula=y~x, se=F, size=0.75, color=col.till[2],fill="red", alpha=0.15)
           #         + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
           #         + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.till
myplot <- paste0(folder.out, "/NO_to_N2O_vs_WFPS_bytill.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.till)
dev.off()
#######################################
g.till.SE <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.till.SE <- (g.till.SE
              + theme_bw(base_size = 8)
              + theme(axis.ticks = element_line(size = 0.1),
                      legend.position =   "none",
                      strip.background =   element_blank(),
                      plot.title = element_text(hjust = 0)
              )
              + xlab("WFPS [%]")
              + ylab(expression('N'[2]*'O / NO'))
              # + scale_x_continuous(breaks = c(0, 15, 30, 45))
              + coord_cartesian(ylim=c(0.011,2000))
              + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", math_format(10^.x)))
              + annotation_logticks(sides = "lr")
)

g.till.SE <- (g.till.SE
              # + facet_grid(precipitation ~ tillage)
              + geom_point(aes(colour=factor(tillage)), size=1.5)
              + geom_hline(yintercept=1, col="red")
              + scale_colour_manual(values=col.till)
              + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
              
              + geom_smooth(data= data.inc[tillage=="NT"],method='lm',formula=y~x, se=TRUE, size=0.75, color=col.till[1])
              + geom_smooth(data= data.inc[tillage=="TT"],method='lm',formula=y~x, se=TRUE, size=0.75, color=col.till[2],fill="red", alpha=0.15)
              
              + annotate("text", x = 25, y = 500, label = "traditional tillage", size=2.5, color="black", hjust= 0)
              + annotate("point", x = 20, y = 500, label = "no tillage", color="red")
              + annotate("text", x = 25, y = 200, label = "no tillage", size=2.5, hjust= 0)
              + annotate("point", x = 20, y = 200, label = "no tillage")
              
              #         + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
              #         + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.till.SE
myplot <- paste0(folder.out, "/NO_to_N2O_vs_WFPS_bytill_SE.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.till.SE)
dev.off()
#######################################
g.rain <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.rain <- (g.rain
           + theme_bw(base_size = 8)
           + theme(axis.ticks = element_line(size = 0.1),
                   legend.position =   "none",
                   strip.background =   element_blank(),
                   plot.title = element_text(hjust = 0)
           )
           + xlab("WFPS [%]")
           + ylab(expression('N'[2]*'O / NO'))
           # + scale_x_continuous(breaks = c(0, 15, 30, 45))
           # + coord_cartesian(ylim=c(0,1500))
           + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                           labels = trans_format("log10", math_format(10^.x)))
           + annotation_logticks(sides = "lr")
)

g.rain <- (g.rain
           # + facet_grid(precipitation ~ tillage)
           + geom_point(aes(colour=factor(precipitation)))
           + geom_hline(yintercept=1, col="red")
           + scale_colour_manual(values=col.rain)
           + geom_smooth(data= data.inc[precipitation=="c"],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.rain[1])
           + geom_smooth(data= data.inc[precipitation=="i"],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.rain[3])
           + geom_smooth(data= data.inc[precipitation=="d"],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.rain[2])
           #         + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
           #         + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.rain
myplot <- paste0(folder.out, "/NO_to_N2O_vs_WFPS_byrain.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.rain)
dev.off()

#######################################
g.event <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.event <- (g.event
            + theme_bw(base_size = 8)
            + theme(axis.ticks = element_line(size = 0.1),
                    legend.position =   "none",
                    strip.background =   element_blank(),
                    plot.title = element_text(hjust = 0)
            )
            + xlab("WFPS [%]")
            + ylab(expression('N'[2]*'O / NO'))
            # + scale_x_continuous(breaks = c(0, 15, 30, 45))
            # + coord_cartesian(ylim=c(0,1500))
            + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                            labels = trans_format("log10", math_format(10^.x)))
            + annotation_logticks(sides = "lr")
)

g.event <- (g.event
            # + facet_grid(precipitation ~ tillage)
            + geom_point(aes(color=factor(myevent)), size=1.5)
            + geom_hline(yintercept=1, col="red")
            + scale_color_manual(values=col.event)
            
            + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
            
            + geom_smooth(data= data.inc[event==1],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.event[1])
            + geom_smooth(data= data.inc[event %in% c(2,3)],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.event[2])
            
            #         + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
            #         + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.event
myplot <- paste0(folder.out, "/NO_to_N2O_vs_WFPS_byevent.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.event)
dev.off()

#######################################
g.event.SE <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.event.SE <- (g.event.SE
               + theme_bw(base_size = 8)
               + theme(axis.ticks = element_line(size = 0.1),
                       legend.position =   "none",
                       strip.background =   element_blank(),
                       plot.title = element_text(hjust = 0)
               )
               + xlab("WFPS [%]")
               + ylab(expression('N'[2]*'O / NO'))
               # + scale_x_continuous(breaks = c(0, 15, 30, 45))
               + coord_cartesian(ylim=c(0.011,2000))
               + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x)))
               + annotation_logticks(sides = "lr")
)

g.event.SE <- (g.event.SE
               # + facet_grid(precipitation ~ tillage)
               + geom_point(aes(color=factor(myevent)), size=1.5)
               + geom_hline(yintercept=1, col="red")
               + scale_color_manual(values=col.event)
               
               
               
               + geom_smooth(data= data.inc[event==1],method='lm',formula=y~x, se=T, size=0.75, color=col.event[1], alpha=0.25)
               + geom_smooth(data= data.inc[event %in% c(2,3)],method='lm',formula=y~x, se=T, size=0.75, color=col.event[2], alpha=0.25)
               
               + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
               
               + annotate("text", x = 25, y = 500, label = "1st incubation-stretch", size=2.5, color="black", hjust= 0)
               + annotate("point", x = 20, y = 500, color="black")
               + annotate("text", x = 25, y = 200, label = "2nd & 3rd", color="black", size=2.5, hjust= 0)
               + annotate("point", x = 20, y = 200, color="grey")
               
               # + geom_text(data=labels, aes(x=x, y=y, label=label), hjust= -0.1, vjust=0, size=2, col= "grey50")
               
               #         + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
               #         + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.event.SE
myplot <- paste0(folder.out, "/NO_to_N2O_vs_WFPS_byevent_SE.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.event.SE)
dev.off()
#######################################
g.inc <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.inc <- (g.inc
          + theme_bw(base_size = 8)
          + theme(axis.ticks = element_line(size = 0.1),
                  legend.position =   "none",
                  strip.background =   element_blank(),
                  plot.title = element_text(hjust = 0)
          )
          + xlab("WFPS [%]")
          + ylab(expression('N'[2]*'O / NO'))
          # + scale_x_continuous(breaks = c(0, 15, 30, 45))
          # + coord_cartesian(ylim=c(0,1500))
          + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x)))
          + annotation_logticks(sides = "lr")
)

g.inc <- (g.inc
          # + facet_grid(precipitation ~ tillage)
          + geom_point(aes(colour=factor(incubation)))
          + geom_hline(yintercept=1, col="red")
          + scale_colour_manual(values=col.inc)
          + geom_smooth(data= data.inc[incubation=="A"],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.inc[1])
          + geom_smooth(data= data.inc[incubation=="B"],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.inc[2])
          + geom_smooth(data= data.inc[incubation=="C"],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.inc[3])
          + geom_smooth(data= data.inc[incubation=="D"],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.inc[4])
          
          + annotate("text", x = 25, y = 500, label = "A incubation", size=2.5, hjust= 0)
          + annotate("point", x = 20, y = 500, color=col.inc[1])
          + annotate("text", x = 25, y = 200, label = "D incubation", color="black", size=2.5, hjust= 0)
          + annotate("point", x = 20, y = 200, color = col.inc[4])
)
g.inc
myplot <- paste0(folder.out, "/NO_to_N2O_vs_WFPS_byINC.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.inc)
dev.off()

#######################################
boxplot(data.inc$CO2)
data.inc[,CO2:=CO2/14/24*1000]
qCO2 <- quantile(data.inc$CO2, probs= c(0,1/3,2/3,1))
data.inc[,qCO2:=as.character(NA)]
data.inc[CO2 < 10, qCO2:= "low"]
data.inc[CO2 > 26 & CO2 < 51, qCO2:="medium"]
data.inc[CO2 > 83, qCO2:="up"]

setkey(data.inc, fertilizer, precipitation, tillage, incubation, qCO2)
g.C <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.C <- (g.C
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   c(0.2 ,0.85),
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab("WFPS [%]")
        + ylab(expression('N'[2]*'O / NO'))
        # + scale_x_continuous(breaks = c(0, 15, 30, 45))
        # + coord_cartesian(ylim=c(0,1500))
        + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x)))
        + annotation_logticks(sides = "lr")
)

g.C <- (g.C
        # + facet_grid(precipitation ~ tillage)
        + geom_point(aes(size=CO2), shape=1)
        + geom_hline(yintercept=1, col="red")
#                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
#                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
        
)
g.C
myplot <- paste0(folder.out, "/NO_to_N2O_vs_WFPS_byCO2.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.C)
dev.off()
#######################################
g.NOvsN2O <- ggplot(data.inc, aes(x= NO, y = N2O))
g.NOvsN2O <- (g.NOvsN2O
              + theme_bw(base_size = 8)
              + theme(axis.ticks = element_line(size = 0.1),
                      legend.position =   c(0.2 ,0.85),
                      strip.background =   element_blank(),
                      plot.title = element_text(hjust = 0)
              )
              + xlab(expression('NO [ µg-N m'^-2*'h'^-1*' ]'))
              + ylab(expression('N'[2]*'O [ µg-N m'^-2*'h'^-1*' ]'))
              # + scale_x_continuous(breaks = c(0, 15, 30, 45))
              # + coord_cartesian(ylim=c(0,1500))
              + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", math_format(10^.x)))
              + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", math_format(10^.x)))
              + annotation_logticks(sides = "trbl")
)

g.NOvsN2O <- (g.NOvsN2O
              # + facet_grid(precipitation ~ tillage)
              # + geom_point(aes(size=WFPS))
              + geom_abline(intercept=0, slope=1, linetype=2)
              + geom_point()
              # + geom_hline(yintercept=1, col="red")
              # + scale_colour_manual(values=col.inc)
              # + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
              # + geom_smooth(data= data.inc[qCO2=="medium"],method='lm',formula=y~x, se=FALSE, size=0.75, linetype = 2)
              # + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
              #           + geom_smooth(data= data.inc[incubation=="B"],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.inc[2])
              #           + geom_smooth(data= data.inc[incubation=="C"],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.inc[3])
              #           + geom_smooth(data= data.inc[incubation=="D"],method='lm',formula=y~x, se=FALSE, size=0.75, color=col.inc[4])
              
              #           + annotate("text", x = 25, y = 500, label = "A incubation", size=2.5, hjust= 0)
              #           + annotate("point", x = 20, y = 500, color=col.inc[1])
              #           + annotate("text", x = 25, y = 200, label = "D incubation", color="black", size=2.5, hjust= 0)
              #           + annotate("point", x = 20, y = 200, color = col.inc[4])
)
g.NOvsN2O

g.NOvsN2O.C <- ggplot(data.inc, aes(x= NO, y = N2O))
g.NOvsN2O.C <- (g.NOvsN2O.C
                + theme_bw(base_size = 8)
                + theme(axis.ticks = element_line(size = 0.1),
                        legend.position =   c(0.1 ,0.85),
                        strip.background =   element_blank(),
                        plot.title = element_text(hjust = 0)
                )
                + xlab(expression('NO [ µg-N m'^-2*'h'^-1*' ]'))
                + ylab(expression('N'[2]*'O [ µg-N m'^-2*'h'^-1*' ]'))
                # + scale_x_continuous(breaks = c(0, 15, 30, 45))
                # + coord_cartesian(ylim=c(0,1500))
                + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
                + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
                + annotation_logticks(sides = "trbl")
)

g.NOvsN2O.C <- (g.NOvsN2O.C
                + geom_abline(intercept=0, slope=1, linetype=2)
                + geom_point(aes(size=CO2))
)
g.NOvsN2O.C

myplot <- paste0(folder.out, "/N2O_vs_NO.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.NOvsN2O, vp = vplayout(1, 1))
print(g.NOvsN2O.C, vp = vplayout(1, 2)) #see plot_results_pressEvolution.R

dev.off()

################################################################################
myplot <- paste0(folder.out, "/NO_to_N2O_vs_WFPS_compilation_till_event_SE.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.till.SE, vp = vplayout(1, 1))
print(g.event.SE, vp = vplayout(1, 2)) #see plot_results_pressEvolution.R

dev.off()

################################################################################
################################################################################
g.NO <- ggplot(data.inc, aes(x= WFPS, y = NO))
g.NO <- (g.NO
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   c(0.2 ,0.25),
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab("WFPS [%]")
        + ylab(expression('NO [ µg-N m'^2*'h'^-1*' ]'))
        # + scale_x_continuous(breaks = c(0, 15, 30, 45))
        + coord_cartesian(ylim=c(0.1,1500))
        + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x)))
        + annotation_logticks(sides = "lr")
)

g.NO <- (g.NO
        # + facet_grid(precipitation ~ tillage)
        # + geom_point(aes(size=ammonium))
        + geom_point(size=1.5)
                        + geom_smooth(method='lm',formula=y~x, se=F, size=0.75, linetype = 1, col="red")
        #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
        
)
g.NO

###
g.N2O <- ggplot(data.inc, aes(x= WFPS, y = N2O))
g.N2O <- (g.N2O
         + theme_bw(base_size = 8)
         + theme(axis.ticks = element_line(size = 0.1),
                 legend.position =   c(0.2 ,0.25),
                 strip.background =   element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + xlab("WFPS [%]")
         + ylab(expression('N'[2]*'O [ µg-N m'^2*'h'^-1*' ]'))
         # + scale_x_continuous(breaks = c(0, 15, 30, 45))
         + coord_cartesian(ylim=c(0.1,1500))
         + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                         labels = trans_format("log10", math_format(10^.x)))
         + annotation_logticks(sides = "lr")
)

g.N2O <- (g.N2O
         # + facet_grid(precipitation ~ tillage)
         # + geom_point(aes(size=ammonium))
         + geom_point(size=1.5)
         + geom_smooth(method='lm',formula=y~x, se=F, size=0.75, linetype = 1, col="red")
         #                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
         #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
         
)
g.N2O

###
g.NO.N2O <- ggplot(data.inc, aes(x= WFPS, y = N2O+NO))
g.NO.N2O <- (g.NO.N2O
          + theme_bw(base_size = 8)
          + theme(axis.ticks = element_line(size = 0.1),
                  legend.position =   c(0.2 ,0.25),
                  strip.background =   element_blank(),
                  plot.title = element_text(hjust = 0)
          )
          + xlab("WFPS [%]")
          + ylab(expression('NO + N'[2]*'O [ µg-N m'^2*'h'^-1*' ]'))
          # + scale_x_continuous(breaks = c(0, 15, 30, 45))
          + coord_cartesian(ylim=c(0.1,1500))
          + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x)))
          + annotation_logticks(sides = "lr")
)

g.NO.N2O <- (g.NO.N2O
          # + facet_grid(precipitation ~ tillage)
          # + geom_point(aes(size=ammonium))
          + geom_point(size=1.5)
          + geom_smooth(method='lm',formula=y~x, se=F, size=0.75, linetype = 1, col="red")
          #                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
          #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
          
)
g.NO.N2O

################################################################################
myplot <- paste0(folder.out, "/N_vs_WFPS_compilation.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.NO, vp = vplayout(1, 1))
print(g.N2O, vp = vplayout(1, 2))
print(g.NO.N2O, vp = vplayout(1, 3))

dev.off()

###
################################################################################
g.NO <- ggplot(data.inc, aes(x= WFPS, y = NO))
g.NO <- (g.NO
         + theme_bw(base_size = 8)
         + theme(axis.ticks = element_line(size = 0.1),
                 plot.margin= unit(c(0.1, 0.25, -0.25, 0.1), "lines"),
                 axis.text.x =        element_blank(),
                 legend.position =   c(0.2 ,0.25),
                 strip.background =   element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + xlab("")
         + ylab(expression('NO [ µg-N m'^2*'h'^-1*' ]'))
         # + scale_x_continuous(breaks = c(0, 15, 30, 45))
         # + coord_cartesian(ylim=c(0,1500))
         + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                         labels = trans_format("log10", math_format(10^.x)))
         + annotation_logticks(sides = "lr")
)

g.NO <- (g.NO
         # + facet_grid(precipitation ~ tillage)
         # + geom_point(aes(size=ammonium))
         + geom_point()
         + geom_smooth(method='lm',formula=y~x, se=T, size=0.75, linetype = 1, col="red")
         #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
         
)
g.NO

###
g.N2O <- ggplot(data.inc, aes(x= WFPS, y = N2O))
g.N2O <- (g.N2O
          + theme_bw(base_size = 8)
          + theme(axis.ticks = element_line(size = 0.1),
                  plot.margin= unit(c(-0.25, 0.25, -0.25, 0.1), "lines"),
                  axis.text.x =        element_blank(),
                  legend.position =   c(0.2 ,0.25),
                  strip.background =   element_blank(),
                  plot.title = element_text(hjust = 0)
          )
          + xlab("")
          + ylab(expression('N'[2]*'O [ µg-N m'^2*'h'^-1*' ]'))
          # + scale_x_continuous(breaks = c(0, 15, 30, 45))
          # + coord_cartesian(ylim=c(0,1500))
          + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x)))
          + annotation_logticks(sides = "lr")
)

g.N2O <- (g.N2O
          # + facet_grid(precipitation ~ tillage)
          # + geom_point(aes(size=ammonium))
          + geom_point()
          + geom_smooth(method='lm',formula=y~x, se=T, size=0.75, linetype = 1, col="red")
          #                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
          #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
          
)
g.N2O

###
g.NO.N2O <- ggplot(data.inc, aes(x= WFPS, y = N2O+NO))
g.NO.N2O <- (g.NO.N2O
             + theme_bw(base_size = 8)
             + theme(axis.ticks = element_line(size = 0.1),
                     plot.margin= unit(c(0.1, 0.25, 0.1, 0.1), "lines"),
                     legend.position =   c(0.2 ,0.25),
                     strip.background =   element_blank(),
                     plot.title = element_text(hjust = 0)
             )
             + xlab("WFPS [%]")
             + ylab(expression('NO + N'[2]*'O [ µg-N m'^2*'h'^-1*' ]'))
             # + scale_x_continuous(breaks = c(0, 15, 30, 45))
             # + coord_cartesian(ylim=c(0,1500))
             + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                             labels = trans_format("log10", math_format(10^.x)))
             + annotation_logticks(sides = "lr")
)

g.NO.N2O <- (g.NO.N2O
             # + facet_grid(precipitation ~ tillage)
             # + geom_point(aes(size=ammonium))
             + geom_point()
             + geom_smooth(method='lm',formula=y~x, se=T, size=0.75, linetype = 1, col="red")
             #                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
             #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
             
)
g.NO.N2O

################################################################################
myplot <- paste0(folder.out, "/N_vs_WFPS_compilation_vert.png")
png(filename=myplot,  width = 50, height = 150, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3,1)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.NO, vp = vplayout(1, 1))
print(g.N2O, vp = vplayout(2, 1))
print(g.NO.N2O, vp = vplayout(3, 1))

dev.off()

################################################################################################################################################################
g.0 <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.0 <- (g.0
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   c(0.2 ,0.75),
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab("WFPS [%]")
        + ylab(expression('N'[2]*'O / NO'))
        # + scale_x_continuous(breaks = c(0, 15, 30, 45))
        # + coord_cartesian(ylim=c(0,1500))
        + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x)))
        + annotation_logticks(sides = "lr")
)

g.0 <- (g.0
        # + facet_grid(precipitation ~ tillage)
        + geom_point()
        + geom_hline(yintercept=1, col="red")
        + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
        #                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
        #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
        
)
g.0
###
g.C <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.C <- (g.C
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   c(0.2 ,0.85),
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab("WFPS [%]")
        + ylab(expression('N'[2]*'O / NO'))
        # + scale_x_continuous(breaks = c(0, 15, 30, 45))
        # + coord_cartesian(ylim=c(0,1500))
        + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x)))
        + annotation_logticks(sides = "lr")
)

g.C <- (g.C
        # + facet_grid(precipitation ~ tillage)
        + geom_point(aes(size=CO2), alpha=0.4)
        + geom_hline(yintercept=1, col="red")
        + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
        #                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
        #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
        
)
g.C
###
g.amm <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.amm <- (g.amm
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   c(0.2 ,0.75),
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab("WFPS [%]")
        + ylab(expression('N'[2]*'O / NO'))
        # + scale_x_continuous(breaks = c(0, 15, 30, 45))
        # + coord_cartesian(ylim=c(0,1500))
        + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x)))
        + annotation_logticks(sides = "lr")
)

g.amm <- (g.amm
        # + facet_grid(precipitation ~ tillage)
        + geom_point(aes(size=ammonium), alpha=0.4)
        + geom_hline(yintercept=1, col="red")
        + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
        #                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
        #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
        
)
g.amm

###
g.nit <- ggplot(data.inc, aes(x= WFPS, y = N2OtoNO))
g.nit <- (g.nit
          + theme_bw(base_size = 8)
          + theme(axis.ticks = element_line(size = 0.1),
                  legend.position =   c(0.2 ,0.75),
                  strip.background =   element_blank(),
                  plot.title = element_text(hjust = 0)
          )
          + xlab("WFPS [%]")
          + ylab(expression('N'[2]*'O / NO'))
          # + scale_x_continuous(breaks = c(0, 15, 30, 45))
          # + coord_cartesian(ylim=c(0,1500))
          + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x)))
          + annotation_logticks(sides = "lr")
)

g.nit <- (g.nit
          # + facet_grid(precipitation ~ tillage)
          + geom_point(aes(size=nitrate), alpha=0.4)
          + geom_hline(yintercept=1, col="red")
          + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
          #                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
          #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
          
)
g.nit
###
myplot <- paste0(folder.out, "/N_vs_WFPS_compilation_vs_amm_nit__full.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.amm, vp = vplayout(1, 1))
print(g.nit, vp = vplayout(1, 2))

dev.off()

###
myplot <- paste0(folder.out, "/N_vs_WFPS_compilation_vs_CO2__full.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.0, vp = vplayout(1, 1))
print(g.C, vp = vplayout(1, 2))

dev.off()

###
myplot <- paste0(folder.out, "/N_vs_WFPS_compilation_vs_CO2_amm_nit__full.png")
png(filename=myplot,  width = 160, height = 160, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.0, vp = vplayout(1, 1))
print(g.C, vp = vplayout(1, 2))
print(g.amm, vp = vplayout(2, 1))
print(g.nit, vp = vplayout(2, 2))

dev.off()
################################################################################################################################################################

g.NOvsN2O.0 <- ggplot(data.inc, aes(x= NO, y = N2O))
g.NOvsN2O.0 <- (g.NOvsN2O.0
                + theme_bw(base_size = 8)
                + theme(axis.ticks = element_line(size = 0.1),
                        # legend.position =   c(0.1 ,0.85),
                        strip.background =   element_blank(),
                        plot.title = element_text(hjust = 0)
                )
                + xlab(expression('NO [ µg-N m'^-2*'h'^-1*' ]'))
                + ylab(expression('N'[2]*'O [ µg-N m'^-2*'h'^-1*' ]'))
                # + scale_x_continuous(breaks = c(0, 15, 30, 45))
                # + coord_cartesian(ylim=c(0,1500))
                + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
                + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
                + annotation_logticks(sides = "trbl")
)

g.NOvsN2O.0 <- (g.NOvsN2O.0
                + geom_abline(intercept=0, slope=1, linetype=2)
                + geom_point(aes())
)
g.NOvsN2O.0
###
g.NOvsN2O.C <- ggplot(data.inc, aes(x= NO, y = N2O))
g.NOvsN2O.C <- (g.NOvsN2O.C
                + theme_bw(base_size = 8)
                + theme(axis.ticks = element_line(size = 0.1),
                        legend.position =   "none",
                        strip.background =   element_blank(),
                        plot.title = element_text(hjust = 0)
                )
                + xlab(expression('NO [ µg-N m'^-2*'h'^-1*' ]'))
                + ylab(expression('N'[2]*'O [ µg-N m'^-2*'h'^-1*' ]'))
                # + scale_x_continuous(breaks = c(0, 15, 30, 45))
                # + coord_cartesian(ylim=c(0,1500))
                + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
                + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
                + annotation_logticks(sides = "trbl")
)

g.NOvsN2O.C <- (g.NOvsN2O.C
                + geom_abline(intercept=0, slope=1, linetype=2)
                + geom_point(aes(size=CO2), shape=1)
)
g.NOvsN2O.C
###
g.NOvsN2O.amm <- ggplot(data.inc, aes(x= NO, y = N2O))
g.NOvsN2O.amm <- (g.NOvsN2O.amm
                + theme_bw(base_size = 8)
                + theme(axis.ticks = element_line(size = 0.1),
                        legend.position =   "none",
                        strip.background =   element_blank(),
                        plot.title = element_text(hjust = 0)
                )
                + xlab(expression('NO [ µg-N m'^-2*'h'^-1*' ]'))
                + ylab(expression('N'[2]*'O [ µg-N m'^-2*'h'^-1*' ]'))
                # + scale_x_continuous(breaks = c(0, 15, 30, 45))
                # + coord_cartesian(ylim=c(0,1500))
                + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
                + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
                + annotation_logticks(sides = "trbl")
)

g.NOvsN2O.amm <- (g.NOvsN2O.amm
                + geom_abline(intercept=0, slope=1, linetype=2)
                + geom_point(aes(size=ammonium), shape=1)
)
g.NOvsN2O.amm
###
g.NOvsN2O.nit <- ggplot(data.inc, aes(x= NO, y = N2O))
g.NOvsN2O.nit <- (g.NOvsN2O.nit
                + theme_bw(base_size = 8)
                + theme(axis.ticks = element_line(size = 0.1),
                        legend.position =   "none",
                        strip.background =   element_blank(),
                        plot.title = element_text(hjust = 0)
                )
                + xlab(expression('NO [ µg-N m'^-2*'h'^-1*' ]'))
                + ylab(expression('N'[2]*'O [ µg-N m'^-2*'h'^-1*' ]'))
                # + scale_x_continuous(breaks = c(0, 15, 30, 45))
                # + coord_cartesian(ylim=c(0,1500))
                + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
                + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                labels = trans_format("log10", math_format(10^.x)))
                + annotation_logticks(sides = "trbl")
)

g.NOvsN2O.nit <- (g.NOvsN2O.nit
                + geom_abline(intercept=0, slope=1, linetype=2)
                + geom_point(aes(size=nitrate), shape=1)
)
g.NOvsN2O.nit
###
###
myplot <- paste0(folder.out, "/NO_vs_N2O_vs_amm_nit__open.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.amm, vp = vplayout(1, 1))
print(g.nit, vp = vplayout(1, 2))

dev.off()

###
myplot <- paste0(folder.out, "/NO_vs_N2O_vs_CO2__open.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.NOvsN2O.0, vp = vplayout(1, 1))
print(g.NOvsN2O.C, vp = vplayout(1, 2))

dev.off()

###
myplot <- paste0(folder.out, "/NO_vs_N2O_vs_CO2_amm_nit__open.png")
png(filename=myplot,  width = 160, height = 160, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.NOvsN2O.0, vp = vplayout(1, 1))
print(g.NOvsN2O.C, vp = vplayout(1, 2))
print(g.NOvsN2O.amm, vp = vplayout(2, 1))
print(g.NOvsN2O.nit, vp = vplayout(2, 2))

dev.off()



################################################################################
################################################################################
################################################################################
################################################################################
# monography till, event, CO2, ammonium
################################################################################
#######################################
g.till.SE <- ggplot(data.inc[!116:118], aes(x= WFPS, y = N2OtoNO))
g.till.SE <- (g.till.SE
              + theme_bw(base_size = 8)
              + theme(axis.ticks = element_line(size = 0.1),
                      axis.text.x = element_blank(),
                      plot.margin= unit(c(0, 0.25, 0, 0.1), "lines"),
                      legend.position =   "none",
                      strip.background =   element_blank(),
                      plot.title = element_text(hjust = 0)
              )
              # + xlab("WFPS [%]")
              + xlab("")
              + ylab(expression('N'[2]*'O / NO'))
              # + scale_x_continuous(breaks = c(0, 15, 30, 45))
              + coord_cartesian(ylim=c(0.011,2000))
              + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                              labels = trans_format("log10", math_format(10^.x)))
              + annotation_logticks(sides = "lr")
)

g.till.SE <- (g.till.SE
              # + facet_grid(precipitation ~ tillage)
              + geom_point(aes(colour=factor(tillage)), size=1.5)
              + geom_hline(yintercept=1, col="red")
              + scale_colour_manual(values=col.till)
              + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
              
              + geom_smooth(data= data.inc[tillage=="NT"],method='lm',formula=y~x, se=TRUE, size=0.75, color=col.till[1])
              + geom_smooth(data= data.inc[tillage=="TT"],method='lm',formula=y~x, se=TRUE, size=0.75, color=col.till[2],fill="red", alpha=0.15)
              
              + annotate("text", x = 25, y = 500, label = "traditional tillage", size=2.5, color="black", hjust= 0)
              + annotate("point", x = 20, y = 500, label = "no tillage", color="red")
              + annotate("text", x = 25, y = 200, label = "no tillage", size=2.5, hjust= 0)
              + annotate("point", x = 20, y = 200, label = "no tillage")
              
              #         + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
              #         + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.till.SE
#######################################
g.event.SE <- ggplot(data.inc[!116:118], aes(x= WFPS, y = N2OtoNO))
g.event.SE <- (g.event.SE
               + theme_bw(base_size = 8)
               + theme(axis.ticks = element_line(size = 0.1),
                       axis.text.x = element_blank(),
                       plot.margin= unit(c(0, 0.25, 0, 0.1), "lines"),
                       legend.position =   "none",
                       strip.background =   element_blank(),
                       plot.title = element_text(hjust = 0)
               )
               # + xlab("WFPS [%]")
               + xlab("")
               + ylab(expression('N'[2]*'O / NO'))
               # + scale_x_continuous(breaks = c(0, 15, 30, 45))
               + coord_cartesian(ylim=c(0.011,2000))
               + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x)))
               + annotation_logticks(sides = "lr")
)

g.event.SE <- (g.event.SE
               # + facet_grid(precipitation ~ tillage)
               + geom_point(aes(color=factor(myevent)), size=1.5)
               + geom_hline(yintercept=1, col="red")
               + scale_color_manual(values=col.event)
               
               
               
               + geom_smooth(data= data.inc[event==1],method='lm',formula=y~x, se=T, size=0.75, color=col.event[1], alpha=0.25)
               + geom_smooth(data= data.inc[event %in% c(2,3)],method='lm',formula=y~x, se=T, size=0.75, color=col.event[2], alpha=0.25)
               
               + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
               
               + annotate("text", x = 25, y = 500, label = "1st incubation-stretch", size=2.5, color="black", hjust= 0)
               + annotate("point", x = 20, y = 500, color="black")
               + annotate("text", x = 25, y = 200, label = "2nd & 3rd", color="black", size=2.5, hjust= 0)
               + annotate("point", x = 20, y = 200, color="grey")
               
               # + geom_text(data=labels, aes(x=x, y=y, label=label), hjust= -0.1, vjust=0, size=2, col= "grey50")
               
               #         + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
               #         + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
g.event.SE
########################
###
g.C <- ggplot(data.inc[!116:118], aes(x= WFPS, y = N2OtoNO))
g.C <- (g.C
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                plot.margin= unit(c(0, 0.25, 0, 0.1), "lines"),
                legend.position =   c(0.2 ,0.79),
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab("WFPS [%]")
        + ylab(expression('N'[2]*'O / NO'))
        # + scale_x_continuous(breaks = c(0, 15, 30, 45))
        # + coord_cartesian(ylim=c(0,1500))
        + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x)))
        + annotation_logticks(sides = "lr")
)

g.C <- (g.C
        # + facet_grid(precipitation ~ tillage)
        + geom_point(aes(size=CO2), alpha=0.25)
        + geom_hline(yintercept=1, col="red")
        + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
        #                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
        #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
        
)
g.C
###
g.amm <- ggplot(data.inc[!116:118], aes(x= WFPS, y = N2OtoNO))
g.amm <- (g.amm
          + theme_bw(base_size = 8)
          + theme(axis.ticks = element_line(size = 0.1),
                  plot.margin= unit(c(0, 0.25, 0, 0.1), "lines"),
                  legend.position =   c(0.2 ,0.75),
                  strip.background =   element_blank(),
                  plot.title = element_text(hjust = 0)
          )
          + xlab("WFPS [%]")
          + ylab(expression('N'[2]*'O / NO'))
          # + scale_x_continuous(breaks = c(0, 15, 30, 45))
          # + coord_cartesian(ylim=c(0,1500))
          + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x)))
          + annotation_logticks(sides = "lr")
)

g.amm <- (g.amm
          # + facet_grid(precipitation ~ tillage)
          + geom_point(aes(size=ammonium), alpha=0.25)
          + geom_hline(yintercept=1, col="red")
          + geom_smooth(method='lm',formula=y~x, se=F, size=0.5,linetype=2, color="black")
          #                 + geom_smooth(data= data.inc[qCO2=="low"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1)
          #                 + geom_smooth(data= data.inc[qCO2=="up"],method='lm',formula=y~x, se=T, size=0.75, linetype = 1, color= "red")
          
)
g.amm

# ################################################################################
# + theme(axis.ticks = element_line(size = 0.1),
#         plot.margin= unit(c(-0.25, 0.25, -0.25, 0.1), "lines"),
#         axis.text.x =        element_blank(),
#         legend.position =   c(0.2 ,0.25),
#         strip.background =   element_blank(),
#         plot.title = element_text(hjust = 0)
# )
# + xlab("")


###
myplot <- paste0(folder.out, "/N_vs_WFPS_compilation_vs_till_event_amm_CO2__full.png")
png(filename=myplot,  width = 160, height = 160, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.till.SE, vp = vplayout(1, 1))
print(g.event.SE, vp = vplayout(1, 2))
print(g.C, vp = vplayout(2, 1))
print(g.amm, vp = vplayout(2, 2))

dev.off()
