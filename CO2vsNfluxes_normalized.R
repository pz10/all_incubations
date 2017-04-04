# output files
folder.in <- paste0(output_path, "/CO2vsNflux")
myfile <- paste0(folder.in, "/instant_flux_quantiles.dat")
data <- fread(myfile)


data <- data[,.(
        days = days,
        fertilization = factor(fertilization),
        precipitation =factor(precipitation),
        tillage = factor(tillage),
        incubation = factor(incubation),
        event = factor(event),
        NO, N2O, CO2, CH4, NON2O,
        qCO2 = qCO2.event,
        qNO = qNO.event,
        qN2O = qN2O.event,
        qNON2O = qNON2O.event
)]

data[,qCO2:= qCO2/50-1]
data[,qNO:= qNO/50-1]
data[,qN2O:= qN2O/50-1]
data[,qNON2O:= qNON2O/50-1]

data[, qCO2xqNO:= qCO2 * qNO]
data[, qCO2xqN2O:= qCO2 * qN2O]
data[, qCO2xqNON2O:= qCO2 * qNON2O]

flux <- copy(data)
flux[, fertilization:= factor(fertilization,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))]
setnames(flux, "fertilization", "fertilizer")
flux[, precipitation:= factor(precipitation,levels = c("cons", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing"))]
flux[, tillage:= factor(tillage, levels = c("NT", "T"), labels=c("no tillage", "traditional tillage"))]
flux[, event:= factor(event, levels = c(1,2,3), labels=c("1st event", "2nd event", "3rd event"))]

################################################################################
### plot
### NO
############
# fertilizer
g.1 <- ggplot(flux, aes(x= qCO2xqNO, y = log(NO)))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('log(NO) [µg N m'[-2]*' h'[-1]*']'))
        + xlab("qCO2 * qNO (normalized)")
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
#         + geom_ribbon(data=NO.fert.ev, aes(x = qNO.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
#         + geom_line(data=NO.fert.ev, aes(x = qNO.event, y = CO2.q50), size=0.5, col="red")
        #         + geom_ribbon(data=myNO, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        #         + geom_line(data=myNO, aes(x = days, y = mymean), size=0.5, col="red")
)
g.1

myplot <- paste0(folder.in, "/NO_vs_qCO2_x_qNO_byfert.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# rain
g.1 <- ggplot(flux, aes(x= qCO2xqNO, y = log(NO)))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('log(NO) [µg N m'[-2]*' h'[-1]*']'))
        + xlab("qCO2 * qNO (normalized)")
)

g.1 <- (g.1
        + facet_grid(precipitation ~ event)
        # + geom_ribbon(data=NO.rain.ev, aes(x = qNO.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        # + geom_line(data=NO.rain.ev, aes(x = qNO.event, y = CO2.q50), size=0.5, col="red")
        # + geom_smooth(method='lm',formula=y~x, se=FALSE, col="black", size=0.5, linetype=2)
)
g.1

myplot <- paste0(folder.in, "/NO_vs_qCO2_x_qNO_byrain.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# tillage
g.1 <- ggplot(flux, aes(x= qCO2xqNO, y = log(NO)))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('log(NO) [µg N m'[-2]*' h'[-1]*']'))
        + xlab("qCO2 * qNO (normalized)")
)

g.1 <- (g.1
        + facet_grid(tillage ~ event)
        # + geom_ribbon(data=NO.till.ev, aes(x = qNO.event, y = CO2.q50, ymin=CO2.q25, ymax=CO2.q75, fill= "red"), alpha=0.4)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
        # + geom_line(data=NO.till.ev, aes(x = qNO.event, y = CO2.q50), size=0.5, col="red")
)
g.1

myplot <- paste0(folder.in, "/NO_vs_qCO2_x_qNO_bytillage.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

### N2O
############
# fertilizer
g.1 <- ggplot(flux, aes(x= qCO2xqN2O, y = log(N2O)))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('log(N2O) [µg N m'[-2]*' h'[-1]*']'))
        + xlab("qCO2 * qN2O (normalized)")
)

g.1 <- (g.1
        + facet_grid(fertilizer ~ event)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
)
g.1

myplot <- paste0(folder.in, "/N2O_vs_qCO2_x_qN2O_byfert.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# rain
g.1 <- ggplot(flux, aes(x= qCO2xqN2O, y = log(N2O)))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('log(N2O) [µg N m'[-2]*' h'[-1]*']'))
        + xlab("qCO2 * qN2O (normalized)")
)

g.1 <- (g.1
        + facet_grid(precipitation ~ event)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
)
g.1

myplot <- paste0(folder.in, "/N2O_vs_qCO2_x_qN2O_byrain.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# tillage
g.1 <- ggplot(flux, aes(x= qCO2xqN2O, y = log(N2O)))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('log(N2O) [µg N m'[-2]*' h'[-1]*']'))
        + xlab("qCO2 * qN2O (normalized)")
)

g.1 <- (g.1
        + facet_grid(tillage ~ event)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
)
g.1

myplot <- paste0(folder.in, "/N2O_vs_qCO2_x_qN2O_bytillage.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

### NON2O
############
# fertilizer
g.1 <- ggplot(flux, aes(x= qCO2xqNON2O, y = log(NON2O)))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('log(NON2O) [µg N m'[-2]*' h'[-1]*']'))
        + xlab("qCO2 * qNON2O (normalized)")
)

g.1 <- (g.1
        + facet_grid(fertilizer ~ event)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
)
g.1

myplot <- paste0(folder.in, "/NON2O_vs_qCO2_x_qNON2O_byfert.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# rain
g.1 <- ggplot(flux, aes(x= qCO2xqNON2O, y = log(NON2O)))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('log(NON2O) [µg N m'[-2]*' h'[-1]*']'))
        + xlab("qCO2 * qNON2O (normalized)")
)

g.1 <- (g.1
        + facet_grid(precipitation ~ event)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
)
g.1

myplot <- paste0(folder.in, "/NON2O_vs_qCO2_x_qNON2O_byrain.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

############
# tillage
g.1 <- ggplot(flux, aes(x= qCO2xqNON2O, y = log(NON2O)))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('log(NON2O) [µg N m'[-2]*' h'[-1]*']'))
        + xlab("qCO2 * qNON2O (normalized)")
)

g.1 <- (g.1
        + facet_grid(tillage ~ event)
        + geom_point(size=0.5, colour="black", alpha= 0.5)
)
g.1

myplot <- paste0(folder.in, "/NON2O_vs_qCO2_x_qNON2O_bytillage.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

