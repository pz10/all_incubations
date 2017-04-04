# output files
folder.in <- paste0(output_path, "/cum_fluxes_full")
cum.flux <- paste0(folder.in, "/every_event_cum_fluxes.dat")

cum <- fread(cum.flux)
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

myNO <- cum[,.(
        mymean = mean(NO, na.rm=T),
        mySE = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO)))
), by=.(days, fertilizer, precipitation, tillage)]


myN2O <- cum[,.(
        mymean = mean(N2O, na.rm=T),
        mySE = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O)))
), by=.(days, fertilizer, precipitation, tillage)]


myCO2 <- cum[,.(
        mymean = mean(CO2, na.rm=T),
        mySE = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2)))
), by=.(days, fertilizer, precipitation, tillage)]


myCH4 <- cum[,.(
        mymean = mean(CH4, na.rm=T),
        mySE = sd(CH4, na.rm=T)/sqrt(sum(!is.na(CH4)))
), by=.(days, fertilizer, precipitation, tillage)]



myCO2.till.rain <- cum[,.(
        mymean = mean(CO2, na.rm=T),
        mySE = sd(CO2, na.rm=T)/sqrt(sum(!is.na(CO2)))
), by=.(days, tillage, precipitation)]


folder_out <-paste0(folder.in, "/every_event_cum_plots")
dir.create(folder_out, recursive = T)

### CO2
# tillage rain
g.1 <- ggplot(cum, aes(x= days, y = CO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' [ g-C m'^-2*' ]'))
        + scale_x_continuous(breaks = c(0, 15, 30, 45))
)

g.1 <- (g.1
        + facet_grid(precipitation ~ tillage)
        + geom_point(size=0.25, colour="black")
        + geom_ribbon(data=myCO2.till.rain, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_point(data=myCO2.till.rain, aes(x = days, y = mymean), size=0.5, col="red")
)
myplot <- paste0(folder_out, "/CO2_reset_till.rain_SE.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

# tillage rain fert
g.1 <- ggplot(cum, aes(x= days, y = CO2))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + ylab(expression('CO'[2]*' [ g-C m'^-2*' ]'))
        + scale_x_continuous(breaks = c(0, 15, 30, 45))
)

g.1 <- (g.1
        + facet_grid(precipitation ~ tillage)
        + geom_point(size=0.25, colour="black")
        + geom_ribbon(data=myCO2, aes(x = days, y = mymean, ymin=mymean - mySE, ymax=mymean + mySE, fill= "red"), alpha=0.5)
        + geom_point(data=myCO2, aes(x = days, y = mymean), size=0.5, col="red")
)
myplot <- paste0(folder_out, "/CO2_reset_till.rain_SE.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()