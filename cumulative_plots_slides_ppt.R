# folder_out <- paste0(output_path, "/all_4incubations_plots")
folder_out <-paste0(output_path, "/presentation_plots/cum_fluxes")
dir.create(folder_out)

# flux files
folder.in <-paste0(output_path, "/cum_fluxes_full")
cum.flux.every.out <- paste0(folder.in, "/every_event_cum_fluxes.dat")

# read data files
cum.flux.every <-  fread(input = cum.flux.every.out)


# # unit change
# # [µg-N/m2] to [mg-N/m2]
# cum.flux.every[,NO:= NO/1000]
# cum.flux.every[,N2O:= N2O/1000]
# cum.flux.every[,NOsd:= NOsd/1000]
# cum.flux.every[,N2Osd:= N2Osd/1000]
# 
# cum.flux.every[,aNO:= aNO/1000]
# cum.flux.every[,bNO:= bNO/1000]
# cum.flux.every[,cNO:= cNO/1000]
# cum.flux.every[,dNO:= dNO/1000]
# 
# cum.flux.every[,aN2O:= aN2O/1000]
# cum.flux.every[,bN2O:= bN2O/1000]
# cum.flux.every[,cN2O:= cN2O/1000]
# cum.flux.every[,dN2O:= dN2O/1000]
# 
# # [mg-C/m2] to [g-C/m2]
# cum.flux.every[,CO2:= CO2/1000]
# cum.flux.every[,CO2sd:= CO2sd/1000]
# 
# cum.flux.every[,aCO2:= aCO2/1000]
# cum.flux.every[,bCO2:= bCO2/1000]
# cum.flux.every[,cCO2:= cCO2/1000]
# cum.flux.every[,dCO2:= dCO2/1000]
# # [µg-C/m2] to [mg-C/m2]
# cum.flux.every[,CH4:= CH4/1000]
# cum.flux.every[,CH4sd:= CH4sd/1000]
# 
# cum.flux.every[,aCH4:= aCH4/1000]
# cum.flux.every[,bCH4:= bCH4/1000]
# cum.flux.every[,cCH4:= cCH4/1000]
# cum.flux.every[,dCH4:= dCH4/1000]

# range
NOylim <- range(c(cum.flux.every$aNO,
                  cum.flux.every$bNO,
                  cum.flux.every$cNO,
                  cum.flux.every$dNO))

N2Oylim <- range(c(cum.flux.every$aN2O,
                   cum.flux.every$bN2O,
                   cum.flux.every$cN2O,
                   cum.flux.every$dN2O))

CO2ylim <- range(c(cum.flux.every$aCO2,
                   cum.flux.every$bCO2,
                   cum.flux.every$cCO2,
                   cum.flux.every$dCO2))

CH4ylim <- range(c(cum.flux.every$aCH4,
                   cum.flux.every$bCH4,
                   cum.flux.every$cCH4,
                   cum.flux.every$dCH4))
range <- c(-5, 45)
setkey(cum.flux.every, days)
incubation <- "cumulative"
################################################################################
################################################################################
### CLD
################################################################################
# NO (slides for ppt presentation)
i <- "NO"
state <- "1sd_interval"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i),"_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: NO [mg-N/m2] ", state)

toplot <- cum.flux.every[!is.na(NO)                    
                         , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))


p <- qplot(days, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        #         scale_fill_manual(values = c("black","red"), name="Tillage") +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.93, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) + 
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) +
        geom_line(aes(y=NO-NOsd, ymax=NO-NOsd),  linetype="dotted") +
        geom_line(aes(y=NO+NOsd, ymax=NO+NOsd),  linetype="dotted")

dev.off()
################################################################################
################################################################################
### QCL
################################################################################

# N2O (slides for ppt presentation)
i <- "N2O"
state <- "1sd_interval"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i),"_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: N2O [mg-N/m2] ", state)

toplot <- cum.flux.every[!is.na(N2O)                    
                         , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))


p <- qplot(days, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        #         scale_fill_manual(values = c("black","red"), name="Tillage") +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.93, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) + 
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) +
        geom_line(aes(y=N2O-N2Osd, ymax=N2O-N2Osd),  linetype="dotted") +
        geom_line(aes(y=N2O+N2Osd, ymax=N2O+N2Osd),  linetype="dotted")

dev.off()
################################################################################
# CO2 (slides for ppt presentation)
i <- "CO2"
state <- "1sd_interval"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i),"_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: CO2 [g-C/m2] ", state)

toplot <- cum.flux.every[!is.na(CO2)                    
                         , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))


p <- qplot(days, CO2, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        #         scale_fill_manual(values = c("black","red"), name="Tillage") +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.93, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) + 
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) +
        geom_line(aes(y=CO2-CO2sd, ymax=CO2-CO2sd),  linetype="dotted") +
        geom_line(aes(y=CO2+CO2sd, ymax=CO2+CO2sd),  linetype="dotted")

dev.off()
################################################################################
# CH4 (slides for ppt presentation)
i <- "CH4"
state <- "1sd_interval"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i),"_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: CH4 [mg-C/m2] ", state)

toplot <- cum.flux.every[!is.na(CH4)                    
                         , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))


p <- qplot(days, CH4, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        #         scale_fill_manual(values = c("black","red"), name="Tillage") +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.93, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) + 
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) +
        geom_line(aes(y=CH4-CH4sd, ymax=CH4-CH4sd),  linetype="dotted") +
        geom_line(aes(y=CH4+CH4sd, ymax=CH4+CH4sd),  linetype="dotted")

dev.off()