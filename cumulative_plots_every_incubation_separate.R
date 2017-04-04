folder_out <- paste0(output_path, "/all_4incubations_plots")
dir.create(folder_out)
folder.out1 <-paste0(output_path, "/cum_fluxes_full")

# flux files
cum.flux.every.out <- paste0(folder.out1, "/every_event_cum_fluxes.dat")

# read data files
cum.flux.every <-  fread(input = cum.flux.every.out)

# # unit change
# # [µg-N/m2] to [mg-N/m2]
# cum.flux.every[,aNO:= aNO/1000]
# cum.flux.every[,bNO:= bNO/1000]
# cum.flux.every[,cNO:= cNO/1000]
# cum.flux.every[,dNO:= dNO/1000]
# 
# cum.flux.every[,aN2O:= aN2O/1000]
# cum.flux.every[,bN2O:= bN2O/1000]
# cum.flux.every[,cN2O:= cN2O/1000]
# cum.flux.every[,dN2O:= dN2O/1000]
# # [mg-C/m2] to [g-C/m2]
# cum.flux.every[,aCO2:= aCO2/1000]
# cum.flux.every[,bCO2:= bCO2/1000]
# cum.flux.every[,cCO2:= cCO2/1000]
# cum.flux.every[,dCO2:= dCO2/1000]
# # [µg-C/m2] to [mg-C/m2]
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
incubation <- "cumulative_"
################################################################################################################################################################
################################################################################
# A incubation
################################################################################
################################################################################

################################################################################
################################################################################
### CLD
################################################################################
# NO
i <- "NO_A_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative NO [mg-N/m2] (A incubation)")

toplot <- cum.flux.every[!is.na(aNO)                    
                         , ]


p <- qplot(days, aNO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = NOylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
################################################################################
### QCL
################################################################################
#N2O
i <- "N2O_A_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative N2O [mg-N/m2] (A incubation) ")

toplot <- cum.flux.every[!is.na(aN2O)
                         , ]

p <- qplot(days, aN2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = N2Oylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
#CO2
i <- "CO2_A_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative CO2 [g-C/m2] (A incubation)")

toplot <- cum.flux.every[ !is.na(aCO2)
                          , ]

p <- qplot(days, aCO2, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = CO2ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
#CH4
i <- "CH4_A_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative CH4 [mg-C/m2] (A incubation)")

toplot <- cum.flux.every[ !is.na(aCH4)
                          , ]

p <- qplot(days, aCH4, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = CH4ylim)

p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################################################################################################
################################################################################
# B incubation
################################################################################
################################################################################

################################################################################
################################################################################
### CLD
################################################################################
# NO
i <- "NO_B_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative NO [mg-N/m2] (B incubation)")

toplot <- cum.flux.every[!is.na(bNO)                    
                         , ]


p <- qplot(days, bNO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = NOylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
################################################################################
### QCL
################################################################################
#N2O
i <- "N2O_B_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative N2O [mg-N/m2] (B incubation) ")

toplot <- cum.flux.every[!is.na(bN2O)
                         , ]

p <- qplot(days, bN2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = N2Oylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
#CO2
i <- "CO2_B_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative CO2 [g-C/m2] (B incubation)")

toplot <- cum.flux.every[ !is.na(bCO2)
                          , ]

p <- qplot(days, bCO2, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = CO2ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
#CH4
i <- "CH4_B_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative CH4 [mg-C/m2] (B incubation)")

toplot <- cum.flux.every[ !is.na(bCH4)
                          , ]

p <- qplot(days, bCH4, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = CH4ylim)

p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################################################################################################
################################################################################
# C incubation
################################################################################
################################################################################

################################################################################
################################################################################
### CLD
################################################################################
# NO
i <- "NO_C_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative NO [mg-N/m2] (C incubation)")

toplot <- cum.flux.every[!is.na(cNO)                    
                         , ]


p <- qplot(days, cNO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = NOylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
################################################################################
### QCL
################################################################################
#N2O
i <- "N2O_C_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative N2O [mg-N/m2] (C incubation) ")

toplot <- cum.flux.every[!is.na(cN2O)
                         , ]

p <- qplot(days, cN2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = N2Oylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
#CO2
i <- "CO2_C_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative CO2 [g-C/m2] (C incubation)")

toplot <- cum.flux.every[ !is.na(cCO2)
                          , ]

p <- qplot(days, cCO2, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = CO2ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
#CH4
i <- "CH4_C_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative CH4 [mg-C/m2] (C incubation)")

toplot <- cum.flux.every[ !is.na(cCH4)
                          , ]

p <- qplot(days, cCH4, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = CH4ylim)

p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################################################################################################
################################################################################
# D incubation
################################################################################
################################################################################

################################################################################
################################################################################
### CLD
################################################################################
# NO
i <- "NO_D_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative NO [mg-N/m2] (D incubation)")

toplot <- cum.flux.every[!is.na(dNO)                    
                         , ]


p <- qplot(days, dNO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = NOylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
################################################################################
### QCL
################################################################################
#N2O
i <- "N2O_D_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative N2O [mg-N/m2] (D incubation) ")

toplot <- cum.flux.every[!is.na(dN2O)
                         , ]

p <- qplot(days, dN2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = N2Oylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
#CO2
i <- "CO2_D_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative CO2 [g-C/m2] (D incubation)")

toplot <- cum.flux.every[ !is.na(dCO2)
                          , ]

p <- qplot(days, dCO2, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = CO2ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
################################################################################
#CH4
i <- "CH4_D_incubation"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative CH4 [mg-C/m2] (D incubation)")

toplot <- cum.flux.every[ !is.na(dCH4)
                          , ]

p <- qplot(days, dCH4, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = CH4ylim)

p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()