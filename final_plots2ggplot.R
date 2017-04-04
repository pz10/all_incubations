# folder_out <- paste0(output_path, "/all_4incubations_plots")
folder_out <-paste0(output_path, "/presentation_plots/instant_fluxes")
dir.create(folder_out, recursive = T)

# flux files
A.flux.file <- paste0(input_path, "/A_IDASw/A_fluxes/A_fluxes.dat")
B.flux.file <- paste0(input_path, "/B_IDASw/B_fluxes/B_fluxes.dat")
C.flux.file <- paste0(input_path, "/C_IDASw/C_fluxes/C_fluxes.dat")
D.flux.file <- paste0(input_path, "/D_IDASw/D_fluxes/D_fluxes.dat")


# read data files
Aflux <- fread(input = A.flux.file)
Bflux <- fread(input = B.flux.file) 
Cflux <- fread(input = C.flux.file) 
Dflux <- fread(input = D.flux.file)
ABCDfluxes <- rbind(Aflux, Bflux, Cflux, Dflux)
# range <- range(data$days, na.rm =T)
range <- c(-5, 45)
setkey(ABCDfluxes, days)
incubation <- "all"
################################################################################
################################################################################
### CLD
################################################################################
# NO
i <- "NO"
state <- "fullscale"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: NO [µg-N/m2/h] ", state)

toplot <- ABCDfluxes[!is.na(NO)                    
                    , list(days, NO, mNO, fertilizer, precipitation, tillage)]


p <- qplot(days, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12)) +
        scale_x_continuous(limits= c(-5,45), breaks = c(0,15,30,45))


dev.off()

####
# NO (slides for ppt presentation)
i <- "NO"
state <- "fullscale"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: NO [µg-N/m2/h] ", state)


toplot <- ABCDfluxes[!is.na(NO)                    
                         , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(days, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.93, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))  +
        scale_x_continuous(limits= range, breaks = c(0,15,30,45))


dev.off()

# NO scaled 1000
i <- "NO"
state <- "scaled1000"
ylim <- c(-10,1000)

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: NO [µg-N/m2/h] ", state)

toplot <- ABCDfluxes[!is.na(NO)
                    , list(days, NO, mNO, fertilizer, precipitation, tillage)]


p <- qplot(days, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
# NO scaled 2500
i <- "NO"
state <- "scaled2500"
ylim <- c(-10,2500)

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: NO [µg-N/m2/h] ", state)

toplot <- ABCDfluxes[!is.na(NO)                    
                    , list(days, NO, mNO, fertilizer, precipitation, tillage)]


p <- qplot(days, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

# NO scaled zero
i <- "NO"
state <- "scaled_zero2"
ylim <- c(-2,2)

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: NO [µg-N/m2/h] ", state)

toplot <- ABCDfluxes[!is.na(NO)                    
                     , list(days, NO, mNO, fertilizer, precipitation, tillage)]


p <- qplot(days, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
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
i <- "N2O"
state <- "fullscale"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: N2O [µg-N/m2/h] ", state)

toplot <- ABCDfluxes[!is.na(N2O)
                    , list(days, N2O, mN2O, fertilizer, precipitation, tillage)]

p <- qplot(days, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
#N2O  (slides for ppt presentation)
i <- "N2O"
state <- "scaled2500"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: N2O [µg-N/m2/h] ", state)


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(days, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, ylim=c(0,2500))
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.93, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))  +
        scale_x_continuous(limits= range, breaks = c(0,15,30,45))


dev.off()

#N2O < scaled 1000
i <- "N2O"
state <- "scaled1000"
ylim <- c(-10,1000)

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: N2O [µg-N/m2/h] ", state)

toplot <- ABCDfluxes[!is.na(N2O)
                    , list(days, N2O, mN2O, fertilizer, precipitation, tillage)]

p <- qplot(days, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

#N2O < scaled 2500
i <- "N2O"
state <- "scaled2500"
ylim <- c(-10,2500)

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: N2O [µg-N/m2/h] ", state)

toplot <- ABCDfluxes[!is.na(N2O)
                    , list(days, N2O, mN2O, fertilizer, precipitation, tillage)]

p <- qplot(days, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
#N2O < scaled around 0
i <- "N2O"
state <- "scaled_zero10"
ylim <- c(-10,10)

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: N2O [µg-N/m2/h] ", state)

toplot <- ABCDfluxes[!is.na(N2O)
                     , list(days, N2O, mN2O, fertilizer, precipitation, tillage)]

p <- qplot(days, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
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
i <- "CO2"
state <- "full scale"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: CO2 [mg-C/m2/h]", state)

toplot <- ABCDfluxes[ !is.na(CO2)
                     , list(days, CO2, mCO2, fertilizer, precipitation, tillage)]

p <- qplot(days, CO2, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()
#CO2 0-300
i <- "CO2"
state <- "scaled300"
ylim <- c(-10,300)

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: CO2 [mg-C/m2/h]", state)

toplot <- ABCDfluxes[ !is.na(CO2)
                     , list(days, CO2, mCO2, fertilizer, precipitation, tillage)]

p <- qplot(days, CO2, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

#CO2 0-300
i <- "CO2"
state <- "scaled_zero50"
ylim <- c(-50,50)

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: CO2 [mg-C/m2/h]", state)

toplot <- ABCDfluxes[ !is.na(CO2)
                      , list(days, CO2, mCO2, fertilizer, precipitation, tillage)]

p <- qplot(days, CO2, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)
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
i <- "CH4"
state <- "full scale"

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: CH4 [µg-C/m2/h]", state)

toplot <- ABCDfluxes[ !is.na(CH4)
                     , list(days, CH4, mCH4, fertilizer, precipitation, tillage)]

p <- qplot(days, CH4, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)

p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()

#CH4
i <- "CH4"
state <- "scaled20"
ylim <- c(-20, 20)

myplot <- paste0(folder_out, "/", "ggplot_", incubation, "_", paste(i), "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: CH4 [µg-C/m2/h]", state)

toplot <- ABCDfluxes[ !is.na(CH4)
                     , list(days, CH4, mCH4, fertilizer, precipitation, tillage)]

p <- qplot(days, CH4, data = toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim = ylim)

p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(1, 1))+
        theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
        theme(strip.text = element_text(size = 15)) +
        theme(axis.text.y = element_text(size = 12))


dev.off()