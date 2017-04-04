folder_out <- paste0(output_path, "/presentation_plots/WFPS_vs_N")
dir.create(folder_out)

# flux files
A.flux.file <- paste0(input_path, "/A_IDASw/A_fluxes/A_fluxes.dat")
B.flux.file <- paste0(input_path, "/B_IDASw/B_fluxes/B_fluxes.dat")
C.flux.file <- paste0(input_path, "/C_IDASw/C_fluxes/C_fluxes.dat")
D.flux.file <- paste0(input_path, "/D_IDASw/D_fluxes/D_fluxes.dat")

# treatments file
treatment.file <- paste0(input_path, "/_all_fluxes/treatments.dat")

# read data files
Aflux <- fread(input = A.flux.file)
Bflux <- fread(input = B.flux.file) 
Cflux <- fread(input = C.flux.file) 
Dflux <- fread(input = D.flux.file)

treatments <- fread(input = treatment.file)
################################################################################################################################################################
################################################################################
# WFPS
# all
ABCDfluxes <- rbind(Aflux, Bflux, Cflux, Dflux)
# range <- range(data$days, na.rm =T)
range <- c(0, 100)
yrange <- c(0,2500)
setkey(ABCDfluxes, days)
incubation <- "All"
####
# WFPS_vs_NO
i <- "WFPS_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_vs_N2O
i <- "WFPS_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################
# WFPS_0
####
# WFPS_0_vs_NO
i <- "WFPS_0_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_0 [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_0, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_0_vs_N2O
i <- "WFPS_0_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_0 [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_0, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################
# WFPS_f
####
# WFPS_f_vs_NO
i <- "WFPS_f_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_f [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_f, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_f_vs_N2O
i <- "WFPS_f_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_f [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_f, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################################################################################################
################################################################################################################################################################
################################################################################
# WFPS
# A
ABCDfluxes <- rbind(Aflux)
# range <- range(data$days, na.rm =T)
range <- c(0, 100)
yrange <- c(0,2500)
setkey(ABCDfluxes, days)
incubation <- "A"
####
# WFPS_vs_NO
i <- "WFPS_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_vs_N2O
i <- "WFPS_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################
####
# WFPS_0_vs_NO
i <- "WFPS_0_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_0 [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_0, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_0_vs_N2O
i <- "WFPS_0_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_0 [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_0, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################
####
# WFPS_f_vs_NO
i <- "WFPS_f_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_f [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_f, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_f_vs_N2O
i <- "WFPS_f_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_f [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_f, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################################################################################################################################################################################################################################################################
################################################################################
# WFPS
# B
ABCDfluxes <- rbind(Bflux)
# range <- range(data$days, na.rm =T)
range <- c(0, 100)
yrange <- c(0,2500)
setkey(ABCDfluxes, days)
incubation <- "B"
####
# WFPS_vs_NO
i <- "WFPS_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_vs_N2O
i <- "WFPS_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################
####
# WFPS_0_vs_NO
i <- "WFPS_0_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_0 [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_0, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_0_vs_N2O
i <- "WFPS_0_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_0 [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_0, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################
####
# WFPS_f_vs_NO
i <- "WFPS_f_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_f [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_f, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_f_vs_N2O
i <- "WFPS_f_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_f [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_f, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################################################################################################
################################################################################################################################################################
################################################################################
# WFPS
# C
ABCDfluxes <- rbind(Cflux)
# range <- range(data$days, na.rm =T)
range <- c(0, 100)
yrange <- c(0,2500)
setkey(ABCDfluxes, days)
incubation <- "C"
####
# WFPS_vs_NO
i <- "WFPS_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_vs_N2O
i <- "WFPS_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################
####
# WFPS_0_vs_NO
i <- "WFPS_0_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_0 [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_0, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_0_vs_N2O
i <- "WFPS_0_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_0 [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_0, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################
####
# WFPS_f_vs_NO
i <- "WFPS_f_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_f [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_f, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_f_vs_N2O
i <- "WFPS_f_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_f [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_f, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################################################################################################
################################################################################################################################################################
################################################################################
# WFPS
# D
ABCDfluxes <- rbind(Dflux)
# range <- range(data$days, na.rm =T)
range <- c(0, 100)
yrange <- c(0,2500)
setkey(ABCDfluxes, days)
incubation <- "D"
####
# WFPS_vs_NO
i <- "WFPS_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_vs_N2O
i <- "WFPS_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################
####
# WFPS_0_vs_NO
i <- "WFPS_0_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_0 [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_0, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_0_vs_N2O
i <- "WFPS_0_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_0 [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_0, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################
####
# WFPS_f_vs_NO
i <- "WFPS_f_vs_NO"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_f [%] vs NO [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(NO)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_f, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
####
# WFPS_f_vs_N2O
i <- "WFPS_f_vs_N2O"
state <- "2500"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubation: WFPS_f [%] vs N2O [µg-N/m2/h] ")


toplot <- ABCDfluxes[!is.na(N2O)                    
                     , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "T"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("cons", "incr", "decr"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS_f, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range, ylim=yrange)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.73, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) 


dev.off()
################################################################################################################################################################