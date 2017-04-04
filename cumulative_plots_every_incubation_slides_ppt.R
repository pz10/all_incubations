# folder_out <- paste0(output_path, "/all_4incubations_plots")
folder_out <-paste0(output_path, "/presentation_plots/cum_fluxes")
dir.create(folder_out)
folder.out1 <-paste0(output_path, "/cum_fluxes_full")

# flux files
cum.flux.every.out <- paste0(folder.out1, "/every_event_cum_fluxes.dat")

# read data files
cum.flux.every <-  fread(input = cum.flux.every.out)
tokeep <- names(cum.flux.every) %in% c("days", "treatment", "fertilizer", "precipitation", "tillage", "NO", "N2O", "CO2", "CH4")
todelete <- names(cum.flux.every)[!tokeep]

### A incubation
cum.A <- copy(cum.flux.every)
cum.A[, NO:= aNO]
cum.A[, N2O:= aN2O]
cum.A[, CO2:= aCO2]
cum.A[, CH4:= aCH4]
cum.A[, todelete:=NULL, with=FALSE]
cum.A[, incubation:= "A"]

### B incubation
cum.B <- copy(cum.flux.every)
cum.B[, NO:= bNO]
cum.B[, N2O:= bN2O]
cum.B[, CO2:= bCO2]
cum.B[, CH4:= bCH4]
cum.B[, todelete:=NULL, with=FALSE]
cum.B[, incubation:= "B"]
# cum.B[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### C incubation
cum.C <- copy(cum.flux.every)
cum.C[, NO:= cNO]
cum.C[, N2O:= cN2O]
cum.C[, CO2:= cCO2]
cum.C[, CH4:= cCH4]
cum.C[, todelete:=NULL, with=FALSE]
cum.C[, incubation:= "C"]
# cum.C[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### D incubation
cum.D <- copy(cum.flux.every)
cum.D[, NO:= dNO]
cum.D[, N2O:= dN2O]
cum.D[, CO2:= dCO2]
cum.D[, CH4:= dCH4]
cum.D[, todelete:=NULL, with=FALSE]
cum.D[, incubation:= "D"]
# cum.D[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### all incubation binding
cum.flux.every <- rbind(cum.A, cum.B, cum.C, cum.D)

# # unit change
# # [µg-N/m2] to [mg-N/m2]
# cum.flux.every[,NO:= NO/1000]
# cum.flux.every[,N2O:= N2O/1000]
# # [mg-C/m2] to [g-C/m2]
# cum.flux.every[,CO2:= CO2/1000]
# # [µg-C/m2] to [mg-C/m2]
# cum.flux.every[,CH4:= CH4/1000]

# range
range <- c(-5, 45)
setkey(cum.flux.every, days)
incubation <- "cumulative_together"
################################################################################
################################################################################
### CLD
################################################################################
# NO (slides for ppt presentation)
i <- "NO"
state <- "cum"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative NO [mg-N/m2] ")

toplot <- cum.flux.every[!is.na(NO)                    
                         , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))


p <- qplot(days, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(1),
           xlab="", ylab="", main= title)
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
        theme(legend.title = element_text(size = 50, face = 'bold'))   +
        scale_x_continuous(limits= range, breaks = c(0,15,30,45))

dev.off()
################################################################################
################################################################################
### QCL
################################################################################

# N2O (slides for ppt presentation)
i <- "N2O"
state <- "cum"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative N2O [mg-N/m2] ")

toplot <- cum.flux.every[!is.na(N2O)                    
                         , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))


p <- qplot(days, N2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(1),
           xlab="", ylab="", main= title)
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
        theme(legend.title = element_text(size = 50, face = 'bold'))    +
        scale_x_continuous(limits= range, breaks = c(0,15,30,45)) 

dev.off()
################################################################################

# CO2 (slides for ppt presentation)
i <- "CO2"
state <- "cum"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative CO2 [g-C/m2] ")

toplot <- cum.flux.every[!is.na(CO2)                    
                         , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))


p <- qplot(days, CO2, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(1),
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
        theme(legend.title = element_text(size = 50, face = 'bold')) 

dev.off()
################################################################################
# CH4 (slides for ppt presentation)
i <- "CH4"
state <- "cum"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0("cumulative CH4 [mg-C/m2] ")

toplot <- cum.flux.every[!is.na(CH4)                    
                         , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))


p <- qplot(days, CH4, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(1),
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
        theme(legend.title = element_text(size = 50, face = 'bold')) 

dev.off()
