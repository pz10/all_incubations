folder_out <- paste0(output_path, "/presentation_plots/WFPS_vs_N/WFPS_vs_instant_N")
dir.create(folder_out, recursive =T)

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
################################################################################
################################################################################
################################################################################
# WFPS
# all
ABCDfluxes <- rbind(Aflux, Bflux, Cflux, Dflux)
tobindNO <- copy(ABCDfluxes)
tobindN2O <- copy(ABCDfluxes)
tobindNO[, species:= "NO"]
tobindNO[,NOandN2O:=NO]

tobindN2O[, species:= "N2O"]
tobindN2O[,NOandN2O:=N2O]

mydata <- rbind(tobindNO, tobindN2O)
mydata[NOandN2O<0, NOandN2O:=0]

################################################################################################################################################################
#  t= 0-5 days
################################################################################
# calculate meanNO and meanN2O by WFPSintervals (by treatment)
timecode <- "_00_05"
timecode_t <- "days: 0-5"

data <- copy(mydata)
data <- data[days>0&days<=5,]

title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]", timecode_t)
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O", timecode, ".png")
png(filename = myplot, width = 1920, height = 800, units = "px")

p <- ggplot(data, aes(x=WFPS_0, y=NOandN2O, color=species))
p +
        geom_point( na.rm=TRUE, size=2) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set2") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        
        xlim(0,100) +
        ylim(0,2500) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = "")+
        #         theme(legend.position = c(0.9, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()
################################################################################################################################################################
#  t= 5-10 days
################################################################################
# calculate meanNO and meanN2O by WFPSintervals (by treatment)
timecode <- "_05_10"
timecode_t <- "days: 5-10"

data <- copy(mydata)
data <- data[days>5&days<=10,]

title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]", timecode_t)
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O", timecode, ".png")
png(filename = myplot, width = 1920, height = 800, units = "px")

p <- ggplot(data, aes(x=WFPS_0, y=NOandN2O, color=species))
p +
        geom_point( na.rm=TRUE, size=2) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set2") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        
        xlim(0,100) +
        ylim(0,2500) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = "")+
        #         theme(legend.position = c(0.9, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()
################################################################################################################################################################
#  t= 10-15 days
################################################################################
# calculate meanNO and meanN2O by WFPSintervals (by treatment)
timecode <- "_10_15"
timecode_t <- "days: 10-15"

data <- copy(mydata)
data <- data[days>10&days<=15,]

title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]", timecode_t)
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O", timecode, ".png")
png(filename = myplot, width = 1920, height = 800, units = "px")

p <- ggplot(data, aes(x=WFPS_0, y=NOandN2O, color=species))
p +
        geom_point( na.rm=TRUE, size=2) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set2") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        
        xlim(0,100) +
        ylim(0,2500) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = "")+
        #         theme(legend.position = c(0.9, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()
################################################################################################################################################################
#  t= 15-20 days
################################################################################
# calculate meanNO and meanN2O by WFPSintervals (by treatment)
timecode <- "_15_20"
timecode_t <- "days: 15-20"

data <- copy(mydata)
data <- data[days2ndEvent>0&days2ndEvent<=5,]

title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]", timecode_t)
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O", timecode, ".png")
png(filename = myplot, width = 1920, height = 800, units = "px")

p <- ggplot(data, aes(x=WFPS_0, y=NOandN2O, color=species))
p +
        geom_point( na.rm=TRUE, size=2) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set2") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        
        xlim(0,100) +
        ylim(0,2500) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = "")+
        #         theme(legend.position = c(0.9, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()
################################################################################################################################################################
#  t= 20-25 days
################################################################################
# calculate meanNO and meanN2O by WFPSintervals (by treatment)
timecode <- "_20_25"
timecode_t <- "days: 20-25"

data <- copy(mydata)
data <- data[days2ndEvent>5&days2ndEvent<=10,]

title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]", timecode_t)
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O", timecode, ".png")
png(filename = myplot, width = 1920, height = 800, units = "px")

p <- ggplot(data, aes(x=WFPS_0, y=NOandN2O, color=species))
p +
        geom_point( na.rm=TRUE, size=2) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set2") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        
        xlim(0,100) +
        ylim(0,2500) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = "")+
        #         theme(legend.position = c(0.9, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()
################################################################################################################################################################
#  t= 25-30 days
################################################################################
# calculate meanNO and meanN2O by WFPSintervals (by treatment)
timecode <- "_25_30"
timecode_t <- "days: 25-30"

data <- copy(mydata)
data <- data[days2ndEvent>10&days2ndEvent<=15,]

title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]", timecode_t)
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O", timecode, ".png")
png(filename = myplot, width = 1920, height = 800, units = "px")

p <- ggplot(data, aes(x=WFPS_0, y=NOandN2O, color=species))
p +
        geom_point( na.rm=TRUE, size=2) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set2") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        
        xlim(0,100) +
        ylim(0,2500) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = "")+
        #         theme(legend.position = c(0.9, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()
################################################################################################################################################################
#  t= 30-35 days
################################################################################
# calculate meanNO and meanN2O by WFPSintervals (by treatment)
timecode <- "_30_35"
timecode_t <- "days: 30-35"

data <- copy(mydata)
data <- data[days3rdEvent>0&days3rdEvent<=5,]

title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]", timecode_t)
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O", timecode, ".png")
png(filename = myplot, width = 1920, height = 800, units = "px")

p <- ggplot(data, aes(x=WFPS_0, y=NOandN2O, color=species))
p +
        geom_point( na.rm=TRUE, size=2) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set2") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        
        xlim(0,100) +
        ylim(0,2500) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = "")+
        #         theme(legend.position = c(0.9, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()
################################################################################################################################################################
#  t= 35-40 days
################################################################################
# calculate meanNO and meanN2O by WFPSintervals (by treatment)
timecode <- "_35_40"
timecode_t <- "days: 35-40"

data <- copy(mydata)
data <- data[days3rdEvent>5&days3rdEvent<=10,]

title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]", timecode_t)
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O", timecode, ".png")
png(filename = myplot, width = 1920, height = 800, units = "px")

p <- ggplot(data, aes(x=WFPS_0, y=NOandN2O, color=species))
p +
        geom_point( na.rm=TRUE, size=2) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set2") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        
        xlim(0,100) +
        ylim(0,2500) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = "")+
        #         theme(legend.position = c(0.9, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()
################################################################################################################################################################
#  t= 40-45 days
################################################################################
# calculate meanNO and meanN2O by WFPSintervals (by treatment)
timecode <- "_40_45"
timecode_t <- "days: 40-45"

data <- copy(mydata)
data <- data[days3rdEvent>10&days3rdEvent<=15,]

title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]", timecode_t)
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O", timecode, ".png")
png(filename = myplot, width = 1920, height = 800, units = "px")

p <- ggplot(data, aes(x=WFPS_0, y=NOandN2O, color=species))
p +
        geom_point( na.rm=TRUE, size=2) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set2") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        
        xlim(0,100) +
        ylim(0,2500) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = "")+
        #         theme(legend.position = c(0.9, 0.92))+
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()