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
tobindNO <- copy(ABCDfluxes)
tobindN2O <- copy(ABCDfluxes)
tobindNO[, species:= "NO"]
tobindNO[,NOandN2O:=NO]

tobindN2O[, species:= "N2O"]
tobindN2O[,NOandN2O:=N2O]

data <- rbind(tobindNO, tobindN2O)
data[NOandN2O<0, NOandN2O:=0]



title <- paste("WFPS [%] vs N-emission (NO or N2O) [µg-N/m2/h]")
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O.png")
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

# from WFPS_vs_mean_N_plots.R
title <- paste("WFPS [%] vs NO ~ N2O relative abundance [%]")
myplot <- paste0(folder_out, "/slides_WFPS_vs_NOandN2O_relative_abundance.png")
png(filename = myplot, width = 1920, height = 400, units = "px")

p <- ggplot(mybind, aes(x=WFPS, y=rel_abundance, color=species))
p +
        geom_point(alpha=1, na.rm=TRUE, size=5) +
        #         scale_colour_manual(values = c("deepskyblue4","darkorange3")) +
        #         scale_colour_manual(values = c("black","red")) +
        #         scale_colour_brewer(palette="Set1") +
        scale_colour_manual(values = c("dodgerblue4","orangered")) +
        xlim(0,100) +
        ylim(0,100) +
        xlab("") + #WFPS [%]
        ylab("") + #[µg-n/m2/h]
        ggtitle(title) +
        
        theme(legend.position = c(0.85,0.5)) +
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))
dev.off()