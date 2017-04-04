folder_out <- paste0(output_path, "/presentation_plots/WFPS_vs_N/WFPS_vs_mean_N")
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
ABCDfluxes <- rbind(Aflux, Bflux, Cflux, Dflux)
################################################################################################################################################################
# calculate meanNO and meanN2O by WFPSintervals (by treatment)

window <- 5 # one side
myWFPS <- seq(from = window, to = (100-window), by = 0.5)
heading <- c("WFPS", "treatment", "fertilizer", "precipitation", "tillage",
             "NO", "NO_0", "NO_f",
             "N2O", "N2O_0", "N2O_f")
dim_names <- list(NULL, heading)
nrows <- length(myWFPS)*18
ncols <- length(heading)
WFPS_N <- matrix(data = as.numeric(NA), nrow = nrows, ncol = ncols, byrow = TRUE, dimnames = dim_names)
WFPS_N <- data.table(WFPS_N)
ascharacter <- c("treatment", "precipitation", "tillage") # fertilizer is leave aside bcause 0,50 and 100 are read to numeric by fread
WFPS_N[,ascharacter:= lapply(.SD, as.character),.SDcols = ascharacter, with=F]

count <-0
for(i in 1:18){
        mytreatment <- treatments[i, treatment]
        myfertilizer <- treatments[i, fertilizer]
        myprecipitation <- treatments[i, precipitation]
        mytillage <- treatments[i, tillage]
        mydata <- ABCDfluxes[treatment == mytreatment,]
        for(j in 1:length(myWFPS)){
                jWFPS <- myWFPS[j]
                WFPS_N[count+j, WFPS:= myWFPS[j] ]
                
                WFPS_N[count+j, treatment:= mytreatment]
                WFPS_N[count+j, precipitation:= myprecipitation]
                WFPS_N[count+j, fertilizer:= myfertilizer]
                WFPS_N[count+j, tillage:= mytillage]
                # WFPS
                myNO <- mydata[WFPS>(jWFPS-window) & WFPS <= (jWFPS+window), NO] 
                WFPS_N[count+j, NO:= mean(myNO, na.rm=TRUE)]
                myN2O <- mydata[WFPS>(jWFPS-window) & WFPS <= (jWFPS+window), N2O] 
                WFPS_N[count+j, N2O:= mean(myN2O, na.rm=TRUE)]
                # WFPS_0
                myNO <- mydata[WFPS_0>(jWFPS-window) & WFPS_0 <= (jWFPS+window), NO] 
                WFPS_N[count+j, NO_0:= mean(myNO, na.rm=TRUE)]
                myN2O <- mydata[WFPS_0>(jWFPS-window) & WFPS_0 <= (jWFPS+window), N2O] 
                WFPS_N[count+j, N2O_0:= mean(myN2O, na.rm=TRUE)]
                # WFPS_f
                myNO <- mydata[WFPS_f>(jWFPS-window) & WFPS_f <= (jWFPS+window), NO] 
                WFPS_N[count+j, NO_f:= mean(myNO, na.rm=TRUE)]
                myN2O <- mydata[WFPS_f>(jWFPS-window) & WFPS_f <= (jWFPS+window), N2O] 
                WFPS_N[count+j, N2O_f:= mean(myN2O, na.rm=TRUE)]
        }
        count <- count + length(myWFPS)
}

WFPS_N[is.nan(NO), NO:= 0]
WFPS_N[is.nan(NO_0), NO_0:= 0]
WFPS_N[is.nan(NO_f), NO_f:= 0]
WFPS_N[is.nan(N2O), N2O:= 0]
WFPS_N[is.nan(N2O_0), N2O_0:= 0]
WFPS_N[is.nan(N2O_f), N2O_f:= 0]

WFPS_N[NO < 0, NO:= 0]
WFPS_N[NO_0 < 0, NO_0:= 0]
WFPS_N[NO_f < 0, NO_f:= 0]
WFPS_N[N2O < 0, N2O:= 0]
WFPS_N[N2O_0 < 0, N2O_0:= 0]
WFPS_N[N2O_f < 0, N2O_f:= 0]

# NO/(NO+N2O)
WFPS_N[,NOtimesN2O:= 100*NO/(N2O + NO)]
WFPS_N[is.nan(NOtimesN2O), NOtimesN2O:= 0]

WFPS_N[,NOtimesN2O_0:= 100*NO_0/(N2O_0 + NO_0)]
WFPS_N[is.nan(NOtimesN2O_0), NOtimesN2O_0:= 0]

WFPS_N[,NOtimesN2O_f:= 100*NO_f/(N2O_f + NO_f)]
WFPS_N[is.nan(NOtimesN2O_f), NOtimesN2O_f:= 0]

# NO2/(NO+N2O)
WFPS_N[,N2OtimesNO:= 100*N2O/(N2O + NO)]
WFPS_N[is.nan(N2OtimesNO), N2OtimesNO:= 0]

WFPS_N[,N2OtimesNO_0:= 100*N2O_0/(N2O_0 + NO_0)]
WFPS_N[is.nan(N2OtimesNO_0), N2OtimesNO_0:= 0]

WFPS_N[,N2OtimesNO_f:= 100*N2O_f/(N2O_f + NO_f)]
WFPS_N[is.nan(N2OtimesNO_f), N2OtimesNO_f:= 0]
################################################################################
# calculate meanNO and meanN2O by WFPSintervals (al data together)
window <- 5 #one side
myWFPS <- seq(from = window, to = (100-window), by = 0.5)

heading <- c("WFPS",
             "NO", "NO_0", "NO_f",
             "N2O", "N2O_0", "N2O_f")
dim_names <- list(NULL, heading)
nrows <- length(myWFPS)
ncols <- length(heading)
WFPS_N_alltogether <- matrix(data = as.numeric(NA), nrow = nrows, ncol = ncols, byrow = TRUE, dimnames = dim_names)
WFPS_N_alltogether <- data.table(WFPS_N_alltogether)
mydata <- ABCDfluxes
for(j in 1:length(myWFPS)){
        jWFPS <- myWFPS[j]
        WFPS_N_alltogether[j, WFPS:= myWFPS[j] ]

        # WFPS
        myNO <- mydata[WFPS>(jWFPS-window) & WFPS <= (jWFPS+window), NO] 
        WFPS_N_alltogether[j, NO:= mean(myNO, na.rm=TRUE)]
        myN2O <- mydata[WFPS>(jWFPS-window) & WFPS <= (jWFPS+window), N2O] 
        WFPS_N_alltogether[j, N2O:= mean(myN2O, na.rm=TRUE)]
        # WFPS_0
        myNO <- mydata[WFPS_0>(jWFPS-window) & WFPS_0 <= (jWFPS+window), NO] 
        WFPS_N_alltogether[j, NO_0:= mean(myNO, na.rm=TRUE)]
        myN2O <- mydata[WFPS_0>(jWFPS-window) & WFPS_0 <= (jWFPS+window), N2O] 
        WFPS_N_alltogether[j, N2O_0:= mean(myN2O, na.rm=TRUE)]
        # WFPS_f
        myNO <- mydata[WFPS_f>(jWFPS-window) & WFPS_f <= (jWFPS+window), NO] 
        WFPS_N_alltogether[j, NO_f:= mean(myNO, na.rm=TRUE)]
        myN2O <- mydata[WFPS_f>(jWFPS-window) & WFPS_f <= (jWFPS+window), N2O] 
        WFPS_N_alltogether[j, N2O_f:= mean(myN2O, na.rm=TRUE)]
}


WFPS_N_alltogether[is.nan(NO), NO:= 0]
WFPS_N_alltogether[is.nan(NO_0), NO_0:= 0]
WFPS_N_alltogether[is.nan(NO_f), NO_f:= 0]
WFPS_N_alltogether[is.nan(N2O), N2O:= 0]
WFPS_N_alltogether[is.nan(N2O_0), N2O_0:= 0]
WFPS_N_alltogether[is.nan(N2O_f), N2O_f:= 0]

WFPS_N_alltogether[NO < 0, NO:= 0]
WFPS_N_alltogether[NO_0 < 0, NO_0:= 0]
WFPS_N_alltogether[NO_f < 0, NO_f:= 0]
WFPS_N_alltogether[N2O < 0, N2O:= 0]
WFPS_N_alltogether[N2O_0 < 0, N2O_0:= 0]
WFPS_N_alltogether[N2O_f < 0, N2O_f:= 0]

# NO/(NO+N2O)
WFPS_N_alltogether[,NOtimesN2O:= 100*NO/(N2O + NO)]
WFPS_N_alltogether[is.nan(NOtimesN2O), NOtimesN2O:= 0]

WFPS_N_alltogether[,NOtimesN2O_0:= 100*NO_0/(N2O_0 + NO_0)]
WFPS_N_alltogether[is.nan(NOtimesN2O_0), NOtimesN2O_0:= 0]

WFPS_N_alltogether[,NOtimesN2O_f:= 100*NO_f/(N2O_f + NO_f)]
WFPS_N_alltogether[is.nan(NOtimesN2O_f), NOtimesN2O_f:= 0]

# NO2/(NO+N2O)
WFPS_N_alltogether[,N2OtimesNO:= 100*N2O/(N2O + NO)]
WFPS_N_alltogether[is.nan(N2OtimesNO), N2OtimesNO:= 0]

WFPS_N_alltogether[,N2OtimesNO_0:= 100*N2O_0/(N2O_0 + NO_0)]
WFPS_N_alltogether[is.nan(N2OtimesNO_0), N2OtimesNO_0:= 0]

WFPS_N_alltogether[,N2OtimesNO_f:= 100*N2O_f/(N2O_f + NO_f)]
WFPS_N_alltogether[is.nan(N2OtimesNO_f), N2OtimesNO_f:= 0]

plot(NO ~WFPS, data = WFPS_N_alltogether, xlim=c(0,100), ylim=c(0,100))
plot(N2O ~WFPS, data = WFPS_N_alltogether, xlim=c(0,100), ylim=c(0,100))

plot(NOtimesN2O ~WFPS, data = WFPS_N_alltogether, xlim=c(0,100), ylim=c(0,100))
points(N2OtimesNO ~WFPS, data = WFPS_N_alltogether, xlim=c(0,100), ylim=c(0,100))
abline(h=50, col="red")

plot(NOtimesN2O_0 ~WFPS, data = WFPS_N_alltogether, xlim=c(0,100), ylim=c(0,100))
points(N2OtimesNO_0 ~WFPS, data = WFPS_N_alltogether, xlim=c(0,100), ylim=c(0,100))
abline(h=50, col="red")

plot(NOtimesN2O_f ~WFPS, data = WFPS_N_alltogether, xlim=c(0,100), ylim=c(0,100))
points(N2OtimesNO_f ~WFPS, data = WFPS_N_alltogether, xlim=c(0,100), ylim=c(0,100))
abline(h=50, col="red")

###
bindNO <- copy(WFPS_N_alltogether)
bindN2O <- copy(WFPS_N_alltogether)
bindNO[,species:="NO"]
bindNO[, rel_abundance:= NOtimesN2O_0]
bindN2O[,species:="N2O"]
bindN2O[, rel_abundance:= N2OtimesNO_0]
mybind <- rbind(bindNO, bindN2O)

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

################################################################################################################################################################
### plots WFPS
range <- c(0, 100)
# yrange <- c(0,2500)
yrange <- range(WFPS_N[, list(NO, NO_0, NO_f, N2O, N2O_0, N2O_f)], na.rm=TRUE)

setkey(WFPS_N, WFPS)
incubation <- "All"
################################################################################
# WFPS_vs_mean_NO
i <- "WFPS_vs_mean_NO"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS [%] vs mean NO [µg-N/m2/h] ")


toplot <- WFPS_N[!is.na(NO)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
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
################################################################################
# WFPS_vs_mean_N2O
i <- "WFPS_vs_mean_N2O"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS [%] vs mean N2O [µg-N/m2/h] ")


toplot <- WFPS_N[!is.na(N2O)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
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
yrange <- c(0,100)
# WFPS_vs_NOtimesN2O
i <- "WFPS_vs_NOtimesN2O"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS [%] vs NO/(NO + N2O) [%]")


toplot <- WFPS_N[!is.na(NOtimesN2O)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, NOtimesN2O, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
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
yrange <- c(0,100)
# WFPS_vs_N2OtimesNO
i <- "WFPS_vs_N2OtimesNO"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS [%] vs N2O/(NO + N2O) [%]")


toplot <- WFPS_N[!is.na(N2OtimesNO)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, N2OtimesNO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
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
### plots WFPS_0
range <- c(0, 100)
# yrange <- c(0,2500)
yrange <- range(WFPS_N[, list(NO, NO_0, NO_f, N2O, N2O_0, N2O_f)], na.rm=TRUE)

setkey(WFPS_N, WFPS)
incubation <- "All"
################################################################################
# WFPS_0_vs_mean_NO
i <- "WFPS_0_vs_mean_NO"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_0 [%] vs mean NO [µg-N/m2/h] ")


toplot <- WFPS_N[!is.na(NO_0)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, NO_0, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
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
# WFPS_0_vs_mean_N2O
i <- "WFPS_0_vs_mean_N2O"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_0 [%] vs mean N2O [µg-N/m2/h] ")


toplot <- WFPS_N[!is.na(N2O_0)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, N2O_0, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
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
yrange <- c(0,100)
# WFPS_vs_NOtimesN2O
i <- "WFPS_0_vs_NOtimesN2O"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_0 [%] vs NO/(NO + N2O) [%]")


toplot <- WFPS_N[!is.na(NOtimesN2O_0)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, NOtimesN2O_0, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
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
yrange <- c(0,100)
# WFPS_0_vs_N2OtimesNO
i <- "WFPS_0_vs_N2OtimesNO"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_0 [%] vs N2O/(NO + N2O) [%]")


toplot <- WFPS_N[!is.na(N2OtimesNO_0)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, N2OtimesNO_0, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
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
### plots WFPS_f
range <- c(0, 100)
# yrange <- c(0,2500)
yrange <- range(WFPS_N[, list(NO, NO_0, NO_f, N2O, N2O_0, N2O_f)], na.rm=TRUE)

setkey(WFPS_N, WFPS)
incubation <- "All"
################################################################################
# WFPS_f_vs_mean_NO
i <- "WFPS_f_vs_mean_NO"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_f [%] vs mean NO [µg-N/m2/h] ")


toplot <- WFPS_N[!is.na(NO_f)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, NO_f, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
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
# WFPS_f_vs_mean_N2O
i <- "WFPS_f_vs_mean_N2O"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, "_", state, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_f [%] vs mean N2O [µg-N/m2/h] ")


toplot <- WFPS_N[!is.na(N2O_f)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, N2O_f, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
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
yrange <- c(0,100)
# WFPS_vs_NOtimesN2O
i <- "WFPS_f_vs_NOtimesN2O"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_f [%] vs NO/(NO + N2O) [%]")


toplot <- WFPS_N[!is.na(NOtimesN2O_f)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, NOtimesN2O_f, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
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
yrange <- c(0,100)
# WFPS_f_vs_N2OtimesNO
i <- "WFPS_f_vs_N2OtimesNO"
state <- "full scale"
# 
myplot <- paste0(folder_out, "/", "slides_",  paste(i), "_",incubation, ".png")
png(filename = myplot, width = 1920, height = 1200, units = "px")
title <- paste0(incubation, " incubations: WFPS_f [%] vs N2O/(NO + N2O) [%]")


toplot <- WFPS_N[!is.na(N2OtimesNO_f)                    
                 , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(WFPS, N2OtimesNO_f, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(2),
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
ABCDfluxes <- rbind(Aflux, Bflux, Cflux, Dflux)
with(ABCDfluxes, {
        plot(NO ~ WFPS,col = "blue", pch=20)
        points(N2O ~ WFPS,col = "orange", pch=20)
}
)
p <- qplot(WFPS, N2O, data=ABCDfluxes, color = "blue")
p1 <- qplot(WFPS, N2O, data=ABCDfluxes)
p + p1

with(ABCDfluxes, plot(N2O ~ WFPS))