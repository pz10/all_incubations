folder_out <- paste0(output_path, "/presentation_plots/water_treatments")
dir.create(folder_out)

# treatments file
treatment.file <- paste0(input_path, "/_all_fluxes/treatments.dat")
treatments <- fread(input = treatment.file)


################################################################################
mydays <- 0:45
heading <- c("days", "treatment", "fertilizer", "precipitation", "tillage",
             "Rain")
dim_names <- list(NULL, heading)
nrows <- length(mydays)*18
ncols <- length(heading)

rain <- matrix(data = as.numeric(NA), nrow = nrows, ncol = ncols, byrow = TRUE, dimnames = dim_names)
rain <- data.table(rain)

rain[,days:=rep(0:45,18)]
rain[,treatment:=rep(treatments$treatment, each=46)]
rain[,fertilizer:=rep(treatments$fertilizer, each=46)]
rain[,precipitation:=rep(treatments$precipitation, each=46)]
rain[,tillage:=rep(treatments$tillage, each=46)]

rain[precipitation=="c"& days%%3 == 0, Rain:=6]
rain[precipitation=="d" & days == 0, Rain:=50]
rain[precipitation=="d" & days == 15, Rain:=30]
rain[precipitation=="d" & days == 30, Rain:=10]
rain[precipitation=="i" & days == 0, Rain:=10]
rain[precipitation=="i" & days == 15, Rain:=30]
rain[precipitation=="i" & days == 30, Rain:=50]
################################################################################


myplot <- paste0(folder_out, "/Rain_treatments_nogrids.png")
png(filename = myplot, width = 1920, height = 1200, units = "px")

toplot <- rain                    

toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))

p <- qplot(days, Rain, data=toplot, color=precipitation, facets=precipitation ~ fertilizer, size=I(2),
           xlab="", ylab="",  xlim= c(0,45), ylim=c(0,50))
p +
        theme_bw() +
        scale_fill_manual(values = c("deepskyblue", "deepskyblue1","dodgerblue4"), name="Rain pattern") +
        scale_shape_manual(values = c(6,1)) +
        theme( panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank() ) + # remove the vertical grid lines
        opts(axis.ticks=theme_blank()) +
        theme(legend.position = "") +
        theme(plot.title = element_text(lineheight= 0.7, face="bold", size=50, vjust=2)) +  
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold'))  +
        scale_x_continuous( breaks = c(0,15,30,45))


dev.off()
