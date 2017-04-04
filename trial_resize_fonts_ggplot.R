# NO
i <- "NO"
state <- "cum"

myplot <- paste0(folder_out, "/", "slides_", incubation, "_", paste(i),".png")
png(filename = myplot, width = 1170, height = 750, units = "px")
title <- paste0("cumulative NO [mg-N/m2] ")

toplot <- cum.flux.every[!is.na(NO)                    
                         , ]
toplot <- data.frame(toplot)
toplot$tillage <- factor(toplot$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
toplot$precipitation <- factor(toplot$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
toplot$fertilizer <- factor(toplot$fertilizer, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))


p <- qplot(days, NO, data=toplot, color=tillage, facets=precipitation ~ fertilizer, size=I(1),
           xlab="", ylab="", main= title, na.rm = TRUE, xlim= range)
p +
        theme_bw() +
        scale_colour_manual(values = c("black","red")) +
#         scale_fill_manual(values = c("black","red"), name="Tillage") +
        scale_shape_manual(values = c(6,1)) +
        theme(legend.position = c(0.93, 0.92))+
        theme(plot.title = element_text(lineheight= .7, face="bold", size=24)) + 
        theme(strip.text = element_text(size = 20)) +
        theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20)) +
        theme(legend.text = element_text(size = 20)) +
        theme(legend.title = element_text(size = 30, face = 'bold')) 

dev.off()


theme( panel.grid.major.x = element_blank() ) + # remove the vertical grid lines
        theme(strip.text = element_text(size = 25)) +
        theme(axis.text.y = element_text(size = 25)) +
        theme(legend.text = element_text(size = 30)) +
        theme(legend.title = element_text(size = 35, face = 'bold')) +
        theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(legend.key.height =  unit(3, "cm")) +
        ggtitle("Fertilizer-load effect on cumulative NO emission \n [mg-N / m2]") +
        theme(plot.title = element_text(size = 25, lineheight=0.8, face="bold", vjust=2))