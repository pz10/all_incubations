folder_out <-paste0(output_path, "/presentation_plots")
dir.create(folder_out)

NH3folder <-paste0(output_path, "/AMMONIAtrial")
NH3file <- paste0(NH3folder, "/NH3.dat")
data <- fread(input = NH3file)

################################################################################
################################################################################
data[, labelT:= paste(fertilizer, precipitation, sep="-")]
data[, labelP:= paste(tillage, fertilizer, sep="-")]
data[, labelF:= paste(tillage, precipitation, sep="-")]
data[tillage=="NT", legendT:= "No"]
data[tillage=="TT", legendT:= "traditional"]

mydata <- data.frame(data)
mydata$tillage <- factor(mydata$tillage, levels = c("NT", "TT"), labels=c("no", "traditional"))
mydata$precipitation <- factor(mydata$precipitation, levels = c("c", "i", "d"), labels=c("constant", "increasing", "decreasing"))
mydata$fertilizer <- factor(mydata$fertilizer, levels = c(0, 50, 100), labels=c("0 kg-N/ha", "50", "100"))
mydata$labelT <- factor(mydata$labelT, levels = c("0-c",  "0-i", "0-d",
                                                  "50-c",  "50-i", "50-d",
                                                  "100-c", "100-i", "100-d"))
mydata$labelP <- factor(mydata$labelP, levels = c("NT-0", "NT-50", "NT-100",
                                                  "TT-0", "TT-50", "TT-100"))
mydata$labelF <- factor(mydata$labelF, levels = c("NT-c", "NT-i", "NT-d",
                                                  "TT-c", "TT-i", "TT-d"))

str(mydata)
table(mydata$tillage, mydata$precipitation, mydata$fertilizer)
################################################################################


# fit <- aov(NH3 ~ fertilizer + precipitation + tillage , data = mydata)
fit <- aov(log(1+NH3) ~ fertilizer + precipitation + tillage , data = mydata)
# fit <- Anova(aov(NH3 ~ fertilizer + precipitation*tillage, data = mydata))
summary(fit)

TukeyHSD(fit)
par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(fit, which= c("precipitation")))
################################################################################
### tillage boxplot
# by treatment
myplot <- paste0(folder_out, "/NH3_boxplot_tillage_bytreatment.png")
png(filename = myplot, width = 1600, height = 1200, units = "px")

means <- aggregate(NH3 ~  tillage*precipitation*fertilizer, mydata, mean)
p <- ggplot(data=mydata, aes(x = tillage, y=NH3, fill=tillage))

p +
        theme_bw() +
        geom_boxplot() +
        facet_wrap(~ labelT, ncol = 9) +
        scale_fill_manual(values = c("grey", "red")) +
        #         scale_colour_manual(values = c("grey","red")) +
        
        stat_summary(fun.y=mean, colour="black", geom="point", 
                     shape=20, size=5,show_guide = FALSE)  +
        
        theme( panel.grid.major.x = element_blank() ) + # remove the vertical grid lines
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) +
        theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) +
        theme(axis.title.y = element_blank()) +
        ggtitle("Tillage effect on cumulative NH3 emission \n [mg-N / m2]") +
        theme(plot.title = element_text(size = 50, lineheight=0.8, face="bold", vjust=2)) +
        theme(legend.key.height =  unit(5, "cm"))
dev.off()

# all treatments together
myplot <- paste0(folder_out, "/NH3_boxplot_tillage.png")
png(filename = myplot, width = 320, height = 1200, units = "px")
means <- aggregate(NH3 ~  tillage, mydata, mean)
p <- ggplot(data=mydata, aes(x = tillage, y=NH3, fill=tillage))

p +
        theme_bw() +
        geom_boxplot() +
        #         facet_wrap(~ tillage, ncol = 1) +
        scale_fill_manual(values = c("grey", "red")) +
        #         scale_colour_manual(values = c("grey","red")) +
        
        stat_summary(fun.y=mean, colour="black", geom="point", 
                     shape=20, size=5,show_guide = FALSE)  +
        
        theme( panel.grid.major.x = element_blank() ) + # remove the vertical grid lines
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) +
        theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
        theme(legend.position="none")
dev.off()

################################################################################
### precipitation boxplot
# by treatment
myplot <- paste0(folder_out, "/NH3_boxplot_precipitation_bytreatment.png")
png(filename = myplot, width = 1600, height = 1200, units = "px")

means <- aggregate(NH3 ~  tillage*precipitation*fertilizer, mydata, mean)
p <- ggplot(data=mydata, aes(x = precipitation, y=NH3, fill=precipitation))

p +
        theme_bw() +
        geom_boxplot() +
        facet_wrap(~ labelP, ncol = 6) +
        scale_fill_manual(values = c("white", "deepskyblue","dodgerblue4"), name="Rain pattern") +
        #         scale_colour_manual(values = c("grey","red")) +
        stat_summary(fun.y=mean, colour="black", geom="point", 
                     shape=20, size=5,show_guide = FALSE)  +
        
        theme( panel.grid.major.x = element_blank() ) + # remove the vertical grid lines
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) +
        theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) +
        theme(axis.title.y = element_blank()) +
        ggtitle("Rain-pattern effect on cumulative NH3 emission \n [mg-N / m2]") +
        theme(plot.title = element_text(size = 50, lineheight=0.8, face="bold", vjust=2)) +
        theme(legend.key.height =  unit(5, "cm"))

dev.off()

# all treatments together
myplot <- paste0(folder_out, "/NH3_boxplot_precipitation.png")
png(filename = myplot, width = 320, height = 1200, units = "px")
means <- aggregate(NH3 ~  precipitation, mydata, mean)
p <- ggplot(data=mydata, aes(x = precipitation, y=NH3, fill=precipitation))

p +
        theme_bw() +
        geom_boxplot() +
        #         facet_wrap(~ tillage, ncol = 1) +
        scale_fill_manual(values = c("white", "deepskyblue","dodgerblue4"), name="Rain pattern") +
        #         scale_colour_manual(values = c("grey","red")) +
        
        stat_summary(fun.y=mean, colour="black", geom="point", 
                     shape=20, size=5,show_guide = FALSE)  +
        
        theme( panel.grid.major.x = element_blank() ) + # remove the vertical grid lines
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) +
        theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
        theme(legend.position="none")
dev.off()

################################################################################
### fertilizer boxplot
# by treatment
myplot <- paste0(folder_out, "/NH3_boxplot_fertilizer_bytreatment.png")
png(filename = myplot, width = 1600, height = 1200, units = "px")

means <- aggregate(NH3 ~  tillage*precipitation*fertilizer, mydata, mean)
p <- ggplot(data=mydata, aes(x = fertilizer, y=NH3, fill=fertilizer))

p +
        theme_bw() +
        geom_boxplot() +
        facet_wrap(~ labelF, ncol = 6) +
        scale_fill_manual(values = c("white","olivedrab2", "olivedrab4"), name="Fertilizer \n load") +
        #         scale_colour_manual(values = c("grey","red")) +
        stat_summary(fun.y=mean, colour="black", geom="point", 
                     shape=20, size=5,show_guide = FALSE)  +
        
        theme( panel.grid.major.x = element_blank() ) + # remove the vertical grid lines
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(legend.title = element_text(size = 50, face = 'bold')) +
        theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) +
        theme(axis.title.y = element_blank()) +
        ggtitle("Fertilizer-load effect on cumulative NH3 emission \n [mg-N / m2]") +
        theme(plot.title = element_text(size = 50, lineheight=0.8, face="bold", vjust=2)) +
        theme(legend.key.height =  unit(5, "cm"))

dev.off()        

dev.off()

# all treatments together
myplot <- paste0(folder_out, "/NH3_boxplot_fertilizer.png")
png(filename = myplot, width = 320, height = 1200, units = "px")
means <- aggregate(NH3 ~  fertilizer, mydata, mean)
p <- ggplot(data=mydata, aes(x = fertilizer, y=NH3, fill=fertilizer))

p +
        theme_bw() +
        geom_boxplot() +
        #         facet_wrap(~ tillage, ncol = 1) +
        scale_fill_manual(values = c("white","olivedrab2", "olivedrab4")) +
        #         scale_colour_manual(values = c("grey","red")) +
        
        stat_summary(fun.y=mean, colour="black", geom="point", 
                     shape=20, size=5,show_guide = FALSE)  +
        
        theme( panel.grid.major.x = element_blank() ) + # remove the vertical grid lines
        theme(strip.text = element_text(size = 35)) +
        theme(axis.text.y = element_text(size = 35)) +
        theme(legend.text = element_text(size = 35)) +
        theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank()) +
        theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
        theme(legend.position="none")
dev.off()
####
# Pairwise comparisons using t tests with pooled SD 
pairwise.t.test(data$NH3,data$treatment,p.adjust.method = "holm")
pairwise.t.test(data$NH3,data$labelF,p.adjust.method = "holm")
pairwise.t.test(data$NH3,data$labelT,p.adjust.method = "holm")
pairwise.t.test(data$NH3,data$labelP,p.adjust.method = "holm")

pairwise.t.test(data$NH3,data$tillage,p.adjust.method = "holm")
pairwise.t.test(data$NH3,data$precipitation,p.adjust.method = "holm")
pairwise.t.test(data$NH3,data$fertilizer,p.adjust.method = "holm")