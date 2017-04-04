folder_out <-paste0(output_path, "/presentation_plots")
dir.create(folder_out)

folder.out2 <-paste0(output_path, "/cum_fluxes_14")
cum.flux.1st.14 <- paste0(folder.out2, "/1st_event_cum14_fluxes.dat")
cum.flux.2nd.14 <- paste0(folder.out2, "/2nd_event_cum14_fluxes.dat")
cum.flux.3rd.14 <- paste0(folder.out2, "/3rd_event_cum14_fluxes.dat")
cum.flux.1st.29 <- paste0(folder.out2, "/1st_event_cum29_fluxes.dat")
cum.flux.1st.44 <- paste0(folder.out2, "/1st_event_cum44_fluxes.dat")

data <- fread(input = cum.flux.1st.44)
################################################################################
### A incubation
cum.A <- copy(data)
cum.A[, NO:= aNO]
cum.A[, N2O:= aN2O]
cum.A[, CO2:= aCO2]
cum.A[, CH4:= aCH4]
# cum.A[, todelete:=NULL, with=FALSE]
cum.A[, incubation:= "A"]

### B incubation
cum.B <- copy(data)
cum.B[, NO:= bNO]
cum.B[, N2O:= bN2O]
cum.B[, CO2:= bCO2]
cum.B[, CH4:= bCH4]
# cum.B[, todelete:=NULL, with=FALSE]
cum.B[, incubation:= "B"]
# cum.B[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### C incubation
cum.C <- copy(data)
cum.C[, NO:= cNO]
cum.C[, N2O:= cN2O]
cum.C[, CO2:= cCO2]
cum.C[, CH4:= cCH4]
# cum.C[, todelete:=NULL, with=FALSE]
cum.C[, incubation:= "C"]
# cum.C[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### D incubation
cum.D <- copy(data)
cum.D[, NO:= dNO]
cum.D[, N2O:= dN2O]
cum.D[, CO2:= dCO2]
cum.D[, CH4:= dCH4]
# cum.D[, todelete:=NULL, with=FALSE]
cum.D[, incubation:= "D"]
# cum.D[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### all incubation binding
data <- rbind(cum.A, cum.B, cum.C, cum.D)
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


fit <- aov(N2O ~ fertilizer + precipitation + tillage , data = mydata)
# fit <- Anova(aov(N2O ~ fertilizer + precipitation*tillage, data = mydata))
summary(fit)

TukeyHSD(fit)
par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(fit, which= c("precipitation")))
# plot(TukeyHSD(fit, which= c("fertilizer")))
# plot(TukeyHSD(fit, which= c("tillage")))
################################################################################
### tillage boxplot
# by treatment
myplot <- paste0(folder_out, "/N2O_boxplot_tillage_bytreatment.png")
png(filename = myplot, width = 1600, height = 1200, units = "px")

means <- aggregate(N2O ~  tillage*precipitation*fertilizer, mydata, mean)
p <- ggplot(data=mydata, aes(x = tillage, y=N2O, fill=tillage))

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
        ggtitle("Tillage effect on cumulative N2O emission \n [mg-N / m2]") +
        theme(plot.title = element_text(size = 50, lineheight=0.8, face="bold", vjust=2)) +
        theme(legend.key.height =  unit(5, "cm"))
dev.off()

# all treatments together
myplot <- paste0(folder_out, "/N2O_boxplot_tillage.png")
png(filename = myplot, width = 320, height = 1200, units = "px")
means <- aggregate(N2O ~  tillage, mydata, mean)
p <- ggplot(data=mydata, aes(x = tillage, y=N2O, fill=tillage))

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
myplot <- paste0(folder_out, "/N2O_boxplot_precipitation_bytreatment.png")
png(filename = myplot, width = 1600, height = 1200, units = "px")

means <- aggregate(N2O ~  tillage*precipitation*fertilizer, mydata, mean)
p <- ggplot(data=mydata, aes(x = precipitation, y=N2O, fill=precipitation))

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
        ggtitle("Rain-pattern effect on cumulative N2O emission \n [mg-N / m2]") +
        theme(plot.title = element_text(size = 50, lineheight=0.8, face="bold", vjust=2)) +
        theme(legend.key.height =  unit(5, "cm"))

dev.off()

# all treatments together
myplot <- paste0(folder_out, "/N2O_boxplot_precipitation.png")
png(filename = myplot, width = 320, height = 1200, units = "px")
means <- aggregate(N2O ~  precipitation, mydata, mean)
p <- ggplot(data=mydata, aes(x = precipitation, y=N2O, fill=precipitation))

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
myplot <- paste0(folder_out, "/N2O_boxplot_fertilizer_bytreatment.png")
png(filename = myplot, width = 1600, height = 1200, units = "px")

means <- aggregate(N2O ~  tillage*precipitation*fertilizer, mydata, mean)
p <- ggplot(data=mydata, aes(x = fertilizer, y=N2O, fill=fertilizer))

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
        ggtitle("Fertilizer-load effect on cumulative N2O emission \n [mg-N / m2]") +
        theme(plot.title = element_text(size = 50, lineheight=0.8, face="bold", vjust=2)) +
        theme(legend.key.height =  unit(5, "cm"))

dev.off()        

dev.off()

# all treatments together
myplot <- paste0(folder_out, "/N2O_boxplot_fertilizer.png")
png(filename = myplot, width = 320, height = 1200, units = "px")
means <- aggregate(N2O ~  fertilizer, mydata, mean)
p <- ggplot(data=mydata, aes(x = fertilizer, y=N2O, fill=fertilizer))

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
pairwise.t.test(data$N2O,data$treatment,p.adjust.method = "holm")
pairwise.t.test(data$N2O,data$labelF,p.adjust.method = "holm")
pairwise.t.test(data$N2O,data$labelT,p.adjust.method = "holm")
pairwise.t.test(data$N2O,data$labelP,p.adjust.method = "holm")

pairwise.t.test(data$N2O,data$tillage,p.adjust.method = "holm")
pairwise.t.test(data$N2O,data$precipitation,p.adjust.method = "holm")
pairwise.t.test(data$N2O,data$fertilizer,p.adjust.method = "holm")