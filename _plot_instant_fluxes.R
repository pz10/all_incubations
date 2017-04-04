# folder_out <- paste0(output_path, "/all_4incubations_plots")
folder_out <-paste0(output_path, "/monography_plots/instant_fluxes")
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
Aflux[,incubation:= "A"]
Bflux[,incubation:= "B"]
Cflux[,incubation:= "C"]
Dflux[,incubation:= "D"]

fluxes <- rbind(Aflux, Bflux, Cflux, Dflux)
setnames(fluxes, "fertilizer", "fertilization")
fluxes[precipitation=="cons", precipitation:= "homogeneous"]
fluxes[, tillage:= factor(tillage, levels = c("NT", "T"), labels = c("no tillage", "traditional tillage"))]
fluxes[, precipitation:= factor(precipitation, levels = c("homogeneous", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing"))]
fluxes[, fertilization:= factor(fertilization, levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))]

fluxes[days.adjusted <(-2), days.adjusted:=NA]
fluxes[days.adjusted > 14 & days.adjusted < 15, days.adjusted:=NA]
fluxes[days.adjusted > 29 & days.adjusted < 30, days.adjusted:=NA]
fluxes[days.adjusted > 44, days.adjusted:=NA]

###########################################################################################################
################################################################################
################################################################################
# plots
################################################################################
# NO
ann_nt <- data.frame(days.adjusted = 25, NO = 800, lab = "no tillage",
                     precipitation = factor("homogeneous",levels = c("homogeneous", "incr", "decr")),
                     fertilization = factor(0,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))
)
ann_tt <- data.frame(days.adjusted = 25, NO = 1000, lab = "traditional tillage",
                     precipitation = factor("homogeneous",levels = c("homogeneous", "incr", "decr")),
                     fertilization = factor(0,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))
)
mypoint <- data.table(
        days.adjusted = c(20,20),
        NO = c(1000, 800),
        tillage = factor(c("T", "NT"), levels = c("NT", "T"), labels = c("no tillage", "traditional tillage")),
        precipitation = factor(rep("homogeneous", 2), levels = c("homogeneous", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing")),
        fertilization = factor(rep(0,2), levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))
)


g.1 <- ggplot(fluxes, aes(x= days.adjusted, y = NO))
g.1 <- (g.1
        + facet_grid(precipitation ~ fertilization, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0),
                
                legend.key.size =    unit(2.2, "lines")
        )
        + ylab(expression('NO   [ µg-N m' ^2 * ' h'^-1 *' ]'))
        + xlab("days")
        + coord_cartesian(xlim= c(-5, 50))
        + scale_x_continuous(breaks = c(0,15,30,45))
        + scale_y_continuous(breaks = c(0,250,500,750,1000))
)

g.1 <- (g.1
        + geom_point(mapping= aes(colour = tillage), size=0.5)
        + scale_colour_manual(values = c("black","red"))
        + geom_text(data = ann_tt, label = "traditional tillage",hjust= 0, col="red", size=2.5)
        + geom_text(data = ann_nt, label = "no tillage",hjust= 0, size=2.5)
        # + geom_point(data=mypoint, mapping= aes(colour = tillage), size=1)
)
# g.1

myplot <- paste0(folder_out, "/instant_NO.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()


################################################################################
# N2O
ann_nt <- data.frame(days.adjusted = 0, N2O = 1600, lab = "no tillage",
                     precipitation = factor("homogeneous",levels = c("homogeneous", "incr", "decr")),
                     fertilization = factor(0,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))
)
ann_tt <- data.frame(days.adjusted = 0, N2O = 1800, lab = "traditional tillage",
                     precipitation = factor("homogeneous",levels = c("homogeneous", "incr", "decr")),
                     fertilization = factor(0,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))
)
mypoint <- data.table(
        days.adjusted = c(20,20),
        N2O = c(1000, 800)*2,
        tillage = factor(c("T", "NT"), levels = c("NT", "T"), labels = c("no tillage", "traditional tillage")),
        precipitation = factor(rep("homogeneous", 2), levels = c("homogeneous", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing")),
        fertilization = factor(rep(0,2), levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))
)


g.1 <- ggplot(fluxes, aes(x= days.adjusted, y = N2O))
g.1 <- (g.1
        + facet_grid(precipitation ~ fertilization, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0),
                
                legend.key.size =    unit(2.2, "lines")
        )
        + ylab(expression('N'[2]*'O   [ µg-N m' ^2 * ' h'^-1 *' ]'))
        + xlab("days")
        + coord_cartesian(xlim= c(-5, 50), ylim=c(0,2000))
        + scale_x_continuous(breaks = c(0,15,30,45))
        + scale_y_continuous(breaks = c(250,500,750,1000)*2)
)

g.1 <- (g.1
        + geom_point(mapping= aes(colour = tillage), size=0.5)
        + scale_colour_manual(values = c("black","red"))
        + geom_text(data = ann_tt, label = "traditional tillage",hjust= 0, col="red", size=2.5)
        + geom_text(data = ann_nt, label = "no tillage",hjust= 0, size=2.5)
        + geom_point(data=mypoint, mapping= aes(colour = tillage), size=1)
)
# g.1

myplot <- paste0(folder_out, "/instant_N2O.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()

################################################################################
# CO2
ann_nt <- data.frame(days.adjusted = 23, CO2 = 160, lab = "no tillage",
                     precipitation = factor("homogeneous",levels = c("homogeneous", "incr", "decr")),
                     fertilization = factor(0,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))
)
ann_tt <- data.frame(days.adjusted = 23, CO2 = 190, lab = "traditional tillage",
                     precipitation = factor("homogeneous",levels = c("homogeneous", "incr", "decr")),
                     fertilization = factor(0,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))
)
# mypoint <- data.table(
#         days.adjusted = c(20,20),
#         CO2 = c(1000, 800)*2,
#         tillage = factor(c("T", "NT"), levels = c("NT", "T"), labels = c("no tillage", "traditional tillage")),
#         precipitation = factor(rep("homogeneous", 2), levels = c("homogeneous", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing")),
#         fertilization = factor(rep(0,2), levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))
# )


# g.1 <- ggplot(fluxes, aes(x= days.adjusted, y = CO2))
g.1 <- ggplot(fluxes[incubation != "D"], aes(x= days.adjusted, y = CO2))
g.1 <- (g.1
        + facet_grid(precipitation ~ fertilization, scales = "fixed")
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0.25, 0.5, 0, 0), "lines"),
                axis.ticks = element_line(size = 0.1),
                legend.position =   "none",
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0),
                
                legend.key.size =    unit(2.2, "lines")
        )
        + ylab(expression('CO'[2]*'O   [ mg-C m' ^2 * ' h'^-1 *' ]'))
        + xlab("days")
        + coord_cartesian(xlim= c(-5, 50), ylim= c(-20,230))
        + scale_x_continuous(breaks = c(0,15,30,45))
        # + scale_y_continuous(breaks = c(250,500,750,1000)*2)
)

g.1 <- (g.1
        + geom_point(mapping= aes(colour = tillage), size=0.5)
        + scale_colour_manual(values = c("black","red"))
        + geom_text(data = ann_tt, label = "traditional tillage",hjust= 0, col="red", size=2.5)
        + geom_text(data = ann_nt, label = "no tillage",hjust= 0, size=2.5)
#         # + geom_point(data=mypoint, mapping= aes(colour = tillage), size=1)
)
g.1

myplot <- paste0(folder_out, "/instant_CO2_ABC.png")
png(filename=myplot,  width = 160, height = 100, units = "mm", res=1200)
print(g.1)
dev.off()