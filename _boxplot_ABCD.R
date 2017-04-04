################################################################################
# NO
g.1a <- ggplot(data, aes(x = precipitation, y=NO, fill=precipitation))
g.1a <- (g.1a
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab(expression('NO   [ mg-N m' ^-2*' ]'))
         + xlab("Rain pattern")
         + labs(title = "(a) \n  ")
)

g.1a <- (g.1a
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.rain)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1a
###
g.1b <- ggplot(data, aes(x = fertilization, y=NO, fill=fertilization))
g.1b <- (g.1b
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(1.8, 0.5, 0, -1), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 axis.text.y = element_blank(),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab("")
         + xlab("Fertilization")
)

g.1b <- (g.1b
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.fert)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1b


g.2 <- ggplot(data, aes(x = precipitation, y=NO, fill=precipitation))
g.2 <- (g.2
        + facet_wrap( ~ labelP, scales = "fixed", ncol = 6)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0), "lines"),
                axis.ticks = element_blank(),
                axis.text.x =        element_text(colour = "white"),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        
        + ylab("")
        + xlab("Rain pattern")
        + xlab("Treatments")
        + labs(title = "(b)")
)

g.2 <- (g.2
        + geom_boxplot(size=0.25, outlier.colour = NA)
        + scale_fill_manual(values = col.rain)
        + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
        + scale_colour_manual(values=col.till)
)
g.2

#
myplot <- paste0(folder_out, "/boxplot_NO.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 100)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1a, vp = vplayout(1, 1:23))
print(g.1b, vp = vplayout(1, 24:40))
print(g.2, vp = vplayout(1, 41:100))

dev.off()
# 

################################################################################
# N2O
g.1a <- ggplot(data, aes(x = precipitation, y=N2O, fill=precipitation))
g.1a <- (g.1a
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab(expression('N'[2]*'O   [ mg-N m' ^-2*' ]'))
         + xlab("Rain pattern")
         + labs(title = "(a) \n  ")
)

g.1a <- (g.1a
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.rain)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1a
###
g.1b <- ggplot(data, aes(x = fertilization, y=N2O, fill=fertilization))
g.1b <- (g.1b
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(1.8, 0.5, 0, -1), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 axis.text.y = element_blank(),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab("")
         + xlab("Fertilization")
)

g.1b <- (g.1b
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.fert)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1b


g.2 <- ggplot(data, aes(x = precipitation, y=N2O, fill=precipitation))
g.2 <- (g.2
        + facet_wrap( ~ labelP, scales = "fixed", ncol = 6)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0), "lines"),
                axis.ticks = element_blank(),
                axis.text.x =        element_text(colour = "white"),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        
        + ylab("")
        + xlab("Treatments")
        + labs(title = "(b)")
)

g.2 <- (g.2
        + geom_boxplot(size=0.25, outlier.colour = NA)
        + scale_fill_manual(values = col.rain)
        + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
        + scale_colour_manual(values=col.till)
)
g.2

#
myplot <- paste0(folder_out, "/boxplot_N2O.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 100)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1a, vp = vplayout(1, 1:23))
print(g.1b, vp = vplayout(1, 24:40))
print(g.2, vp = vplayout(1, 41:100))

dev.off()
################################################################################
# NO.N2O
g.1a <- ggplot(data, aes(x = precipitation, y=NO.N2O, fill=precipitation))
g.1a <- (g.1a
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab(expression('NO + N'[2]*'O   [ mg-N m' ^-2*' ]'))
         + xlab("Rain pattern")
         + labs(title = "(a) \n  ")
)

g.1a <- (g.1a
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.rain)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1a
###
g.1b <- ggplot(data, aes(x = fertilization, y=NO.N2O, fill=fertilization))
g.1b <- (g.1b
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(1.8, 0.5, 0, -1), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 axis.text.y = element_blank(),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab("")
         + xlab("Fertilization")
)

g.1b <- (g.1b
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.fert)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1b


g.2 <- ggplot(data, aes(x = precipitation, y=NO.N2O, fill=precipitation))
g.2 <- (g.2
        + facet_wrap( ~ labelP, scales = "fixed", ncol = 6)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0), "lines"),
                axis.ticks = element_blank(),
                axis.text.x =        element_text(colour = "white"),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        
        + ylab("")
        + xlab("Treatments")
        + labs(title = "(b)")
)

g.2 <- (g.2
        + geom_boxplot(size=0.25, outlier.colour = NA)
        + scale_fill_manual(values = col.rain)
        + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
        + scale_colour_manual(values=col.till)
)
g.2

#
myplot <- paste0(folder_out, "/boxplot_NO.N2O.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 100)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1a, vp = vplayout(1, 1:23))
print(g.1b, vp = vplayout(1, 24:40))
print(g.2, vp = vplayout(1, 41:100))

dev.off()

################################################################################
# NO.N2O.NH3
g.1a <- ggplot(data, aes(x = precipitation, y=NO.N2O.NH3, fill=precipitation))
g.1a <- (g.1a
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab(expression('NO + N'[2]*'O + NH'[3]*'   [ mg-N m' ^-2*' ]'))
         + xlab("Rain pattern")
         + labs(title = "(a) \n  ")
)

g.1a <- (g.1a
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.rain)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1a
###
g.1b <- ggplot(data, aes(x = fertilization, y=NO.N2O.NH3, fill=fertilization))
g.1b <- (g.1b
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(1.8, 0.5, 0, -1), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 axis.text.y = element_blank(),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab("")
         + xlab("Fertilization")
)

g.1b <- (g.1b
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.fert)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1b


g.2 <- ggplot(data, aes(x = precipitation, y=NO.N2O.NH3, fill=precipitation))
g.2 <- (g.2
        + facet_wrap( ~ labelP, scales = "fixed", ncol = 6)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0), "lines"),
                axis.ticks = element_blank(),
                axis.text.x =        element_text(colour = "white"),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        
        + ylab("")
        + xlab("Treatments")
        + labs(title = "(b)")
)

g.2 <- (g.2
        + geom_boxplot(size=0.25, outlier.colour = NA)
        + scale_fill_manual(values = col.rain)
        + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
        + scale_colour_manual(values=col.till)
)
g.2

#
myplot <- paste0(folder_out, "/boxplot_NO.N2O.NH3.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 100)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1a, vp = vplayout(1, 1:23))
print(g.1b, vp = vplayout(1, 24:40))
print(g.2, vp = vplayout(1, 41:100))

dev.off()

################################################################################
# NH3
g.1a <- ggplot(data, aes(x = precipitation, y=NH3, fill=precipitation))
g.1a <- (g.1a
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab(expression('NH'[3]*'   [ mg-N m' ^-2*' ]'))
         + xlab("Rain pattern")
         + labs(title = "(a) \n  ")
)

g.1a <- (g.1a
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.rain)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1a
###
g.1b <- ggplot(data, aes(x = fertilization, y=NH3, fill=fertilization))
g.1b <- (g.1b
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(1.8, 0.5, 0, -1), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 axis.text.y = element_blank(),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab("")
         + xlab("Fertilization")
)

g.1b <- (g.1b
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.fert)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1b


g.2 <- ggplot(data, aes(x = precipitation, y=NH3, fill=precipitation))
g.2 <- (g.2
        + facet_wrap( ~ labelP, scales = "fixed", ncol = 6)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0), "lines"),
                axis.ticks = element_blank(),
                axis.text.x =        element_text(colour = "white"),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        
        + ylab("")
        + xlab("Treatments")
        + labs(title = "(b)")
)

g.2 <- (g.2
        + geom_boxplot(size=0.25, outlier.colour = NA)
        + scale_fill_manual(values = col.rain)
        + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
        + scale_colour_manual(values=col.till)
)
g.2

#
myplot <- paste0(folder_out, "/boxplot_NH3.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 100)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1a, vp = vplayout(1, 1:23))
print(g.1b, vp = vplayout(1, 24:40))
print(g.2, vp = vplayout(1, 41:100))

dev.off()
################################################################################
# CO2
g.1a <- ggplot(data, aes(x = precipitation, y=CO2, fill=precipitation))
g.1a <- (g.1a
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab(expression('CO'[2]*'   [ g-C m' ^-2*' ]'))
         + xlab("Rain pattern")
         + labs(title = "(a) \n  ")
)

g.1a <- (g.1a
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.rain)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1a
###
g.1b <- ggplot(data, aes(x = fertilization, y=CO2, fill=fertilization))
g.1b <- (g.1b
         #         + facet_wrap( ~ precipitation, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(1.8, 0.5, 0, -1), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 axis.text.y = element_blank(),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 #                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab("")
         + xlab("Fertilization")
)

g.1b <- (g.1b
         + geom_boxplot(size=0.25, outlier.colour = NA)
         + scale_fill_manual(values = col.fert)
         + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         + scale_colour_manual(values=col.till)
)
g.1b


g.2 <- ggplot(data, aes(x = precipitation, y=CO2, fill=precipitation))
g.2 <- (g.2
        + facet_wrap( ~ labelP, scales = "fixed", ncol = 6)
        + theme_bw(base_size = 8)
        + theme(plot.margin= unit(c(0, 0.5, 0, 0), "lines"),
                axis.ticks = element_blank(),
                axis.text.x =        element_text(colour = "white"),
                legend.position =   "none",
                panel.margin = unit(0.1, "lines"),
                strip.background = element_blank(),
                #                         strip.text.x = element_blank(),
                plot.title = element_text(hjust = 0)
        )
        
        + ylab("")
        + xlab("Treatments")
        + labs(title = "(b)")
)

g.2 <- (g.2
        + geom_boxplot(size=0.25, outlier.colour = NA)
        + scale_fill_manual(values = col.rain)
        + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
        + scale_colour_manual(values=col.till)
)
g.2

#
myplot <- paste0(folder_out, "/boxplot_CO2.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 100)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

print(g.1a, vp = vplayout(1, 1:23))
print(g.1b, vp = vplayout(1, 24:40))
print(g.2, vp = vplayout(1, 41:100))

dev.off()