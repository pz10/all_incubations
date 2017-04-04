# define output directory
my.dir <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/_all_fluxes/cum_fluxes_14/cumFlux_by_event"

# read data
myfile <- paste0(my.dir, "/EventCumFlux_bycore.dat")
data<- fread(myfile)
data <- data[order(N2O, decreasing = T),]
data[,rank:=1:.N]

# colors
col.rain <- c("grey", "deepskyblue","dodgerblue4")
col.fert <- c("grey","olivedrab2", "olivedrab4")
col.till <- c("black","red")

# col.event <- c("#AC391E","#AC691E", "#AC841E")
col.event <- c("grey80", "grey50", "grey20")
col.inc <- c("grey20", "grey40", "grey60", "grey80")

data[, precipitation:= factor(precipitation, levels= c("h", "i", "d"))]

###############
###
g.till <- ggplot(data, aes(x= rank, y = log(N2O)))
g.till <- (g.till
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   c(0.2 ,0.25),
                legend.key.size =    unit(0.25, "lines"),
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab("")
        + ylab(expression('log(N'[2]*'O [ µg-N m'^2*' ] )'))
        # + scale_x_continuous(breaks = c(0, 15, 30, 45))
        # + coord_cartesian(ylim=c(0.1,1500))
)

g.till <- (g.till
        + geom_point(size=0.7, aes(color=factor(tillage)))
        + scale_colour_manual(values=col.till)
        
)
g.till

###
g.fert <- ggplot(data, aes(x= rank, y = log(N2O)))
g.fert <- (g.fert
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   c(0.2 ,0.25),
                legend.key.size =    unit(0.25, "lines"),
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab("")
        + ylab(expression('log(N'[2]*'O [ µg-N m'^2*' ] )'))
        # + scale_x_continuous(breaks = c(0, 15, 30, 45))
        # + coord_cartesian(ylim=c(0.1,1500))
)

g.fert <- (g.fert
        + geom_point(size=0.7, aes(color=factor(fertilization)))
        + scale_colour_manual(values=col.fert)
        
)
g.fert

###
g.rain <- ggplot(data, aes(x= rank, y = log(N2O)))
g.rain <- (g.rain
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                legend.position =   c(0.2 ,0.25),
                legend.key.size =    unit(0.25, "lines"),
                strip.background =   element_blank(),
                plot.title = element_text(hjust = 0)
        )
        + xlab("")
        + ylab(expression('log(N'[2]*'O [ µg-N m'^2*' ] )'))
        # + scale_x_continuous(breaks = c(0, 15, 30, 45))
        # + coord_cartesian(ylim=c(0.1,1500))
)

g.rain <- (g.rain
        + geom_point(size=0.7, aes(color=factor(precipitation)))
        + scale_colour_manual(values=col.rain)
        
)
g.rain

###
g.event <- ggplot(data, aes(x= rank, y = log(N2O)))
g.event <- (g.event
           + theme_bw(base_size = 8)
           + theme(axis.ticks = element_line(size = 0.1),
                   legend.position =   c(0.2 ,0.25),
                   legend.key.size =    unit(0.25, "lines"),
                   strip.background =   element_blank(),
                   plot.title = element_text(hjust = 0)
           )
           + xlab("")
           + ylab(expression('log(N'[2]*'O [ µg-N m'^2*' ] )'))
           # + scale_x_continuous(breaks = c(0, 15, 30, 45))
           # + coord_cartesian(ylim=c(0.1,1500))
)

g.event <- (g.event
           + geom_point(size=0.7, aes(color=factor(event)))
           + scale_colour_manual(values=col.event)
           
)
g.event

###############
myplot <- paste0(my.dir, "/ranked_N2O_by event.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(g.till, vp = vplayout(1, 1))
print(g.event, vp = vplayout(1, 2))

print(g.rain, vp = vplayout(2, 1))
print(g.fert, vp = vplayout(2, 2))

dev.off()

