# output files
folder.in <- paste0(output_path, "/CO2vsNflux")
myfile <- paste0(folder.in, "/instant_flux_quantiles.dat")
data <- fread(myfile)


data <- data[,.(
        days = days,
        fertilization = factor(fertilization),
        precipitation =factor(precipitation),
        tillage = factor(tillage),
        incubation = factor(incubation),
        event = factor(event),
        NO, N2O, CO2, CH4, NON2O,
        qCO2 = qCO2.event,
        qNO = qNO.event,
        qN2O = qN2O.event,
        qNON2O = qNON2O.event
)]


###
d1 <- data[qNO>80]
d1[,species:= "NO"]
d2 <- data[qN2O>80]
d2[,species:= "N2O"]
mydata <- rbindlist(list(d1,d2))

################################################################################
# pairwise comparisons
require(multcomp)
require(car)
require(HH)
require(gvlma)
require(bootstrap)
require(MASS)
require(leaps)
require(coin)
require(lmPerm)

### by=.(event, precipitation)
mytest <- mydata[,.(
        p.oneway = as.numeric(NA),
        p.adj9.oneway.bonf = as.numeric(NA),
        p.adj9.oneway.holm = as.numeric(NA),
        
        p.wilcox = as.numeric(NA),
        p.adj9.wilcox.bonf = as.numeric(NA),
        p.adj9.wilcox.holm = as.numeric(NA)
), by=.(event, species)]

for(i in unique(mytest$event)){
        for(j in mytest$species){
                d <- mydata[event==i & species == j,]
                
                myp <- with(d, oneway_test(qCO2~tillage, distribution=approximate(B = 9999)))
                myp<- pvalue(myp)[1]
                mytest[event==i & species == j, p.oneway:= myp]
                
                myp <- with(d, wilcox_test(qCO2~tillage, distribution=approximate(B = 9999)))
                myp<- pvalue(myp)[1]
                mytest[event==i & species == j, p.wilcox:= myp]
        }
}
mytest[, p.adj9.oneway.bonf:= p.adjust(p.oneway, method="bonferroni", n=9)]
mytest[, p.adj9.oneway.holm:= p.adjust(p.oneway, method="holm", n=9)]

mytest[, p.adj9.wilcox.bonf:= p.adjust(p.wilcox, method="bonferroni", n=9)]
mytest[, p.adj9.wilcox.holm:= p.adjust(p.wilcox, method="holm", n=9)]

pairtest.till.byevent.species <- copy(mytest)


###
mydata[,event:= factor(event)]
mydata[,species:= factor(species)]
mytest <- mydata[,.(
        p.oneway = as.numeric(NA),
        p.adj9.oneway.bonf = as.numeric(NA),
        p.adj9.oneway.holm = as.numeric(NA),
        
        p.wilcox = as.numeric(NA),
        p.adj9.wilcox.bonf = as.numeric(NA),
        p.adj9.wilcox.holm = as.numeric(NA)
), by=.(event)]

for(i in unique(mytest$event)){
        d <- mydata[event==i,]
        
        myp <- with(d, oneway_test(qCO2~species, distribution=approximate(B = 9999)))
        myp<- pvalue(myp)[1]
        mytest[event==i, p.oneway:= myp]
        
        myp <- with(d, wilcox_test(qCO2~species, distribution=approximate(B = 9999)))
        myp<- pvalue(myp)[1]
        mytest[event==i, p.wilcox:= myp]
}
mytest[, p.adj9.oneway.bonf:= p.adjust(p.oneway, method="bonferroni", n=9)]
mytest[, p.adj9.oneway.holm:= p.adjust(p.oneway, method="holm", n=9)]

mytest[, p.adj9.wilcox.bonf:= p.adjust(p.wilcox, method="bonferroni", n=9)]
mytest[, p.adj9.wilcox.holm:= p.adjust(p.wilcox, method="holm", n=9)]

pairtest.till.species <- copy(mytest)

# write summary files
to.export <- copy (pairtest.till.byevent.species)
no.format <- c("event", "species", "tillage")
to.format2 <- names(to.export)[!names(to.export) %in% no.format]
to.export[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 3)), .SDcols = to.format2]
myfile <- paste0(folder.in, "/_pairwise_tillage_test_byevent_species.dat")
write.table(to.export, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

to.export <- copy (pairtest.till.species)
no.format <- c("event", "species", "tillage")
to.format2 <- names(to.export)[!names(to.export) %in% no.format]
to.export[,(to.format2):= lapply(.SD, function(x) formatC(x, format = "f", digits = 3)), .SDcols = to.format2]
myfile <- paste0(folder.in, "/_pairwise_species_test_byevent.dat")
write.table(to.export, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

pairtest.till.byevent.species[, x:= c(1,2,1,2,1,2)]
pairtest.till.byevent.species[, y:= 105]
pairtest.till.byevent.species[, label:= c("(***)", "(.)", "ns", "(***)", "(*)",  "ns")]


pairtest.till.species[, x:= 1.5]
pairtest.till.species[, y:= 0]
pairtest.till.species[, label:= c("(***)", "ns", "(***)")]
################################################################################

mydata[, fertilizer:= factor(fertilization,levels = c(0, 50, 100), labels=c("0   kg-N/ha", "50   kg-N/ha", "100   kg-N/ha"))]
mydata[, precipitation:= factor(precipitation,levels = c("cons", "incr", "decr"), labels=c("homogeneous", "increasing", "decreasing"))]
mydata[, tillage:= factor(tillage, levels = c("NT", "T"), labels=c("no tillage", "traditional tillage"))]
mydata[, event:= factor(event, levels = c(1,2,3), labels=c("1st stretch", "2nd stretch", "3rd stretch"))]

pairtest.till.byevent.species[,tillage:= "no tillage"]
pairtest.till.byevent.species[,qCO2:= 105]
pairtest.till.byevent.species[, event:= factor(event, levels = c(1,2,3), labels=c("1st stretch", "2nd stretch", "3rd stretch"))]

pairtest.till.species[,tillage:= "no tillage"]
pairtest.till.species[,species:= "NO"]
pairtest.till.species[,qCO2:= -5]
pairtest.till.species[, event:= factor(event, levels = c(1,2,3), labels=c("1st stretch", "2nd stretch", "3rd stretch"))]
pairtest.till.species[2, label:= "ns  "]

col.rain <- c("white", "deepskyblue","dodgerblue4")
col.fert <- c("white","olivedrab2", "olivedrab4")
col.till <- c("grey","red")



g.1a <- ggplot(mydata, aes(x = species, y=qCO2, fill=tillage))
g.1a <- (g.1a
                 + facet_wrap( ~ event, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab(expression('CO'[2]*' quantile [%]'))
         + xlab("Q80 - Q100")
         # + labs(title = "(a) \n  ")
         + coord_cartesian(ylim=c(-10,110))
)

g.1a <- (g.1a
         + geom_boxplot(size=0.15, outlier.colour = NA)
         + scale_fill_manual(values = col.till)
#          + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
#          + scale_colour_manual(values=col.till)
+ geom_text(data = pairtest.till.byevent.species, aes(label=label), hjust= 0.5, size=2.5)
+ geom_text(data = pairtest.till.species, aes(label=label), hjust= 3, size=2.5)
)
g.1a

myplot <- paste0(folder.in, "/_qCO2_vs_qN80_bytillage.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)
print(g.1a)
dev.off()
# 

g.1a <- ggplot(mydata, aes(x = species, y=qCO2, fill=precipitation))
g.1a <- (g.1a
         + facet_wrap( ~ event, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab(expression('CO'[2]*' quantile [%]'))
         + xlab("quantiles 80-100 %")
         # + labs(title = "(a) \n  ")
)

g.1a <- (g.1a
         + geom_boxplot(size=0.15, outlier.colour = NA)
         + scale_fill_manual(values = col.rain)
         #          + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         #          + scale_colour_manual(values=col.till)
)
g.1a

myplot <- paste0(folder.in, "/_qCO2_vs_qN80_byrain.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)
print(g.1a)
dev.off()
# 
# 

g.1a <- ggplot(mydata, aes(x = species, y=qCO2, fill=fertilization))
g.1a <- (g.1a
         + facet_wrap( ~ event, scales = "fixed", ncol = 3)
         + theme_bw(base_size = 8)
         + theme(plot.margin= unit(c(0.1, 0.5, 0, 0), "lines"),
                 axis.ticks = element_line(size = 0.1),
                 legend.position =   "none",
                 panel.margin = unit(0.1, "lines"),
                 strip.background = element_blank(),
                 #                         strip.text.x = element_blank(),
                 plot.title = element_text(hjust = 0)
         )
         + ylab(expression('CO'[2]*' quantile [%]'))
         + xlab("quantiles 80-100 %")
         # + labs(title = "(a) \n  ")
)

g.1a <- (g.1a
         + geom_boxplot(size=0.15, outlier.colour = NA)
         + scale_fill_manual(values = col.fert)
         #          + geom_point(aes(colour=factor(tillage)), position=position_dodge(width=0.75), size= 1 )
         #          + scale_colour_manual(values=col.till)
)
g.1a

myplot <- paste0(folder.in, "/_qCO2_vs_qN80_byfert.png")
png(filename=myplot,  width = 160, height = 80, units = "mm", res=1200)
print(g.1a)
dev.off()


################################################################################
# ANOVA
# require(multcomp)
# require(car)
# require(HH)
# require(gvlma)
# require(bootstrap)
# require(MASS)
# require(leaps)
# require(coin)
# require(lmPerm)
# 
# # define output directory
# my.output.dir <- paste0(folder.in,"/_ANOVA")
# dir.create(my.output.dir)
# 
# d1 <- data[qNO>80]
# d1[,species:= "NO"]
# d2 <- data[qN2O>80]
# d2[,species:= "N2O"]
# data <- rbindlist(list(d1,d2))
# data[,id:= paste0(incubation, fertilization, precipitation, tillage)]
# 
# # export to html
# my.files <- list.files("_ANOVA_CO2_quantiles", pattern = ".Rmd", full.names = TRUE)
# my.files
# render(input = my.files[1], output_format = "html_document", output_dir = my.output.dir)



