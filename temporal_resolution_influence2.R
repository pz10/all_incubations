# output files
folder.out <- paste0(output_path, "/cum_fluxes_14/temporal_resolution_influence")
dir.create(folder.out)

myA1st <- paste0(folder.out, "/A_1st_18h.dat")
# myNO <- paste0(folder.out, "/NO_A_1st.dat")
# myN2O <- paste0(folder.out, "/N2O_A_1st.dat")
# myCO2 <- paste0(folder.out, "/CO2_A_1st.dat")


# flux files
A.flux.file <- paste0(input_path, "/A_IDASw/A_fluxes/A_fluxes.dat")
# B.flux.file <- paste0(input_path, "/B_IDASw/B_fluxes/B_fluxes.dat")
# C.flux.file <- paste0(input_path, "/C_IDASw/C_fluxes/C_fluxes.dat")
# D.flux.file <- paste0(input_path, "/D_IDASw/D_fluxes/D_fluxes.dat")
treatments <- paste0(input_path_RAW, "/_extra/treatments.dat")

# read data files
Aflux <- fread(input = A.flux.file)
# Bflux <- fread(input = B.flux.file) 
# Cflux <- fread(input = C.flux.file) 
# Dflux <- fread(input = D.flux.file)
treatments <- fread(input = treatments)

setkey(Aflux, time)
# setkey(Bflux, time)
# setkey(Cflux, time)
# setkey(Dflux, time)
setkey(Aflux, treatment, time)
# setkey(Bflux, treatment, time)
# setkey(Cflux, treatment, time)
# setkey(Dflux, treatment, time)

# initial parameters
step <- 0.125 # time points where to calculate cumulative values
lastday <- 14 # last time point to be calculated
minNO <- 0 #minimum flux to be taken into account
minN2O <- 0 #minimum flux to be taken into account
minCO2 <- 0 #minimum flux to be taken into account
minCH4 <- 0 #minimum flux to be taken into account

offsetNO <- 0
offsetN2O <- 25
offsetCO2 <- 15
offsetCH4 <- 100 # we want everything in
# 
# source("1st_event_negative_IN.R")
# source("2nd_event_negative_IN.R")
# source("3rd_event_negative_IN.R")


##################
# only 1st event
Aflux <- Aflux[days>0 & days<14,.(days,CORE, fertilizer, precipitation, tillage,NO, N2O, CO2)]
# source("fluxD24.R")

simulations <- 10000
separation <- 0.75 #days
freq <- 28 # times in 14 days
night <- 12 #in hours; for sampling restriction
resol<- 12 #temporal resolution of sample, in hours e.g. 3h
species <- c("NO", "N2O", "CO2")

# NO

# 1. get original cum flux during 1st event
################################################################################
####
mycores <- sort(unique(Aflux$CORE))
mytimes <- seq(from = 0, to = 14, by =step)

long <- length(species)*length(mycores)
data <- data.table(species=as.character(rep(NA, times=long)),
                   fertilizer = as.numeric(NA),
                   precipitation = as.character(NA),
                   tillage = as.character(NA),
                   simulation=-99, cumflux=as.numeric(NA)
                   )

count <-0
for (i in unique(Aflux$CORE)){
        count <- count+1
        
        myflux <- Aflux[CORE==i]
        
        myzero <- myflux[1,]
        myzero[, days:=0]
        myzero[, NO:=0]
        myzero[, N2O:=0]
        myzero[, CO2:=0]
        # myzero[, CH4:=0]
        
        mylast <- tail(myflux[!is.na(NO) & !is.na(N2O) & !is.na(CO2)],1)
        mylast[,days:=14]
        myflux <- rbindlist(list(myzero, myflux, mylast))
        
        
        
        if(count==1){
                flux <- myflux
        }else{
                flux <- rbindlist(list(flux, myflux))
        }
        
}

count <-0
for(myspecies in unique(species)){
        for(j in 1:18){
                count <- count+1
                
                mycore <- mycores[j]
                
                # select target data
                mydata <- flux[!is.na(get(myspecies))
                               & CORE==mycore
                               , .(days, flux=get(myspecies), fertilizer, precipitation, tillage)]
                
                myfert <- mydata[1, fertilizer]
                myrain <- mydata[1, precipitation]
                mytill <- mydata[1, tillage]
                
                # insert origin (days == 0), with value as last measurement before origin
                # myorigin <- mydata[!is.na(NO)
                #                         & days <= 0
                #                         & treatment==m
                #                         , list(days, treatment, NO)]
                # 
                # myorigin <- tail(myorigin, 1)
                # mydata <- rbind(myorigin, mydata)
                # mydata[1, days:= 0]
                
                # calculate cumulative fluxes ('cum') for target data
                mydata[,hours:= days*24]
                mydata[flux < 0, flux:= 0]
                
                mycum <- auc(mydata$hours,mydata$flux, thresh=0)

                data[count,species:= myspecies]
                data[count,fertilizer:= myfert]
                data[count,precipitation:= myrain]
                data[count,tillage:= mytill]
                data[count,cumflux:= mycum]
        }
}
original <- copy(data)
setkey(original, species, fertilizer, precipitation, tillage)
################################################################################

# 2. make simulations
################################################################################

long <- length(species)*length(mycores)*simulations

data <- data.table(species=as.character(rep(NA, times=long)),
                   fertilizer = as.numeric(NA),
                   precipitation = as.character(NA),
                   tillage = as.character(NA),
                   simulation=-99, cumflux=as.numeric(NA)
)

count <-0
for(s in 1:simulations){
        for (mycore in unique(Aflux$CORE)){
                ### 1 get the random sample
                myflux <- Aflux[CORE==mycore]
                
                myzero <- myflux[1,]
                myzero[, days:=0]
                myzero[, NO:=0]
                myzero[, N2O:=0]
                myzero[, CO2:=0]
                
                mydays <- myflux$days
                
                # get 1st value
                myid <- min(mydays)
#                # mydays <- mydays[mydays>(myid+separation) | mydays<(myid-separation)]
                
                # random sample with conditions (choose one)
                
                # # a. "nights" out
                # night.start <- sample((0:100)/200,1)
                # mynights <- (mydays%%1 > night.start) & (mydays%%1 < (night.start+night/24))
                # # mynights <- mydays%%1 > (1-night/24)
                # mydays <- mydays[!mynights]
                # for(j in 1:(freq-1)){
                #         id <- sample(mydays, 1)
                #         myid <- c(myid, id)
                #         out <- floor(mydays) == floor(id)
                #         mydays <- mydays[!out]
                # }
                # # 
               
                # # b. 18 hours separation restriction, random
                # for(j in 1:(freq-1)){
                #         id <- sample(mydays, 1)
                #         myid <- c(myid, id)
                #         mydays <- mydays[mydays>(id+separation) | mydays<(id-separation)]
                # }
                
                # c. every span hours
                my.random <- sample(1:(100*resol/24)/100,1)
                mytimes <- seq(from=my.random, to=14, by=resol/24)
                for(t in mytimes){
                        id <- mydays[mydays>t] [1]
                        if(!is.na(id)){
                                myid <- c(myid, id)
                        }
                }
                
                
                #
                myid <- sort(myid)
                myflux <- myflux[days %in% myid,]
                
                mylast <- tail(myflux[!is.na(NO) & !is.na(N2O) & !is.na(CO2)],1)
                mylast[,days:=14]
                myflux <- rbindlist(list(myzero, myflux, mylast))
                
                ### calculate cum flux
                for(myspecies in unique(species)){
                        count <- count+1
                        mydata <- myflux[!is.na(get(myspecies))
                                       & CORE==mycore
                                       , .(days, flux=get(myspecies), fertilizer, precipitation, tillage)]
                        
                        myfert <- mydata[1, fertilizer]
                        myrain <- mydata[1, precipitation]
                        mytill <- mydata[1, tillage]
                        
                        mydata[,hours:= days*24]
                        mydata[flux < 0, flux:= 0]
                        
                        mycum <- auc(mydata$hours,mydata$flux, thresh=0)
                        
                        data[count,species:= myspecies]
                        data[count,fertilizer:= myfert]
                        data[count,precipitation:= myrain]
                        data[count,tillage:= mytill]
                        
                        data[count, simulation:=s]
                        data[count,cumflux:= mycum]
                }
        }
        
        rm(myflux)
        cat(paste(s," "))
        
}
data <- rbindlist(list(original, data))
################################################################################
#write into output folder
setkey(data, species, fertilizer, precipitation, tillage, simulation)
data[,cumflux:=cumflux/1000]
mydata <- copy(data)
mydata[,cumflux:=formatC(cumflux, format = "f", digits = 4)]
   
write.table(mydata, file= myA1st, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")


# visualization
################################################################################
# NO

data <- fread(input = myA1st)
with(data, boxplot(cumflux, by="species"))

data[, treatment:= paste(fertilizer, precipitation, tillage, sep="-")]
data[, ref.cumflux:= cumflux[1], by=.(species, treatment)]
data[simulation==-99,.(cumflux, ref.cumflux), by=.(species, treatment)]
data[, norm.cumflux:= 100*cumflux/ref.cumflux]

data[,myorder:= median(norm.cumflux, na.rm=T), by= .(species, treatment)]
setkey(data, myorder)

data[,order.byspecies:= 0]
count <- 1
for(h in unique(data$species)){
        for(i in unique(data[species == h, myorder])){
                data[myorder==i, order.byspecies:= count]
                count <- count+1
        }
        count <- 1
}

data[, mean:=mean(norm.cumflux), by= .(species, treatment)]
data[, q05:=quantile(norm.cumflux, probs=0.05), by= .(species, treatment)]
data[, q95:=quantile(norm.cumflux, probs=0.95), by= .(species, treatment)]

quantiles <- data[,.(q05=mean(q05), q95=mean(q95)), by= .(species, treatment)]
quant.species <- data[,.(q05=mean(q05), q95=mean(q95)), by= .(species)]

# plots
################################################################################
# all cores by species
g.1 <- ggplot(data, aes(x= species,y = norm.cumflux))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                # axis.text.x = element_blank(),
                # axis.ticks.x =  element_blank(),
                plot.margin= unit(c(0, 0, 0.25, 0.25), "lines"),
                plot.title = element_text(face="bold", hjust=0))
        + ylab("cumulative flux / reference flux  [%]")
        + xlab("soil containers")
        + geom_hline(yintercept=100, col="red")
        + geom_boxplot(size=0.15, outlier.size=0.15, width=0.5)
        # + geom_point(aes(y= mean), col="red")
        + geom_point(data=quant.species, aes(x= species,y= q05), col="red")
        + geom_point(data=quant.species, aes(x= species,y= q95), col="red")
        # + scale_x_continuous(breaks = c(0.5, 6.5, 12.5, 18.5))
        # + coord_cartesian(ylim=c(-18,25), xlim= c(0,19))
        # + ggtitle(paste0("(NO)"))
        # + geom_text(data = mylabels, x=mylabels$newcore, y= mylabels$yaxis, label = paste0("(",mylabels$adjustments, ")"), hjust=0.5, vjust=0.5, size = 2.25)
        #       + scale_x_continuous(breaks = c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5, 18.5))
        
)
# g.1
myplot <- paste0(folder.out,"/byspecies_allcores.png")
png(filename=myplot,  width = 80, height = 60, units = "mm", res=1200)

print(g.1)

dev.off()
###
################################################################################
# NO
g.1 <- ggplot(data[species=="NO"], aes(x= order.byspecies, y = norm.cumflux))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                axis.text.x = element_blank(),
                axis.ticks.x =  element_blank(),
                plot.margin= unit(c(0, 0, 0.25, 0.25), "lines"),
                plot.title = element_text(face="bold", hjust=0))
        + ylab("cumulative flux / reference flux  [%]")
        + xlab("soil containers")
        + geom_hline(yintercept=100, col="red")
        + geom_boxplot(aes(group=order.byspecies), size=0.15, outlier.size=0.25)
        # + geom_point(aes(y= mean), col="red")
        # + geom_point(aes(y= q05), col="red")
        # + geom_point(aes(y= q95), col="red")
        # + scale_x_continuous(breaks = c(0.5, 6.5, 12.5, 18.5))
        # + coord_cartesian(ylim=c(-18,25), xlim= c(0,19))
        + ggtitle(paste0("(NO)"))
        # + geom_text(data = mylabels, x=mylabels$newcore, y= mylabels$yaxis, label = paste0("(",mylabels$adjustments, ")"), hjust=0.5, vjust=0.5, size = 2.25)
        #       + scale_x_continuous(breaks = c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5, 18.5))
        
)
# g.1
myplot <- paste0(folder.out,"/NO.png")
png(filename=myplot,  width = 160, height = 60, units = "mm", res=1200)

print(g.1)

dev.off()

### x= magnitude of emission
g.1 <- ggplot(data[species=="NO"], aes(x= ref.cumflux/100, y = norm.cumflux))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                # axis.text.x = element_blank(),
                # axis.ticks.x =  element_blank(),
                plot.margin= unit(c(0, 0, 0.25, 0.25), "lines"),
                plot.title = element_text(face="bold", hjust=0))
        + ylab("cum. NO flux (daily sampling freq.) \n/ \nreference cum. NO flux (3h sampling freq.) \n[%]")
        + xlab("soil containers")
        + geom_hline(yintercept=100, col="red")
        + geom_boxplot(aes(group=order.byspecies), size=0.15, outlier.size=0.15, width=3/100)
        # + geom_point(aes(y= mean), col="red")
        # + geom_point(aes(y= q05), col="red")
        # + geom_point(aes(y= q95), col="red")
        # + scale_x_continuous(breaks = c(0.5, 6.5, 12.5, 18.5))
        # + coord_cartesian(ylim=c(-18,25), xlim= c(0,19))
        # + ggtitle(paste0("(NO)"))
        + xlab(expression('kg NO-N ha'^-1*' in 14 days'))
        # + geom_text(data = mylabels, x=mylabels$newcore, y= mylabels$yaxis, label = paste0("(",mylabels$adjustments, ")"), hjust=0.5, vjust=0.5, size = 2.25)
        #       + scale_x_continuous(breaks = c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5, 18.5))
        
)
g.1
myplot <- paste0(folder.out,"/NO_by_magnitude.png")
png(filename=myplot,  width = 160, height = 70, units = "mm", res=1200)

print(g.1)

dev.off()

###
################################################################################
# N2O
g.1 <- ggplot(data[species=="N2O"], aes(x= order.byspecies, y = norm.cumflux))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                axis.text.x = element_blank(),
                axis.ticks.x =  element_blank(),
                plot.margin= unit(c(0, 0, 0.25, 0.25), "lines"),
                plot.title = element_text(face="bold", hjust=0))
        + ylab("cumulative flux / reference flux  [%]")
        + xlab("soil containers")
        + geom_hline(yintercept=100, col="red")
        + geom_boxplot(aes(group=order.byspecies), size=0.15, outlier.size=0.25)
        # + geom_point(aes(y= mean), col="red")
        # + geom_point(aes(y= q05), col="red")
        # + geom_point(aes(y= q95), col="red")
        # + scale_x_continuous(breaks = c(0.5, 6.5, 12.5, 18.5))
        # + coord_cartesian(ylim=c(-18,25), xlim= c(0,19))
        + ggtitle(paste0("(N2O)"))
        
        # + geom_text(data = mylabels, x=mylabels$newcore, y= mylabels$yaxis, label = paste0("(",mylabels$adjustments, ")"), hjust=0.5, vjust=0.5, size = 2.25)
        #       + scale_x_continuous(breaks = c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5, 18.5))
        
)
# g.1
myplot <- paste0(folder.out,"/N2O.png")
png(filename=myplot,  width = 160, height = 60, units = "mm", res=1200)

print(g.1)

dev.off()

### x= magnitude of emission
g.1 <- ggplot(data[species=="N2O"], aes(x= log(ref.cumflux/100), y = norm.cumflux))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                # axis.text.x = element_blank(),
                # axis.ticks.x =  element_blank(),
                plot.margin= unit(c(0, 0, 0.25, 0.25), "lines"),
                plot.title = element_text(face="bold", hjust=0))
        + ylab("cumulative flux / reference flux  [%]")
        + xlab("soil containers")
        + geom_hline(yintercept=100, col="red")
        + geom_boxplot(aes(group=order.byspecies), size=0.15, outlier.size=0.15, width=3/100)
        # + geom_point(aes(y= mean), col="red")
        # + geom_point(aes(y= q05), col="red")
        # + geom_point(aes(y= q95), col="red")
        # + scale_x_continuous(breaks = c(0.5, 6.5, 12.5, 18.5))
        # + coord_cartesian(ylim=c(-18,25), xlim= c(0,19))
        + ggtitle(paste0("(N2O)"))
        + xlab(expression('mg-N m-2 h-1 in 14 days'))
        # + geom_text(data = mylabels, x=mylabels$newcore, y= mylabels$yaxis, label = paste0("(",mylabels$adjustments, ")"), hjust=0.5, vjust=0.5, size = 2.25)
        #       + scale_x_continuous(breaks = c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5, 18.5))
        
)
# g.1
myplot <- paste0(folder.out,"/N2O_by_magnitude.png")
png(filename=myplot,  width = 160, height = 60, units = "mm", res=1200)

print(g.1)

dev.off()
###
### x= magnitude of emission
g.1 <- ggplot(data[species=="NO"], aes(x= ref.cumflux/100, y = norm.cumflux))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                # axis.text.x = element_blank(),
                # axis.ticks.x =  element_blank(),
                plot.margin= unit(c(0, 0, 0.25, 0.25), "lines"),
                plot.title = element_text(face="bold", hjust=0))
        + ylab("cum. NO flux (daily sampling freq.) \n/ \nreference cum. NO flux (3h sampling freq.) \n[%]")
        + xlab("soil containers")
        + geom_hline(yintercept=100, col="red")
        + geom_boxplot(aes(group=order.byspecies), size=0.15, outlier.size=0.15, width=3/100)
        # + geom_point(aes(y= mean), col="red")
        # + geom_point(aes(y= q05), col="red")
        # + geom_point(aes(y= q95), col="red")
        # + scale_x_continuous(breaks = c(0.5, 6.5, 12.5, 18.5))
        # + coord_cartesian(ylim=c(-18,25), xlim= c(0,19))
        # + ggtitle(paste0("(NO)"))
        + xlab(expression('kg NO-N ha'^-1*' in 14 days'))
        # + geom_text(data = mylabels, x=mylabels$newcore, y= mylabels$yaxis, label = paste0("(",mylabels$adjustments, ")"), hjust=0.5, vjust=0.5, size = 2.25)
        #       + scale_x_continuous(breaks = c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5, 18.5))
        
)
g.1
myplot <- paste0(folder.out,"/NO_by_magnitude.png")
png(filename=myplot,  width = 160, height = 70, units = "mm", res=1200)

print(g.1)

dev.off()
################################################################################
# CO2
g.1 <- ggplot(data[species=="CO2"], aes(x= order.byspecies, y = norm.cumflux))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                axis.text.x = element_blank(),
                axis.ticks.x =  element_blank(),
                plot.margin= unit(c(0, 0, 0.25, 0.25), "lines"),
                plot.title = element_text(face="bold", hjust=0))
        + ylab("cumulative flux / reference flux  [%]")
        + xlab("soil containers")
        + geom_hline(yintercept=100, col="red")
        + geom_boxplot(aes(group=order.byspecies), size=0.15, outlier.size=0.25)
        # + geom_point(aes(y= mean), col="red")
        # + geom_point(aes(y= q05), col="red")
        # + geom_point(aes(y= q95), col="red")
        # + scale_x_continuous(breaks = c(0.5, 6.5, 12.5, 18.5))
        # + coord_cartesian(ylim=c(-18,25), xlim= c(0,19))
        + ggtitle(paste0("(CO2)"))
        # + geom_text(data = mylabels, x=mylabels$newcore, y= mylabels$yaxis, label = paste0("(",mylabels$adjustments, ")"), hjust=0.5, vjust=0.5, size = 2.25)
        #       + scale_x_continuous(breaks = c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5, 18.5))
        
)
# g.1
myplot <- paste0(folder.out,"/CO2.png")
png(filename=myplot,  width = 160, height = 60, units = "mm", res=1200)

print(g.1)

dev.off()

### x= magnitude of emission
g.1 <- ggplot(data[species=="CO2"], aes(x= ref.cumflux, y = norm.cumflux))
g.1 <- (g.1
        + theme_bw(base_size = 8)
        + theme(axis.ticks = element_line(size = 0.1),
                # axis.text.x = element_blank(),
                # axis.ticks.x =  element_blank(),
                plot.margin= unit(c(0, 0, 0.25, 0.25), "lines"),
                plot.title = element_text(face="bold", hjust=0))
        + ylab("cumulative flux / reference flux  [%]")
        + xlab("soil containers")
        + geom_hline(yintercept=100, col="red")
        + geom_boxplot(aes(group=order.byspecies), size=0.15, outlier.size=0.15, width=3)
        # + geom_point(aes(y= mean), col="red")
        # + geom_point(aes(y= q05), col="red")
        # + geom_point(aes(y= q95), col="red")
        # + scale_x_continuous(breaks = c(0.5, 6.5, 12.5, 18.5))
        # + coord_cartesian(ylim=c(-18,25), xlim= c(0,19))
        + ggtitle(paste0("(CO2)"))
        + xlab(expression('g-C m-2 h-1 in 14 days'))
        # + geom_text(data = mylabels, x=mylabels$newcore, y= mylabels$yaxis, label = paste0("(",mylabels$adjustments, ")"), hjust=0.5, vjust=0.5, size = 2.25)
        #       + scale_x_continuous(breaks = c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5, 18.5))
        
)
# g.1
myplot <- paste0(folder.out,"/CO2_by_magnitude.png")
png(filename=myplot,  width = 160, height = 60, units = "mm", res=1200)

print(g.1)

dev.off()

################################################################################
# summaries
setkey(data, species, fertilizer, precipitation, tillage)
sum.treat.species <-
        data[,.(original= ref.cumflux[1],
                q05=quantile(cumflux, probs=0.05),
                q25=quantile(cumflux, probs=0.25),
                q50=quantile(cumflux, probs=0.5),
                q75=quantile(cumflux, probs=0.75),
                q95=quantile(cumflux, probs=0.95),
                
                original.norm= 100,
                q05.norm=quantile(norm.cumflux, probs=0.05),
                q25.norm=quantile(norm.cumflux, probs=0.25),
                q50.norm=quantile(norm.cumflux, probs=0.5),
                q75.norm=quantile(norm.cumflux, probs=0.75),
                q95.norm=quantile(norm.cumflux, probs=0.95)
        ), by= .(species, fertilizer, precipitation, tillage)]

sum.species <-
        data[,.(original.norm= 100,
                q05.norm=quantile(norm.cumflux, probs=0.05),
                q25.norm=quantile(norm.cumflux, probs=0.25),
                q50.norm=quantile(norm.cumflux, probs=0.5),
                q75.norm=quantile(norm.cumflux, probs=0.75),
                q95.norm=quantile(norm.cumflux, probs=0.95)
        ), by= .(species)]


# write files
myfile <- paste0(folder.out,"/summary_bySpecies.dat")
mydata <- copy(sum.species)

no.format <- c("species", "fertilizer", "precipitation", "tillage", "original.norm")
no.format <- names(mydata) %in% no.format
to.format <- names(mydata)[!no.format]
mydata[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format]
write.table(mydata, file= myfile, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)

#
myfile <- paste0(folder.out,"/summary_byTreatment_bySpecies.dat")
mydata <- copy(sum.treat.species)

no.format <- c("species", "fertilizer", "precipitation", "tillage", "original.norm")
no.format <- names(mydata) %in% no.format
to.format <- names(mydata)[!no.format]
mydata[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format]
write.table(mydata, file= myfile, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
