#time recording
print(  paste0(Sys.time(), " start acid_trap_calculation.R")  )
###
### input files
folder <- paste0(input_path_RAW, "/_extra")
empt <- paste0(folder, "/acid_trap_empty_weigths.dat")
tot <- paste0(folder, "/acid_trap_total_weigths.dat")
con <- paste0(folder, "/acid_trap_N_conc.dat")
issue <- paste0(folder, "/acid_trap_sampling_issues.dat")
event <- paste0(folder, "/rain_events_time.dat")
treatt <- paste0(folder, "/treatments.dat")

### output folder
folder_out <- paste0(output_path, "/acid_trap")
dir.create(folder_out, recursive = TRUE)
myfile <- paste0(folder_out, "/acid_trap.dat")

# read files
empty <- fread(empt)
total <- fread(tot)
conc <- fread(con)
issues <- fread(issue)
events <- fread(event)
treats <- fread(treatt)

# merge trap-weight and trap-empty-weight
setkey(empty, incubation, CORE, trap)
setkey(total, incubation, CORE, trap)

total <- total[empty]

# merge concentrations
setkey(total, time, incubation, CORE, trap)
setkey(conc, time, incubation, CORE, trap)
conc <- conc[total]
conc[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

# create id
conc[,id:= paste0(incubation, CORE, trap)]

# solve one of the sampling issues
issues[,time:= as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

issues[2,]
mytime <- issues[2, time]
conc[unclass(time)>unclass(mytime) & id=="C4a", flask.head:= flask.head -1.65]

# get rid of low conc: below detection limit
conc[N.conc.mgL<0.1, N.conc.mgL:= 0]
# conc[, N.conc.mgL:= 0.1]

################################################################################
### start proccesing
conc[,N.sampled:= N.conc.mgL*sampled.vol.ml]
conc[,cumN.sampled:= N.sampled]


# get cum N sampled
setkey(conc, id, time)
for(i in unique(conc$id)){
        conc[id==i & !is.na(N.sampled),cumN.sampled:= cumsum(N.sampled)]
}

conc[,vol.intrap:= before - flask.head]
conc[,N.intrap:= vol.intrap * N.conc.mgL]


# solve rest of sampling issues
issues[3:4,] # do nothing for 4 (i.e. trap b)
mytime <- issues[3, time]
myconc <- conc[unclass(time)<=unclass(mytime) & id=="C13a",tail(N.intrap,1)]
conc[unclass(time)>unclass(mytime) & id=="C13a", N.intrap:= N.intrap + myconc]

issues[1,]
mytime <- issues[1, time]
myconc <- conc[unclass(time)<=unclass(mytime) & id=="A14a",tail(N.intrap,1)]
conc[unclass(time)>unclass(mytime) & id=="A14a", N.intrap:= N.intrap + myconc]


# get cum N intrap
conc[,cumN:=N.intrap + cumN.sampled]
conc[,cumN.mg:=cumN/1000]

# include event times; create 'days' columns
events <- events[,.(
        incubation,
        CORE = COREnumber,
        tillage, fertilization, precipitation,
        RainEvent1, RainEvent2, RainEvent3
)]
events[,RainEvent1:= as.POSIXct(RainEvent1, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
events[,RainEvent2:= as.POSIXct(RainEvent2, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]
events[,RainEvent3:= as.POSIXct(RainEvent3, format = "%Y-%m-%d %H:%M:%S", tz="UTC")]

setkey(events, incubation, CORE)
setkey(conc, incubation, CORE)

conc <- events[conc]

conc[,days:= as.numeric(difftime( time, RainEvent1, units="days" ))]
conc[,days2ndEvent:= as.numeric(difftime( time, RainEvent2, units="days" ))]
conc[,days3rdEvent:= as.numeric(difftime( time, RainEvent3, units="days" ))]
conc[,days.adjusted:= as.numeric(NA)]

conc[,days.adj.full:= days]
# 
conc[days2ndEvent>=0, days.adj.full:= days2ndEvent+15]
conc[days3rdEvent>=0, days.adj.full:= days3rdEvent+30]

conc[,days.adjusted:= days.adj.full]

################################################################################
### removal of obvious outliers before exporting raw (i.e. original times) cum NH3 data

# checking plots
conc[,myid:= paste0(trap, incubation, CORE)]
par(mfrow= c(3,3))
myids <- sort(unique(conc[!is.na(cumN.mg),myid]))
for(i in myids){
        with(conc[myid==i], plot(cumN.mg ~ days, main=paste(i), ylab="", xlab=""))
        # plot(conc[myid==i, cumN.mg], main=paste(i), ylab="", xlab="")
        abline(h=c(0, 0.02), col="red")
        abline(v=c(0, 15, 30, 45))
}

conc[myid=="aB3" & cumN.mg > 0.1, c("N.intrap", "cumN.mg", "cumN"):=NA]


# export raw file
mydata <- conc[,.(incubation, CORE, trap,
                  fertilization, precipitation, tillage,
                  time, days, days2ndEvent, days3rdEvent, days.adj.full,
                  N.conc.mgL, N.sampled, cumN.sampled,
                  vol.intrap, N.intrap,
                  cumN, cumN.mg,
                  comment
                  
)]
setkey(mydata, trap, incubation, fertilization, precipitation, tillage)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "time", "comment")
to.format6 <- c("days", "days2ndEvent", "days3rdEvent", "days.adj.full", "cumN.mg")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
mydata[,(to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6)), .SDcols = to.format6]
myfile <- paste0(folder_out, "/cumNH3_inTrap_nocorrected.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

################################################################################
### correct cumN so that cum curve never decreases

# get cumN marginal increase for each sampling time
setkey(conc, myid, days)
conc[,cumN.marginal:= cumN - shift(cumN, 1, type='lag'), by=myid]

# correct sampling time close to rain events; 
# there are mistakes where acid collection is reported after rain event, which cannot be.
conc[days>0 & days<0.125, c("days", "days.adj.full"):= 0]

conc[days2ndEvent >0 & days2ndEvent<0.125, days2ndEvent:=0]
conc[days2ndEvent >0 & days2ndEvent<0.125, days.adj.full:=15]

conc[days3rdEvent >0 & days3rdEvent<0.125, days3rdEvent:= 0]
conc[days3rdEvent >0 & days3rdEvent<0.125, days.adj.full:= 30]

conc[, days.adjusted:= days.adj.full]


# create event column
conc[,event:=0]
conc[days>0, event:=1]
conc[days2ndEvent>0, event:=2]
conc[days3rdEvent>0, event:=3]

conc[,myid2:= paste0(trap, incubation, CORE, "-", event)]
id2 <- sort(unique(conc[!is.na(cumN.mg) & trap =="a" & event>0, myid2]))

        # conc[, cumN.marginal1:= cumN.marginal]
        # for(i in id2){
        #         mydata <- conc[myid2==i]
        #         if( mydata[1, cumN.marginal]<0 ){mydata[1, cumN.marginal:= 0]}
        #         target.row <- mydata[,.I[cumN.marginal<0]]
        #         target <- mydata[target.row, cumN.marginal]
        #         
        #         if( length(target.row)==0){}else{
        #                 for(j in seq_len(length(target.row))){
        #                         mydata[target.row[j], cumN.marginal:= 0]
        #                         mydata[target.row[j]-1, cumN.marginal:= cumN.marginal+target[j]/2]
        #                 }
        #                 if( mydata[1, cumN.marginal]<0 ){mydata[1, cumN.marginal:= 0]}
        #                 conc[myid2==i, cumN.marginal1:= mydata[,cumN.marginal]]  
        #         }
        # }
        # conc[!is.na(cumN.marginal1), cumN.mg.1:= cumsum(cumN.marginal1)/1000, by= myid]
        # 
        # for(i in myids){
        #         with(conc[myid==i], plot(cumN.mg.1 ~ days.adjusted, col="red", main=paste(i), ylab="", xlab=""))
        #         # plot(conc[myid==i, cumN.mg], main=paste(i), ylab="", xlab="")
        #         with(conc[myid==i], points(cumN.mg ~ days.adjusted))
        #         abline(h=c(0, 0.02), col="red")
        #         abline(v=c(0, 15, 30, 45))
        # }

### since implementing a never decreasing curve is not that straightforward, we get maximum cumN by core, event

# max cumN by .(incubation, core, event, trap)

conc[,cumN.max.event:= max(cumN, na.rm=T), by= myid2]
conc[is.na(cumN), cumN.max.event:= NA]

conc[,cumN.max.pre.event:= shift(cumN.max.event, 1, type='lag'), by=myid2]


aux <- conc[!is.na(cumN.max.event), .(
        cumN.max.event = max(cumN.max.event),
        event = unique(event),
        id = unique(id)
), by=myid2]

setkey(aux, id, event)
aux[,cumN.max.pre.event:= shift(cumN.max.event, 1, type="lag", fill=0), by=id]

aux <- aux[,.(myid2, cumN.max.pre.event)]
setkey(aux, myid2)
setkey(conc, myid2)

conc <- conc[aux]

conc[, NH3.mgNm2 := cumN.max.event/1000/area]
conc[event>1 , NH3.mgNm2 := (cumN.max.event - cumN.max.pre.event)/1000/area]
conc[NH3.mgNm2<0, NH3.mgNm2 := 0]

data <- conc[,.(
        NH3.mgNm2 = max(NH3.mgNm2, na.rm=T)
), by=.(incubation, CORE, trap, fertilization, precipitation, tillage, event)]

data[NH3.mgNm2<0, NH3.mgNm2:=0]

data <- data[event>0]
setkey(data, trap, incubation, fertilization, precipitation, tillage, CORE, event )

################################################################################
### get summaries

#######
# by cores by event (a/b)
d.core.event.export <- copy(data)
d.core.event <-  d.core.event.export[trap=="a"]


# by core (only a)
d.core <- data[,.(
        NH3.mgNm2 = sum(NH3.mgNm2, na.rm=T)
), by=.(incubation, CORE, trap, fertilization, precipitation, tillage)]

d.core.export <- copy(d.core)
d.core <- d.core[trap=="a"]

#######
# by treatment
s.treat <- d.core[,.(
        NH3 = mean(NH3.mgNm2, na.rm=T),
        NH3.sd = sd(NH3.mgNm2, na.rm=T),
        NH3.se = sd(NH3.mgNm2, na.rm=T)/sqrt(sum(!is.na(NH3.mgNm2)))
), by=.(fertilization, precipitation, tillage)]

# by treatment, by event
s.treat.ev <- d.core.event[,.(
        NH3 = mean(NH3.mgNm2, na.rm=T),
        NH3.sd = sd(NH3.mgNm2, na.rm=T),
        NH3.se = sd(NH3.mgNm2, na.rm=T)/sqrt(sum(!is.na(NH3.mgNm2)))
), by=.(fertilization, precipitation, tillage, event)]


#######
# by rain
s.rain <- d.core[,.(
        NH3 = mean(NH3.mgNm2, na.rm=T),
        NH3.sd = sd(NH3.mgNm2, na.rm=T),
        NH3.se = sd(NH3.mgNm2, na.rm=T)/sqrt(sum(!is.na(NH3.mgNm2)))
), by=.(precipitation)]

# by rain, by event
s.rain.ev <- d.core.event[,.(
        NH3 = mean(NH3.mgNm2, na.rm=T),
        NH3.sd = sd(NH3.mgNm2, na.rm=T),
        NH3.se = sd(NH3.mgNm2, na.rm=T)/sqrt(sum(!is.na(NH3.mgNm2)))
), by=.(precipitation, event)]

#######
# by fert
s.fert <- d.core[,.(
        NH3 = mean(NH3.mgNm2, na.rm=T),
        NH3.sd = sd(NH3.mgNm2, na.rm=T),
        NH3.se = sd(NH3.mgNm2, na.rm=T)/sqrt(sum(!is.na(NH3.mgNm2)))
), by=.(fertilization)]

# by fert, by event
s.fert.ev <- d.core.event[,.(
        NH3 = mean(NH3.mgNm2, na.rm=T),
        NH3.sd = sd(NH3.mgNm2, na.rm=T),
        NH3.se = sd(NH3.mgNm2, na.rm=T)/sqrt(sum(!is.na(NH3.mgNm2)))
), by=.(fertilization)]

#######
# by tillage
s.tillage <- d.core[,.(
        NH3 = mean(NH3.mgNm2, na.rm=T),
        NH3.sd = sd(NH3.mgNm2, na.rm=T),
        NH3.se = sd(NH3.mgNm2, na.rm=T)/sqrt(sum(!is.na(NH3.mgNm2)))
), by=.(tillage)]

# by tillage, by event
s.tillage.ev <- d.core.event[,.(
        NH3 = mean(NH3.mgNm2, na.rm=T),
        NH3.sd = sd(NH3.mgNm2, na.rm=T),
        NH3.se = sd(NH3.mgNm2, na.rm=T)/sqrt(sum(!is.na(NH3.mgNm2)))
), by=.(tillage, event)]

################################################################################
### write files

# by core, event
mydata <- copy(d.core.event.export)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "event")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NH3_mgNm2_by_core_event.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# by core
mydata <- copy(d.core.export)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "event")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NH3_mgNm2_by__core.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)



# by tretament, event
mydata <- copy(s.treat.ev)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "event")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NH3_mgNm2_by_treatment_event.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# by tretament
mydata <- copy(s.treat)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "event")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NH3_mgNm2_by__treatment.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)



# by rain, event
mydata <- copy(s.rain.ev)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "event")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NH3_mgNm2_by_rain_event.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# by rain
mydata <- copy(s.rain)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "event")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NH3_mgNm2_by__rain.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)



# by fert, event
mydata <- copy(s.fert.ev)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "event")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NH3_mgNm2_by_fert_event.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# by fert
mydata <- copy(s.fert)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "event")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NH3_mgNm2_by__fert.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)



# by tillage, event
mydata <- copy(s.tillage.ev)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "event")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NH3_mgNm2_by_tillage_event.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)

# by tillage
mydata <- copy(s.tillage)
no.format <- c("incubation", "CORE", "trap", "tillage", "fertilization", "precipitation", "event")
to.format4 <- names(mydata)[!names(mydata) %in% c(no.format, to.format6)]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder_out, "/NH3_mgNm2_by__tillage.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)
