# output files
folder.out1 <-paste0(output_path, "/cum_fluxes_full")
dir.create(folder.out1)
cum.flux.1st.out <- paste0(folder.out1, "/1st_event_cum_fluxes.dat")
cum.flux.2nd.out <- paste0(folder.out1, "/2nd_event_cum_fluxes.dat")
cum.flux.3rd.out <- paste0(folder.out1, "/3rd_event_cum_fluxes.dat")
cum.flux.all.out <- paste0(folder.out1, "/all_events_cum_fluxes.dat")
cum.flux.every.out <- paste0(folder.out1, "/every_event_cum_fluxes.dat")

folder.out2 <-paste0(output_path, "/cum_fluxes_14")
dir.create(folder.out2)
cum.flux.1st.14 <- paste0(folder.out2, "/1st_event_cum14_fluxes.dat")
cum.flux.2nd.14 <- paste0(folder.out2, "/2nd_event_cum14_fluxes.dat")
cum.flux.3rd.14 <- paste0(folder.out2, "/3rd_event_cum14_fluxes.dat")
cum.flux.1st.29 <- paste0(folder.out2, "/1st_event_cum29_fluxes.dat")
cum.flux.1st.44 <- paste0(folder.out2, "/1st_event_cum44_fluxes.dat")

to.write <- c(cum.flux.1st.out, cum.flux.2nd.out, cum.flux.3rd.out, cum.flux.all.out, cum.flux.every.out,
              cum.flux.1st.14, cum.flux.2nd.14, cum.flux.3rd.14, cum.flux.1st.29, cum.flux.1st.44)
# input files
cum.flux.1st <- paste0(output_path, "/4replicates_together/1st_event_cum_fluxes.dat")
cum.flux.2nd <- paste0(output_path, "/4replicates_together/2nd_event_cum_fluxes.dat")
cum.flux.3rd <- paste0(output_path, "/4replicates_together/3rd_event_cum_fluxes.dat")

flux1 <- fread(input = cum.flux.1st)
flux2 <- fread(input = cum.flux.2nd) 
flux3 <- fread(input = cum.flux.3rd)

setkey(flux1, treatment, days)
setkey(flux2, treatment, days2ndEvent)
setkey(flux3, treatment, days3rdEvent)

################################################################################
# copy flux 1:3 for later cumulative flux calculation for all events
flux.1 <- copy(flux1)
flux.12 <- copy(flux2)
flux.123 <- copy(flux3)
setnames(flux.12, names(flux1))
setnames(flux.123, names(flux1))
flux.12[, days:= days + 15]
flux.123[, days:= days + 30]

toadd.columns <- ! names(flux1) %in% c("days", "treatment", "fertilizer", "precipitation", "tillage")
toadd.columns <- names(flux1)[toadd.columns]

################################################################################
# create 4-replicate mean and sd columns

# flux1 (1st event)
setkey(flux1, fertilizer, tillage, precipitation)
flux1[,id:=1:nrow(flux1)]
setkey(flux1, id)

flux1[,NO:= mean(c(aNO, bNO, cNO, dNO)), by= id]
flux1[,NOsd:= sd(c(aNO, bNO, cNO, dNO)), by= id]

flux1[,N2O:= mean(c(aN2O, bN2O, cN2O, dN2O)), by= id]
flux1[,N2Osd:= sd(c(aN2O, bN2O, cN2O, dN2O)), by= id]

flux1[,CO2:= mean(c(aCO2, bCO2, cCO2, dCO2)), by= id]
flux1[,CO2sd:= sd(c(aCO2, bCO2, cCO2, dCO2)), by= id]

flux1[,CH4:= mean(c(aCH4, bCH4, cCH4, dCH4)), by= id]
flux1[,CH4sd:= sd(c(aCH4, bCH4, cCH4, dCH4)), by= id]

# flux2 (2nd event))
setkey(flux2, fertilizer, tillage, precipitation)
flux2[,id:=1:nrow(flux2)]
setkey(flux2, id)

flux2[,NO:= mean(c(aNO, bNO, cNO, dNO)), by= id]
flux2[,NOsd:= sd(c(aNO, bNO, cNO, dNO)), by= id]

flux2[,N2O:= mean(c(aN2O, bN2O, cN2O, dN2O)), by= id]
flux2[,N2Osd:= sd(c(aN2O, bN2O, cN2O, dN2O)), by= id]

flux2[,CO2:= mean(c(aCO2, bCO2, cCO2, dCO2)), by= id]
flux2[,CO2sd:= sd(c(aCO2, bCO2, cCO2, dCO2)), by= id]

flux2[,CH4:= mean(c(aCH4, bCH4, cCH4, dCH4)), by= id]
flux2[,CH4sd:= sd(c(aCH4, bCH4, cCH4, dCH4)), by= id]

# flux3 (3rd event))
setkey(flux3, fertilizer, tillage, precipitation)
flux3[,id:=1:nrow(flux3)]
setkey(flux3, id)

flux3[,NO:= mean(c(aNO, bNO, cNO, dNO)), by= id]
flux3[,NOsd:= sd(c(aNO, bNO, cNO, dNO)), by= id]

flux3[,N2O:= mean(c(aN2O, bN2O, cN2O, dN2O)), by= id]
flux3[,N2Osd:= sd(c(aN2O, bN2O, cN2O, dN2O)), by= id]

flux3[,CO2:= mean(c(aCO2, bCO2, cCO2, dCO2)), by= id]
flux3[,CO2sd:= sd(c(aCO2, bCO2, cCO2, dCO2)), by= id]

flux3[,CH4:= mean(c(aCH4, bCH4, cCH4, dCH4)), by= id]
flux3[,CH4sd:= sd(c(aCH4, bCH4, cCH4, dCH4)), by= id]
################################################################################
fluxevery1 <- copy(flux1)
fluxevery2 <- copy(flux2)
fluxevery3 <- copy(flux3)
setnames(fluxevery2, names(fluxevery1))
setnames(fluxevery3, names(fluxevery1))
fluxevery2[, days:= days + 15]
fluxevery3[, days:= days + 30]

cum.flux.every <- rbind(fluxevery1, fluxevery2, fluxevery3)
setkey(cum.flux.every, fertilizer, tillage, precipitation, days)
################################################################################
################################################################################
        # append cumulative fluxes for 45 days
        event1cum14 <- flux1[days==14, c("treatment", toadd.columns), with=F] #cumulative fluxes at tyhe end of 1st event
        event2cum14 <- flux2[days2ndEvent==14, c("treatment", toadd.columns), with=F]
        
        event12cum.toadd <- event1cum14[,toadd.columns, with=F] + event2cum14[,toadd.columns, with=F]
        event12cum14 <- event2cum14
        event12cum14[,toadd.columns:=event12cum.toadd, with=F] #cumulative fluxes at tyhe end of 2nd event (since day 0)
        
        ### prepare for join
        setnames(event1cum14, c("treatment", paste0("1.", toadd.columns)))
        setnames(event12cum14, c("treatment", paste0("2.", toadd.columns)))
        
        toadd.columns.1 <- names(event1cum14)[-1]
        toadd.columns.12 <- names(event12cum14)[-1]
        
        setkey(flux.1, treatment)
        setkey(flux.12, treatment)
        setkey(flux.123, treatment)
        setkey(event1cum14, treatment)
        setkey(event12cum14, treatment)
        
        # addition of 1st event cum fluxes to 2nd event data
        flux.12.toadd <- flux.12[event1cum14] #each row of flux.12 have an extra column with cum flux from 1st event
        flux.12.toadd <- flux.12.toadd[,toadd.columns, with = FALSE] + flux.12.toadd[,toadd.columns.1, with = FALSE]
        flux.12[,toadd.columns:= flux.12.toadd, with=FALSE]
        
        # addition of 1st&2nd events cum fluxes to 3rd event data
        flux.123.toadd <- flux.123[event12cum14] #each row of flux.12 have an extra column with cum flux from 1st event
        flux.123.toadd <- flux.123.toadd[,toadd.columns, with = FALSE] + flux.123.toadd[,toadd.columns.12, with = FALSE]
        flux.123[,toadd.columns:= flux.123.toadd, with=FALSE]
        
        # bind 3 evnets cumulative data toguether
        flux123 <- rbind(flux.1, flux.12, flux.123)
################################################################################
# create 4-replicate mean and sd columns (days 0-44)
# flux123

setkey(flux123, fertilizer, tillage, precipitation)
flux123[,id:=1:nrow(flux123)]
setkey(flux123, id)

flux123[,NO:= mean(c(aNO, bNO, cNO, dNO)), by= id]
flux123[,NOsd:= sd(c(aNO, bNO, cNO, dNO)), by= id]

flux123[,N2O:= mean(c(aN2O, bN2O, cN2O, dN2O)), by= id]
flux123[,N2Osd:= sd(c(aN2O, bN2O, cN2O, dN2O)), by= id]

flux123[,CO2:= mean(c(aCO2, bCO2, cCO2, dCO2)), by= id]
flux123[,CO2sd:= sd(c(aCO2, bCO2, cCO2, dCO2)), by= id]

flux123[,CH4:= mean(c(aCH4, bCH4, cCH4, dCH4)), by= id]
flux123[,CH4sd:= sd(c(aCH4, bCH4, cCH4, dCH4)), by= id]


################################################################################
# create  to-write 14-days-cumulative-flux objects
cum14.1 <- flux1[days==14,]
cum14.2 <- flux2[days2ndEvent==14,]
cum14.3 <- flux3[days3rdEvent==14,]
cum29 <- flux123[days==29,]
cum44 <- flux123[days==44,]

flux1[,id:=NULL]
flux2[,id:=NULL]
flux3[,id:=NULL]
flux123[,id:=NULL]
cum.flux.every[,id:=NULL]

cum14.1[,id:=NULL]
cum14.2[,id:=NULL]
cum14.3[,id:=NULL]
cum29[,id:=NULL]
cum44[,id:=NULL]

# write output files
################################################################################
no.format <- c("days", "treatment", "fertilizer", "precipitation", "tillage")
no.format <- names(cum14.1) %in% no.format
to.format <- names(cum14.1)[!no.format]

flux1[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]
flux2[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]
flux3[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]
flux123[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]
cum.flux.every[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]

cum14.1[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]
cum14.2[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]  
cum14.3[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]  
cum29[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]  
cum44[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]

data.to.write <- c("flux1", "flux2", "flux3", "flux123", "cum.flux.every",
        "cum14.1", "cum14.2", "cum14.3", "cum29", "cum44")
############################
#
#write into output folder
for (i in 1:length(to.write)){
        write.table(get(data.to.write[i]), file= to.write[i], row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
        print(  paste0(Sys.time(), " cumulative fluxes file was written to:  ")  )
        print(to.write[i])
        print(" ")
}