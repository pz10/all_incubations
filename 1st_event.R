#inicialize output data.table
nrows <- nrow(treatments)*(lastday*8+1)
heading <- c("days", "treatment", "fertilizer", "precipitation", "tillage",
             paste0(letters[1:4], "NO"),
             paste0(letters[1:4], "N2O"),
             paste0(letters[1:4], "CO2"),
             paste0(letters[1:4], "CH4")
             )
ncolumns <- length(heading)
dim_names <- list(NULL, heading)

myfluxes <- matrix(data = as.numeric(NA), nrow = nrows, ncol = ncolumns, byrow = TRUE, dimnames = dim_names)
myfluxes <- data.table(myfluxes)

ascharacter <- c("treatment", "precipitation", "tillage") # fertilizer is leave aside bcause 0,50 and 100 are read to numeric by fread
myfluxes[,ascharacter:= lapply(.SD, as.character),.SDcols = ascharacter, with=F]

# set time points where cumulative flux is calculated
mytimes <- seq(from = 0, to = lastday, by =step)
mylength <- length(mytimes)

# write common frame to output object: days and treatments
myfluxes[,days:= rep(mytimes, nrow(treatments))]

myfluxes[,treatment:= rep(treatments$treatment, times = 1, each = mylength)]
myfluxes[,fertilizer:= rep(treatments$fertilizer, times = 1, each = mylength)]
myfluxes[,precipitation:= rep(treatments$precipitation, times = 1, each = mylength)]
myfluxes[,tillage:= rep(treatments$tillage, times = 1, each = mylength)]


################################################################################
# NO
################################################################################
### calculate cumulative fluxes at 3 h intervals
mytreatments <- treatments$treatment

for(i in 1:4){
        myfile <- paste0(LETTERS[i], "flux")
        
        count <- 1
        for(j in mytreatments){
                # select target data
                mydata <- get(myfile)[!is.na(NO)
                               & days >= 0
                               & days <= (lastday+1)
                               & treatment==j
                               , list(days, treatment, NO)]
                # insert origin (days == 0), with value as last measurement before origin
                myorigin <- get(myfile)[!is.na(NO)
                                 & days <= 0
                                 & treatment==j
                                 , list(days, treatment, NO)]
                
                myorigin <- tail(myorigin, 1)
                mydata <- rbind(myorigin, mydata)
                mydata[1, days:= 0]
                # calculate cumulative fluxes ('cum') for target data
                mydata[,hours:= days*24]
                mydata[NO < minNO, NO:= 0]
                
                for(k in 2:nrow(mydata)){
                        mydt <- mydata[1:k,]
                        mydata[k,cum:= auc(mydt$hours,mydt$NO, thresh=0)]
                }
                mydata[1,cum:= 0]
                # interpolation of cumulative fluxes at 3h intervals
                mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
                # insert output into 'myfluxes'
                myfluxes[count:(count+mylength-1), paste0(letters[i],"NO"):= mycum$y]
                                
                # increase count
                count <- count + mylength
        }
        print(paste0(Sys.time(), "  ", LETTERS[i], "_incubation NO-cumulative values have been calculated (1st rain event)"))

}
################################################################################
# N2O
################################################################################
### calculate cumulative fluxes at 3 h intervals
mytreatments <- treatments$treatment

for(i in 1:4){
        myfile <- paste0(LETTERS[i], "flux")
        
        count <- 1
        for(j in mytreatments){
                # select target data
                mydata <- get(myfile)[!is.na(N2O)
                                      & days >= 0
                                      & days <= (lastday+1)
                                      & treatment==j
                                      , list(days, treatment, N2O)]
                # insert origin (days == 0), with value as last measurement before origin
                myorigin <- get(myfile)[!is.na(N2O)
                                        & days <= 0
                                        & treatment==j
                                        , list(days, treatment, N2O)]
                
                myorigin <- tail(myorigin, 1)
                mydata <- rbind(myorigin, mydata)
                mydata[1, days:= 0]
                # calculate cumulative fluxes ('cum') for target data
                mydata[,hours:= days*24]
                mydata[N2O < minN2O, N2O:= 0]
                
                for(k in 2:nrow(mydata)){
                        mydt <- mydata[1:k,]
                        mydata[k,cum:= auc(mydt$hours,mydt$N2O, thresh=0)]
                }
                mydata[1,cum:= 0]
                # interpolation of cumulative fluxes at 3h intervals
                mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
                # insert output into 'myfluxes'
                myfluxes[count:(count+mylength-1), paste0(letters[i],"N2O"):= mycum$y]
                
                # increase count
                count <- count + mylength
        }
        print(paste0(Sys.time(), "  ", LETTERS[i], "_incubation N2O-cumulative values have been calculated (1st rain event)"))
        
}
################################################################################
# CO2
################################################################################
### calculate cumulative fluxes at 3 h intervals
mytreatments <- treatments$treatment

for(i in 1:4){
        myfile <- paste0(LETTERS[i], "flux")
        
        count <- 1
        for(j in mytreatments){
                # select target data
                mydata <- get(myfile)[!is.na(CO2)
                                      & days >= 0
                                      & days <= (lastday+1)
                                      & treatment==j
                                      , list(days, treatment, CO2)]
                # insert origin (days == 0), with value as last measurement before origin
                myorigin <- get(myfile)[!is.na(CO2)
                                        & days <= 0
                                        & treatment==j
                                        , list(days, treatment, CO2)]
                
                myorigin <- tail(myorigin, 1)
                mydata <- rbind(myorigin, mydata)
                mydata[1, days:= 0]
                # calculate cumulative fluxes ('cum') for target data
                mydata[,hours:= days*24]
                mydata[CO2 < minCO2, CO2:= 0]
                
                for(k in 2:nrow(mydata)){
                        mydt <- mydata[1:k,]
                        mydata[k,cum:= auc(mydt$hours,mydt$CO2, thresh=0)]
                }
                mydata[1,cum:= 0]
                # interpolation of cumulative fluxes at 3h intervals
                mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
                # insert output into 'myfluxes'
                myfluxes[count:(count+mylength-1), paste0(letters[i],"CO2"):= mycum$y]
                
                # increase count
                count <- count + mylength
        }
        print(paste0(Sys.time(), "  ", LETTERS[i], "_incubation CO2-cumulative values have been calculated (1st rain event)"))
        
}
################################################################################
# CH4
################################################################################
### calculate cumulative fluxes at 3 h intervals
mytreatments <- treatments$treatment

for(i in 1:4){
        myfile <- paste0(LETTERS[i], "flux")
        
        count <- 1
        for(j in mytreatments){
                # select target data
                mydata <- get(myfile)[!is.na(CH4)
                                      & days >= 0
                                      & days <= (lastday+1)
                                      & treatment==j
                                      , list(days, treatment, CH4)]
                # insert origin (days == 0), with value as last measurement before origin
                myorigin <- get(myfile)[!is.na(CH4)
                                        & days <= 0
                                        & treatment==j
                                        , list(days, treatment, CH4)]
                
                myorigin <- tail(myorigin, 1)
                mydata <- rbind(myorigin, mydata)
                mydata[1, days:= 0]
                                # This is done to take into account CH4 captation
                                mydata[,CH4:= CH4+100] # later is subtracted again
                
                # calculate cumulative fluxes ('cum') for target data
                mydata[,hours:= days*24]
                mydata[CH4 < minCH4, CH4:= 0]
                
                for(k in 2:nrow(mydata)){
                        mydt <- mydata[1:k,]
                        mydata[k,cum:= auc(mydt$hours,mydt$CH4, thresh=0)]
                }
                mydata[1,cum:= 0]
                # interpolation of cumulative fluxes at 3h intervals
                mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
                                # added +100 is substracted now (this way, CH4 captation will appear as negative)
                                mycum$y <- mycum$y - mycum$x * 24* 100 # mycum$x * 24 --> transformed to hours
                
                # insert output into 'myfluxes'
                myfluxes[count:(count+mylength-1), paste0(letters[i],"CH4"):= mycum$y]
                
                # increase count
                count <- count + mylength
        }
        print(paste0(Sys.time(), "  ", LETTERS[i], "_incubation CH4-cumulative values have been calculated (1st rain event)"))
        
}
################################################################################################################################################################
################################################################################
################################################################################################################################################################
################################################################################
# write output file
################################################################################
# unit change
# [µg-N/m2] to [mg-N/m2]
myfluxes[,aNO:= aNO/1000]
myfluxes[,bNO:= bNO/1000]
myfluxes[,cNO:= cNO/1000]
myfluxes[,dNO:= dNO/1000]

myfluxes[,aN2O:= aN2O/1000]
myfluxes[,bN2O:= bN2O/1000]
myfluxes[,cN2O:= cN2O/1000]
myfluxes[,dN2O:= dN2O/1000]

# [mg-C/m2] to [g-C/m2]
myfluxes[,aCO2:= aCO2/1000]
myfluxes[,bCO2:= bCO2/1000]
myfluxes[,cCO2:= cCO2/1000]
myfluxes[,dCO2:= dCO2/1000]

# [µg-C/m2] to [mg-C/m2]
myfluxes[,aCH4:= aCH4/1000]
myfluxes[,bCH4:= bCH4/1000]
myfluxes[,cCH4:= cCH4/1000]
myfluxes[,dCH4:= dCH4/1000]

# format
no.format <- c("days", "treatment", "fertilizer", "precipitation", "tillage")
no.format <- names(myfluxes) %in% no.format
to.format <- names(myfluxes)[!no.format]

myfluxes[, (to.format):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format]        
############################
#
#write into output folder
write.table(myfluxes, file= cum.flux.1st, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

print(  paste0(Sys.time(), " 1st rain event cumulative fluxes file was written to:  ")  )
print(cum.flux.1st)
