get.cumulative.flux  <- function(target = NULL, step = 0.125, lastday = 14, event = 1 ){
        if(length(target) == 0) stop( "PLEASE SET target as in *_fluxes.dat col names -->  i.e.(CO2, H2O, N2O, NO, NH3, CH4)", call. = FALSE)
        # flux files
        A.flux.file <- paste0(input_path, "/A_IDASw/A_fluxes/A_fluxes.dat")
        B.flux.file <- paste0(input_path, "/B_IDASw/B_fluxes/B_fluxes.dat")
        C.flux.file <- paste0(input_path, "/C_IDASw/C_fluxes/C_fluxes.dat")
        D.flux.file <- paste0(input_path, "/D_IDASw/D_fluxes/D_fluxes.dat")
        treatments <- paste0(input_path, "/_all_fluxes/treatments.dat")
        
        # read data files
        Aflux <- fread(input = A.flux.file)
        Bflux <- fread(input = B.flux.file) 
        Cflux <- fread(input = C.flux.file) 
        Dflux <- fread(input = D.flux.file)
        treatments <- fread(input = treatments)
        
        setkey(Aflux, time)
        setkey(Bflux, time)
        setkey(Cflux, time)
        setkey(Dflux, time)
        setkey(Aflux, treatment, time)
        setkey(Bflux, treatment, time)
        setkey(Cflux, treatment, time)
        setkey(Dflux, treatment, time)
        
        
        #inicialize output data.table
        nrows <- nrow(treatments)*(lastday*8+1)
        heading <- c("days", "treatment", "fertilizer", "precipitation", "tillage")
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
        myfluxes[,days:= rep(mytimes, length(mytreatments))]
        
        myfluxes[,treatment:= rep(treatments$treatment, times = 1, each = mylength)]
        myfluxes[,fertilizer:= rep(treatments$fertilizer, times = 1, each = mylength)]
        myfluxes[,precipitation:= rep(treatments$precipitation, times = 1, each = mylength)]
        myfluxes[,tillage:= rep(treatments$tillage, times = 1, each = mylength)]
        
        
        ################################################################################
        ### calculate cumulative fluxes at 3 h intervals
        mytreatments <- treatments$treatment
        
        for(i in 1:4){
                myfile <- paste0(LETTERS[i], "flux")
                
                count <- 1
                for(j in mytreatments){
                        # select data
                        mydata <- get(myfile)[!is.na(get(target))
                                              & days >= 0
                                              & days <= (lastday+1)
                                              & treatment==j
                                              , list(days, treatment, get(target))]
                        # insert origin (days == 0), with value as last measurement before origin
                        myorigin <- get(myfile)[!is.na(get(target))
                                                & days <= 0
                                                & treatment==j
                                                , list(days, treatment, target = get(target))]
                        
                        myorigin <- tail(myorigin, 1)
                        mydata <- rbind(myorigin, mydata)
                        mydata[1, days:= 0]
                        # calculate cumulative fluxes ('cum') for subset data
                        mydata[,hours:= days*24]
                        mydata[target < 1, target:= 0]
                        
                        for(k in 2:nrow(mydata)){
                                mydt <- mydata[1:k,]
                                mydata[k,cum:= auc(mydt$hours,mydt[,get(target)], thresh=0)]
                        }
                        mydata[1,cum:= 0]
                        # interpolation of cumulative fluxes at 3h intervals
                        mycum <- approx(x = mydata$days, y = mydata$cum, xout = mytimes, method = "linear", rule = 2:2)
                        # insert output into 'myfluxes'
                        myfluxes[count:(count+mylength-1), paste0(letters[i],"target"):= mycum$y]
                        
                        # increase count
                        count <- count + mylength
                }
                print(paste0(Sys.time(),"  ", LETTERS[i], "_incubation"))
                
        }
        myfluxes
}