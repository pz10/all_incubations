# output files
folder.out <- paste0(output_path, "/4replicates_together")
dir.create(folder.out)
cum.flux.1st <- paste0(folder.out, "/1st_event_cum_fluxes.dat")
cum.flux.2nd <- paste0(folder.out, "/2nd_event_cum_fluxes.dat")
cum.flux.3rd <- paste0(folder.out, "/3rd_event_cum_fluxes.dat")
dir.create(folder.out)

# flux files
A.flux.file <- paste0(input_path, "/A_IDASw/A_fluxes/A_fluxes.dat")
B.flux.file <- paste0(input_path, "/B_IDASw/B_fluxes/B_fluxes.dat")
C.flux.file <- paste0(input_path, "/C_IDASw/C_fluxes/C_fluxes.dat")
D.flux.file <- paste0(input_path, "/D_IDASw/D_fluxes/D_fluxes.dat")
treatments <- paste0(input_path_RAW, "/_extra/treatments.dat")

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

# initial parameters
step <- 0.125 # time points where to calculate cumulative values
lastday <- 14 # last time point to be calculated
minNO <- 0 #minimum flux to be taken into account
minN2O <- 0 #minimum flux to be taken into account
minCO2 <- 0 #minimum flux to be taken into account
minCH4 <- 0 #minimum flux to be taken into account

source("1st_event.R")
source("2nd_event.R")
source("3rd_event.R")

offsetNO <- 0
offsetN2O <- 25
offsetCO2 <- 15
offsetCH4 <- 100 # we want everything in
# 
# source("1st_event_negative_IN.R")
# source("2nd_event_negative_IN.R")
# source("3rd_event_negative_IN.R")

