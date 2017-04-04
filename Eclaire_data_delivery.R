folder_out <- paste0(output_path, "/eclaire_data_delivery")
fileout<- paste0(folder_out, "/ECLAIRE.D2.2.dat")
dir.create(folder_out)

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

Aflux[,replicate:=4]
Bflux[,replicate:=3]
Cflux[,replicate:=2]
Dflux[,replicate:=1]

data <- rbind(Aflux, Bflux, Cflux, Dflux)



treatments <- c("replicate", "treatment", "fertilizer", "precipitation", "tillage")
times <- c("days", "days2ndEvent", "days3rdEvent")
fluxes <- c("NO", "N2O", "CO2")
water <- c( "weight", "addedwater",
            "WCv", "WCm",
            "WFPS")

to.delete <- !names(data) %in% c(treatments, times,  fluxes, water)
to.delete <- names(data)[to.delete]
set(data, j = to.delete, value=NULL)

new.order <- c(treatments, times,  fluxes, water)
setcolorder(data, new.order)

no.format <- c(treatments)
no.format <- names(data) %in% no.format
to.format <- names(data)[!no.format]
to.format6 <- to.format [grep("days",to.format)]
to.format4 <- to.format [-grep("days",to.format)]

data[, (to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4, flag = 0)), .SDcols = to.format4]
data[, (to.format6):= lapply(.SD, function(x) formatC(x, format = "f", digits = 6, flag = 0)), .SDcols = to.format6]


############################
#
#write into output folder
write.table(data, file= fileout, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

print(  paste0( Sys.time(), "  fluxes file was written to:  ")  )
print(fileout)

