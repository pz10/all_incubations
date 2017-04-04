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
Aflux[,incubation:= "A"]
Bflux[,incubation:= "B"]
Cflux[,incubation:= "C"]
Dflux[,incubation:= "D"]

fluxes <- rbind(Aflux, Bflux, Cflux, Dflux)
setnames(fluxes, "fertilizer", "fertilization")

NOcomp <- 215
setkey(fluxes, fertilization, precipitation, tillage)


with(fluxes, densityplot(NO))

fluxes <- fluxes[!is.na(days.adjusted), ]
fluxes[, is.over.NOcomp:= NO>NOcomp]
fluxes[, NO.over.comp:= 100*sum(is.over.NOcomp, na.rm=T)/sum(!is.na(NO)), by = .(fertilization, precipitation, tillage)]
fluxes[days.adjusted<15, NO.over.comp.1stevent:= 100*sum(is.over.NOcomp, na.rm=T)/sum(!is.na(NO)), by = .(fertilization, precipitation, tillage)]

summary <- fluxes[, .(NO.over.comp = max(NO.over.comp),
                      NO.over.comp.1stevent = max(NO.over.comp.1stevent, na.rm=T)
                      ), by=.(fertilization, precipitation, tillage)]

mean(summary$NO.over.comp)
mean(summary$NO.over.comp.1stevent)


max(fluxes$NO, na.rm=T)
quantile(fluxes[days.adjusted<15,NO], na.rm=T, probs=c(.8,.9,.95))

max(fluxes$NO, na.rm=T) / NOcomp
