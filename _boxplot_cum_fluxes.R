# folder_out <- paste0(output_path, "/all_4incubations_plots")
folder_out <-paste0(output_path, "/monography_plots/instant_fluxes")
folder_out_kgNha <-paste0(output_path, "/monography_plots/instant_fluxes_kgNha")
dir.create(folder_out, recursive = T)
dir.create(folder_out_kgNha, recursive = T)

folder.in <-paste0(output_path, "/cum_fluxes_14")
cum.flux.1st.14 <- paste0(folder.in, "/1st_event_cum14_fluxes.dat")
cum.flux.2nd.14 <- paste0(folder.in, "/2nd_event_cum14_fluxes.dat")
cum.flux.3rd.14 <- paste0(folder.in, "/3rd_event_cum14_fluxes.dat")
cum.flux.1st.29 <- paste0(folder.in, "/1st_event_cum29_fluxes.dat")
cum.flux.1st.44 <- paste0(folder.in, "/1st_event_cum44_fluxes.dat")

folder.GWP <-paste0(output_path, "/cum_fluxes_14/_cum_fluxes_42_GWP")
dir.create(folder.GWP, recursive = T)

folder.fert.effect <- paste0(folder.in, "/fertilization_effect_summaries")
dir.create(folder.fert.effect, recursive = T)

folder.fert.FIE <- paste0(folder.in, "/fert_FIE_summaries")
dir.create(folder.fert.folder.FIE, recursive = T)

folder.fert.Rr <- paste0(folder.in, "/fert_Rr_summaries")
dir.create(folder.fert.Rr, recursive = T)

data <- fread(input = cum.flux.1st.44)

# NH3 data
folder.in.NH3 <-paste0(output_path, "/AMMONIAtrial")
NH3 <- paste0(folder.in.NH3, "/NH3.dat")

NH3 <- fread(NH3)
setnames(NH3, "fertilizer", "fertilization")
NH3 <- NH3[,.(incubation, treatment, fertilization, precipitation, tillage, NH3)]


# NH3 by event
folder.in.NH3 <-paste0(output_path, "/acid_trap")
NH3.event <- paste0(folder.in.NH3, "/NH3_mgNm2_by_core_event.dat")

NH3.event <- fread(NH3.event)
NH3.event <- NH3.event[trap=="a"]
NH3.event[,trap:=NULL]
setnames(NH3.event, "NH3.mgNm2", "NH3")
NH3.event <- NH3.event[,.(incubation, fertilization, precipitation, tillage, event, NH3)]

################################################################################
### A incubation
cum.A <- copy(data)
cum.A[, NO:= aNO]
cum.A[, N2O:= aN2O]
cum.A[, CO2:= aCO2]
cum.A[, CH4:= aCH4]
# cum.A[, todelete:=NULL, with=FALSE]
cum.A[, incubation:= "A"]

### B incubation
cum.B <- copy(data)
cum.B[, NO:= bNO]
cum.B[, N2O:= bN2O]
cum.B[, CO2:= bCO2]
cum.B[, CH4:= bCH4]
# cum.B[, todelete:=NULL, with=FALSE]
cum.B[, incubation:= "B"]
# cum.B[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### C incubation
cum.C <- copy(data)
cum.C[, NO:= cNO]
cum.C[, N2O:= cN2O]
cum.C[, CO2:= cCO2]
cum.C[, CH4:= cCH4]
# cum.C[, todelete:=NULL, with=FALSE]
cum.C[, incubation:= "C"]
# cum.C[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### D incubation
cum.D <- copy(data)
cum.D[, NO:= dNO]
cum.D[, N2O:= dN2O]
cum.D[, CO2:= dCO2]
cum.D[, CH4:= dCH4]
# cum.D[, todelete:=NULL, with=FALSE]
cum.D[, incubation:= "D"]
# cum.D[,days:= days+0.0007] #add 1miniute, so that it is not overplot

### all incubation binding
data <- rbind(cum.A, cum.B, cum.C, cum.D)
data <- data[,.(incubation, treatment, fertilizer, precipitation, tillage,
                NO, N2O, CO2, CH4)]
setnames(data, "fertilizer", "fertilization")
data[incubation=="D", CH4:= NA]

setkey(data, incubation, fertilization, precipitation, tillage, treatment)
setkey(NH3, incubation, fertilization, precipitation, tillage, treatment)
data <- NH3[data]
################################################################################
data[precipitation == "c", precipitation:= "h"]
source("cum_flux_summaries.R")

data[, labelT:= paste(fertilization, precipitation, sep=" ")]
data[, labelP:= paste(fertilization, tillage, sep=" ")]
data[, labelF:= paste(precipitation, tillage, sep=" ")]

data[, tillage:= factor(tillage, levels = c("NT", "TT"), labels = c("no tillage", "traditional tillage"))]
data[, precipitation:= factor(precipitation, levels = c("h", "i", "d"), labels=c("h", "i", "d"))]
data[, fertilization:= factor(fertilization, levels = c(0, 50, 100), labels=c(0, 50, 100))]

data[, labelP:= factor(labelP, levels=c("0 NT", "50 NT",  "100 NT", "0 TT", "50 TT", "100 TT"),
                     labels= c("0 NT", "50 NT",  "100 NT", "0 TT", "50 TT", "100 TT"))]
data[, labelF:= factor(labelF, levels=c("h NT", "h TT", "i NT", "i TT", "d NT",  "d TT"),
                     labels= c("h NT", "h TT", "i NT", "i TT", "d NT",  "d TT"))]
data[, labelT:= factor(labelT, levels=c("0 h", "0 i", "0 d", "50 h", "50 i", "50 d", "100 h", "100 i", "100 d"),
                     labels= c("0 h", "0 i", "0 d", "50 h", "50 i", "50 d", "100 h", "100 i", "100 d"))]
################################################################################

################################################################################
################################################################################
# plots
################################################################################
# plot color parameters
col.rain <- c("white", "deepskyblue","dodgerblue4")
col.fert <- c("white","olivedrab2", "olivedrab4")
col.till <- c("black","red")

source("_boxplot_ABCD.R")
source("_boxplot_ABC.R")
# 