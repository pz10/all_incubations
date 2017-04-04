require(multcomp)
require(car)
require(HH)
require(gvlma)
require(bootstrap)
require(MASS)
require(leaps)
require(coin)
require(lmPerm)
require(rmarkdown)
library(npar)
source("W.MW.oneway.R")

# define output directory
my.output.dir <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/_all_fluxes/_ANOVA"
dir.create(my.output.dir)

# load data
folder.GWP <-paste0(output_path, "/cum_fluxes_14/_cum_fluxes_42_GWP")
myfile <- paste0(folder.GWP, "/GWPcumFlux_bycore.dat")
data <- fread(myfile)

RAWdata <- data[,.(
        incubation = factor(incubation),
        fertilization = factor(fertilization),
        precipitation = factor(precipitation),
        tillage = factor(tillage),
        NO = NO,
        N2O = N2O,
        NH3 = NH3,
        NO.N2O = NO.N2O,
        NO.N2O.NH3 = NO.N2O.NH3,
        CO2 = CO2,
        
        GWP.CO2 = GWP.CO2,
        GWP.CH4 = GWP.CH4,
        GWP.N2O = GWP.N2O,
        GWP.all = GWP.all,
        GWP.CO2eq = GWP.CO2eq.mgCo2m2h
        
)]
RAWdata[,id:= paste0(incubation, fertilization, precipitation, tillage)]

data <- copy(RAWdata)

# export to html
my.files <- list.files("_ANOVA", pattern = ".Rmd", full.names = TRUE)
my.files
render(input = my.files[2], output_format = "html_document", output_dir = my.output.dir)

