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
folder.in <-paste0(output_path, "/cum_fluxes_14")
myfile <- paste0(folder.in, "/cumFlux_bycore.dat")
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
        CO2 = CO2
        
)]
RAWdata[,id:= paste0(incubation, fertilization, precipitation, tillage)]

data <- copy(RAWdata)

# export to html
my.files <- list.files("_ANOVA", pattern = ".Rmd", full.names = TRUE)
my.files
render(input = my.files[14], output_format = "html_document", output_dir = my.output.dir)



for(i in my.files){
        # do not why it should be repeated within the loop
        my.output.dir <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/_all_fluxes/_ANOVA"
        my.files <- list.files("_ANOVA", pattern = ".Rmd", full.names = TRUE)
        
        # render html docs
        render(input = i, output_format = "html_document", output_dir = my.output.dir)
        
#         not implemented; seems to be for one-way ANOVA
#         # homogeneiety of variances
#         bartlett.test(NO ~ fertilization, data=data) # Bartlett’s test
#         fligner.test(NO ~ fertilization, data=data) #Fligner–Killeen  test
#         require(HH)
#         hov(NO ~ fertilization, data=data) # Brown–Forsythe test
        
#         # not implemented neither
#         t.test(NO ~ tillage, data)
#         # Pairwise comparisons using t tests with pooled SD 
#         pairwise.t.test(data$NO,mydata$treatment,p.adjust.method = "holm")
#         # non-parametric approach
#         ?pairwise.wilcox.test
        
#         # Pairwise comparisons using t tests with pooled SD 
#         pairwise.t.test(data$NO,data$treatment,p.adjust.method = "holm")
#         pairwise.t.test(data$NO,data$labelF,p.adjust.method = "holm")
#         pairwise.t.test(data$NO,data$labelT,p.adjust.method = "holm")
#         pairwise.t.test(data$NO,data$labelP,p.adjust.method = "holm")
#         
#         pairwise.t.test(data$NO,data$tillage,p.adjust.method = "holm")
#         pairwise.t.test(data$NO,data$precipitation,p.adjust.method = "holm")
#         pairwise.t.test(data$NO,data$fertilizer,p.adjust.method = "holm")
#         
#         # non-parametric approach
#         ?pairwise.wilcox.test
}


################################################################################

my.files <- list.files("_ANOVA", pattern = ".Rmd", full.names = TRUE)
my.files
render(input = my.files[1], output_format = "html_document", output_dir = my.output.dir)
# 

# data[,.(
#         mean.NO = mean(NO, na.rm=T),
#         sd.NO = sd(NO, na.rm=T),
#         se.NO = sd(NO, na.rm=T)/sqrt(sum(!is.na(NO))),
#         count.NO = sum(!is.na(NO))
# ), by=incubation]
# 
# data[,.(
#         mean.N2O = mean(N2O, na.rm=T),
#         sd.N2O = sd(N2O, na.rm=T),
#         se.N2O = sd(N2O, na.rm=T)/sqrt(sum(!is.na(N2O))),
#         count.N2O = sum(!is.na(N2O))
# ), by=incubation]
# 
# data[,.(
#         mean.NO.N2O = mean(NO.N2O, na.rm=T),
#         sd.NO.N2O = sd(NO.N2O, na.rm=T),
#         se.NO.N2O = sd(NO.N2O, na.rm=T)/sqrt(sum(!is.na(NO.N2O))),
#         count.NO.N2O = sum(!is.na(NO.N2O))
# ), by=incubation]