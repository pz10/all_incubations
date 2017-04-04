
.libPaths(c(.libPaths(), "C:/Users/zuazo-p/Documents/R/win-library/3.0"))
# cleanning of workspace and time recording
rm(list = ls())
print(Sys.time())
Sys.setenv(TZ='UTC') #--> very important

#1)   set common target input folder and target output folder (Use /, not \).
input_path <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting"
input_path_RAW <-  "G:/BioAtmo/zuazo-p/RAW data/_drying_wetting"
# input_path_RAW <- "E:/RAW data/_drying_wetting"
        
output_path <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/_all_fluxes"
dir.create(output_path)


#2)   set desire date and time format (date_format) and date origin (date_origin) (IDASW and PICARRO)
#     see (http://www.stat.berkeley.edu/classes/s133/dates.html) for date formats
date_format<-"%Y-%m-%d %H:%M:%S"
date_origin<-"1970-01-01"
#     UTC (Coordinated Universal Time) offset of the winter-time of our time zone
#     data must be without DST(day saving time ) transition (e.g. always winter-time)
UTC_offset <- 3600
area <- pi*0.127*0.127/4


require(data.table)
require(flux)
require(lattice)
require(ggplot2)
require(multcomp)
require(car)
require(gridExtra)


require(reshape2)
require(zoo)
require(grid)
require(rmarkdown)

library(MASS)
library(scales)

# # source("final_plots2ggplot.R")
# source("cumulative_flux.R")
# source("means.R")

# source("cumulative_plots.R")
# source("cumulative_plots_every_incubation.R")
# source("cumulative_plots_every_incubation_separate.R")
################################################################################

# cumulative_flux.R
        # 1st_event.R
        # 2nd_event.R
        # 3rd_event.R

# means.R

# WFPS_water_content_plots.R
# WFPS_vs_N_plots.R
# WFPS_vs_mean_N_plots.R
# boxplots_NO.R
# boxplots_NO_byvolume.R
# 
# boxplots_N2O.R
# boxplots_N2O_byvolume.R



# _boxplot_cum_fluxes.R
#         cum_flux_summaries.R
#                 (_ANOVA_NO_N2O_NH3.R)
#                 cum_flux_summaries_GWP.R
#                 cum_flux_summaries_byEvent.R
#         source("_boxplot_ABCD.R")
#         source("_boxplot_ABC.R")


# acid_trap.R
#       acid_trap_calculation.R

# CO2vsNfluxes.R

# cum_plots_MEANplusSE.R

# WFPSvsN_event.R
        # WFPSvsN_event_summaries.R
# WFPSvsN_event_cont.R

# peak_delay.R