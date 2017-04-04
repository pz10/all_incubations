
################################################################################
# all
mymodel <- with(data.inc, lm(formula=log(N2OtoNO) ~ WFPS))
# slope <- formatC(mymodel[[1]][[2]], digits=2)
# intercept <- formatC(mymodel[[1]][[1]], digits=2)
# r2 <- formatC(summary(mymodel)$r.squared, digits = 2)
# # my.eq <- paste0("y = ", intercept, " + ", slope)
# my.eq <- paste0("y = ", slope, "x  " ,intercept)
# myr2 <- paste0("R2 = ", r2)
# my.eq <- paste0(my.eq, " \n ", myr2)

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope



aux <- data.table(
        tillage = as.character(NA),
        event = as.character(NA),
        fertilization = as.character(NA),
        precipitation = as.character(NA),
        incubation = as.character(NA),
        
        formula = "log(N2O/NO) ~ WFPS",
        slope = as.numeric(NA),
        intercept = as.numeric(NA),
        r2 = as.numeric(NA),
        WFPS.at.ratio.1 = as.numeric(NA)
        
)
aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "all"]
aux[, precipitation:= "all"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- copy(aux)


################################################################################
# NT
mymodel <- with(data.inc[tillage=="NT"], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "NT"]
aux[, event:= "all"]
aux[, fertilization:= "all"]
aux[, precipitation:= "all"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

# TT
mymodel <- with(data.inc[tillage=="TT"], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "TT"]
aux[, event:= "all"]
aux[, fertilization:= "all"]
aux[, precipitation:= "all"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

################################################################################
# stretch 1
mymodel <- with(data.inc[event==1], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "1"]
aux[, fertilization:= "all"]
aux[, precipitation:= "all"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

# stretch >1
mymodel <- with(data.inc[event!=1], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= ">1"]
aux[, fertilization:= "all"]
aux[, precipitation:= "all"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

################################################################################
# fertilization 0
mymodel <- with(data.inc[fertilizer==0], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "0"]
aux[, precipitation:= "all"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

# fertilization 50
mymodel <- with(data.inc[fertilizer==50], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "50"]
aux[, precipitation:= "all"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

# fertilization 100
mymodel <- with(data.inc[fertilizer==100], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "100"]
aux[, precipitation:= "all"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

################################################################################
# rain == hom
mymodel <- with(data.inc[precipitation=="c"], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "all"]
aux[, precipitation:= "hom"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

# rain == inc
mymodel <- with(data.inc[precipitation=="i"], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "all"]
aux[, precipitation:= "inc"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

# rain == dec
mymodel <- with(data.inc[precipitation=="d"], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "all"]
aux[, precipitation:= "dec"]
aux[, incubation:= "all"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

################################################################################
# incubation A
mymodel <- with(data.inc[incubation=="A"], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "all"]
aux[, precipitation:= "all"]
aux[, incubation:= "A"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

# incubation B
mymodel <- with(data.inc[incubation=="B"], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "all"]
aux[, precipitation:= "all"]
aux[, incubation:= "B"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

# incubation C
mymodel <- with(data.inc[incubation=="C"], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "all"]
aux[, precipitation:= "all"]
aux[, incubation:= "C"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

# incubation D
mymodel <- with(data.inc[incubation=="D"], lm(formula=log(N2OtoNO) ~ WFPS))

myslope <- mymodel[[1]][[2]]
myintercept <- mymodel[[1]][[1]]
myr2 <- summary(mymodel)$r.squared
myWFPS <- -myintercept/myslope

aux[, tillage:= "all"]
aux[, event:= "all"]
aux[, fertilization:= "all"]
aux[, precipitation:= "all"]
aux[, incubation:= "D"]

aux[, slope:= myslope]
aux[, intercept:= myintercept]
aux[, r2:= myr2]
aux[, WFPS.at.ratio.1:= myWFPS]

mymodels <- rbindlist(list(mymodels, aux))

################################################################################
# write summary files
mydata <- copy (mymodels)
no.format <- c("event", "precipitation", "tillage", "incubation", "fertilization", "formula")
to.format4 <- names(mydata)[!names(mydata) %in% no.format]
mydata[,(to.format4):= lapply(.SD, function(x) formatC(x, format = "f", digits = 4)), .SDcols = to.format4]
myfile <- paste0(folder.out, "/_NO_to_N2O_vs_WFPS_regression_summaries.dat")
write.table(mydata, file= myfile, row.names = FALSE, sep = "\t", quote = FALSE)


################################################################################
data.inc[,myevent:= as.character(NA)]
data.inc[event==1, myevent:= "1"]
data.inc[event!=1, myevent:= ">1"]

data.inc[, tillage:= factor(tillage)]
data.inc[, myevent:= factor(myevent)]
data.inc[, fertilizer:= factor(fertilizer)]
data.inc[, precipitation:= factor(precipitation)]


fit <- lm(log(N2OtoNO) ~ WFPS + tillage,data=data.inc)
summary(fit)
confint(fit)


fit <- lm(log(N2OtoNO) ~ WFPS + tillage + event + fertilizer + precipitation + event,data=data.inc)

fit <- lm(log(N2OtoNO) ~ WFPS * tillage,data=data.inc)
fit <- lm(log(N2OtoNO) ~ tillage/WFPS- 1,data=data.inc)
fit <- lm(log(N2OtoNO) ~ factor(fertilizer)/WFPS,data=data.inc)


fit <- aov(log(N2OtoNO) ~ WFPS * tillage,data=data.inc)
fit <- aov(log(N2OtoNO) ~ tillage/WFPS- 1,data=data.inc)

fit <- aov(log(N2OtoNO) ~ WFPS * myevent,data=data.inc)

fit <- aov(log(N2OtoNO) ~ fertilizer*WFPS,data=data.inc)
fit <- aov(log(N2OtoNO) ~ incubation*WFPS,data=data.inc)

fit <- aov(log(N2OtoNO) ~ precipitation*WFPS,data=data.inc)

fit <- aov(log(N2OtoNO) ~ (myevent+tillage+precipitation+fertilizer+incubation)*WFPS,data=data.inc)
fit <- aov(log(N2OtoNO) ~ (myevent+tillage+precipitation+fertilizer)*WFPS,data=data.inc)

fit <- lm(log(N2OtoNO) ~ (myevent+tillage+precipitation+fertilizer)*WFPS,data=data.inc)
fit <- lm(log(N2OtoNO) ~ (myevent+tillage+precipitation+fertilizer)*WFPS -1,data=data.inc)
fit <- aov(log(N2OtoNO) ~ (myevent+tillage+precipitation+fertilizer)*WFPS -1,data=data.inc)

#############################
require(multcomp)
require(car)
require(HH)
require(gvlma)
require(bootstrap)
require(MASS)
require(leaps)
require(coin)
require(lmPerm)

# define output directory
my.output.dir <- "G:/BioAtmo/zuazo-p/General/eclaire/Automatic system/OUTPUTS/drying_wetting/_all_fluxes/_ANOVA/_ANOVA_N2OtoNObyWFPS"
dir.create(my.output.dir)


# data[,id:= paste0(incubation, fertilization, precipitation, tillage)]

# export to html
my.files <- list.files("_ANOVA_N2OtoNObyWFPS", pattern = ".Rmd", full.names = TRUE)
my.files
render(input = my.files[1], output_format = "html_document", output_dir = my.output.dir)
