W.MW.oneway <- function(formula, data, exact=FALSE, sort=TRUE, n = length(wmc$p),     #modified n to different pvalue correction       
                        method=c("holm", "hochberg", "hommel", "bonferroni",      
                                 "BH", "BY", "fdr", "none")){
        
        if (missing(formula) || class(formula) != "formula" || 
            length(all.vars(formula)) != 2)                                   
                stop("'formula' is missing or incorrect")
        
        method <- match.arg(method)                       
        df <- model.frame(formula, data)          
        y <- df[[1]]
        g <- as.factor(df[[2]])
        vnames <- names(df)
        
        if(sort) g <- reorder(g, y, FUN=median)             
        groups <- levels(g)
        k <- nlevels(g)
        
        getstats <- function(x)(c(N = length(x), Median = median(x),    
                                  MAD = mad(x)))
        sumstats <- t(aggregate(y, by=list(g), FUN=getstats)[2])
        rownames(sumstats) <- c("n", "median", "mad")
        colnames(sumstats) <- groups
        
        kw <- kruskal.test(formula, data)           
        wmc <- NULL
        for (i in 1:(k-1)){
                for (j in (i+1):k){
                        y1 <- y[g==groups[i]]
                        y2 <- y[g==groups[j]] 
                        test <- wilcox.test(y1, y2, exact=exact)
                        r <- data.frame(Group.1=groups[i], Group.2=groups[j], 
                                        W=test$statistic[[1]], p=test$p.value)
                        # note the [[]] to return a single number
                        wmc <- rbind(wmc, r)
                }
        }
        wmc$p <- p.adjust(wmc$p, method=method, n=n)                      
        
        
        data <- data.frame(y, g)               
        names(data) <- vnames
        results <- list(CALL = match.call(), 
                        data=data,
                        sumstats=sumstats, kw=kw, 
                        method=method, wmc=wmc, vnames=vnames)
        class(results) <- c("oneway", "list")
        return(results)             
}

################################################################################
PToneway <- function(formula, data, exact=FALSE, sort=F, n = length(wmc$p),     #modified n to different pvalue correction       
                     method=c("holm", "hochberg", "hommel", "bonferroni",      
                              "BH", "BY", "fdr", "none")){
        
        if (missing(formula) || class(formula) != "formula" || 
            length(all.vars(formula)) != 2)                                   
                stop("'formula' is missing or incorrect")
        
        method <- match.arg(method)                       
        df <- model.frame(formula, data)          
        y <- df[[1]]
        g <- as.factor(df[[2]])
        vnames <- names(df)
        
        #         if(sort) g <- reorder(g, y, FUN=median)             
        groups <- levels(g)
        k <- nlevels(g)
        #         
        #         getstats <- function(x)(c(N = length(x), Median = median(x),    
        #                                   MAD = mad(x)))
        #         sumstats <- t(aggregate(y, by=list(g), FUN=getstats)[2])
        #         rownames(sumstats) <- c("n", "median", "mad")
        #         colnames(sumstats) <- groups
        #         
        #         kw <- kruskal.test(formula, data)           
        wmc <- NULL
        for (i in 1:(k-1)){
                for (j in (i+1):k){
                        mydf <- data.table(df)[get(vnames[2]) %in% c(groups[i],groups[j])]
                        mydf[,vnames[2]:= factor(get(vnames[2]))]
                        #                         y1 <- y[g==groups[i]]
                        #                         y2 <- y[g==groups[j]] 
                        test <- oneway_test(formula, data= mydf)
                        r <- data.frame(Group.1=groups[i], Group.2=groups[j], 
                                        Z=test@ statistic@teststatistic[[1]], p=pvalue(test)[1])
                        # note the [[]] to return a single number
                        wmc <- rbind(wmc, r)
                }
        }
        wmc$p <- p.adjust(wmc$p, method=method, n=n)                      
        
        
        data <- data.frame(y, g)               
        names(data) <- vnames
        results <- list(data=data,
                        wmc=wmc,
                        vnames=vnames,
                        method=method)
        return(results)             
}
###

summary.oneway <- function(object, ...){
        if(!exists("digits")) digits <- 4L
        
        wmc <- object$wmc
        cat("data:", object$vnames[1], "on", object$vnames[2], "\n\n")
        
        wmc$stars <- " "                      
        wmc$stars[wmc$p <   .1] <- "."
        wmc$stars[wmc$p <  .05] <- "*"
        wmc$stars[wmc$p <  .01] <- "**"
        wmc$stars[wmc$p < .001] <- "***"
        names(wmc)[which(names(wmc)=="stars")] <- " " 
        
        cat("\nMultiple Comparisons (2-Sample Permutation Tests)\n")  
        cat(paste("Probability Adjustment = ", object$method, "\n", sep=""))   
        print(wmc, ...)
        cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 
            1\n")
}
################################################################################
# predictive R squared
# http://www.r-bloggers.com/can-we-do-better-than-r-squared/
# http://blog.minitab.com/blog/adventures-in-statistics/multiple-regession-analysis-use-adjusted-r-squared-and-predicted-r-squared-to-include-the-correct-number-of-variables
pred_r_squared <- function(linear.model) {
        lm.anova <- anova(linear.model)
        tss <- sum(lm.anova$"Sum Sq")
        # predictive R^2
        pred.r.squared <- 1 - PRESS(linear.model)/(tss)
        return(pred.r.squared)
}

PRESS <- function(linear.model) {
        pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
        PRESS <- sum(pr^2)
        return(PRESS)
}







###########################
#' @title Model Fit Statistics
#' @description Returns lm model fit statistics R-squared, adjucted R-squared,
#'      predicted R-squared and PRESS.
#'      Thanks to John Mount for his 6-June-2014 blog post, R style tip: prefer functions that return data frames" for
#'      the idea \link{http://www.win-vector.com/blog/2014/06/r-style-tip-prefer-functions-that-return-data-frames}
#' @return Returns a data frame with one row and a column for each statistic
#' @param linear.model A \code{lm()} model.
model_fit_stats <- function(linear.model) {
        model <- as.character(linear.model$call[2]) #summary(linear.model)$call[1], 
        r.sqr <- summary(linear.model)$r.squared
        adj.r.sqr <- summary(linear.model)$adj.r.squared
        pre.r.sqr <- pred_r_squared(linear.model)
        PRESS <- PRESS(linear.model)
        return.df <- data.frame(
                model = model,
                r.squared = r.sqr,
                adj.r.squared = adj.r.sqr,
                pred.r.squared = pre.r.sqr,
                press = PRESS
)
        return(return.df)
}

###########################
#' @title Predictive R-squared
#' @author Thomas Hopper
#' @description returns the predictive r-squared. Requires the function PRESS(), which returns
#'              the PRESS statistic.
#' @param linear.model A linear regression model (class 'lm'). Required.
#'
pred_r_squared <- function(linear.model) {
        #' Use anova() to get the sum of squares for the linear model
        if(sum(class(linear.model)=="lmp")>0){
                tss <- summary(aov(linear.model))[[1]][, 'Sum Sq']
                tss <- sum(tss)
        }else{
                lm.anova <- anova(linear.model)
                #' Calculate the total sum of squares
                tss <- sum(lm.anova$'Sum Sq')  
        }

        # Calculate the predictive R^2
        pred.r.squared <- 1-PRESS(linear.model)/(tss)
        
        return(pred.r.squared)
}

###########################
#' @title PRESS
#' @author Thomas Hopper
#' @description Returns the PRESS statistic (predictive residual sum of squares).
#'              Useful for evaluating predictive power of regression models.
#' @param linear.model A linear regression model (class 'lm'). Required.
#' 
PRESS <- function(linear.model) {
        #' calculate the predictive residuals
        pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
        #' calculate the PRESS
        PRESS <- sum(pr^2)
        
        return(PRESS)
}

########
# Example usage for model_fit_stats.R

# Set up some data
x <- seq(from=0, to=10, by=0.5)
y1 <- 2*x + rnorm(length(x))


# We want to compare two different linear models:
my.lm1 <- lm(y1 ~ sin(x))
my.lm2 <- lm(y1 ~ x)

# We will use plyr for this.
library(plyr)

# Now call model_fit_stats() for each lm model that
# we have, using ldply. This returns the results in
# a data frame that is easily used for further 
# calculations.
ldply(list(my.lm1, my.lm2), model_fit_stats)

# adply() also works, though it should be less robust
# than ldply().
#adply(list(my.lm1, my.lm2), 1, model_fit_stats)

