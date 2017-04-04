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
