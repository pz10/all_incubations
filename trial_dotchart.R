x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
dotchart(x$mpg,
         labels = row.names(x),
         cex=.7,
         groups = x$cyl,
         gcolor = "black",
         color = x$color,
         pch=19,
         main = "Gas Mileage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon")
toplot <- data.frame(toplot)
dotchart(toplot$diff,
        labels = row.names(toplot))



setkey(data, NO, precipitation, fertilizer, tillage, treatment)
data[precipitation=="i", color:= "deepskyblue"]
data[precipitation=="d", color:= "dodgerblue4"]
data[precipitation=="c", color:= "forestgreen"]
data[, NOlow := NO - 2*NOsd, by=treatment]
data[, NOhigh := NO + 2*NOsd, by=treatment]
setkey(data, NO,  precipitation, fertilizer, tillage, treatment, color)

mydata$fertilizer <- as.character(mydata$fertilizer)
dotchart(mydata$NO,
         labels = mydata$treatment,
         cex=.7,
         groups = mydata$precipitation,
         gcolor = "black",
         color = mydata$color,
         pch=19,
#          xlim= range(mydata$NO),
#          ylim = c(1,1),
         main = "NO 1st event\by treatment",
         xlab = "NO emissions")

dotchart(data$NO, labels=data$treatment, cex=.7,
         main="cumulative NO emission before 2nd rain event",
         xlab="cumulative NO emission")

for (i in 1:nrow(data)){
        lines(x=c(data$NOlow[i],data$NOhigh[i]), y=c(i,i))
}
