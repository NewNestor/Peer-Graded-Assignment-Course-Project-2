plot1 <- function(){
+   subNEI <- NEI[ , c("Emissions", "year")]
+   subNEI$year=as.factor(subNEI$year)
+   freqPerYear <- aggregate(Emissions~year,subNEI, FUN=sum)
+   subNEI <- NULL
+   with(freqPerYear,barplot(Emissions, names=year, xlab = "Year", ylab = "PM2.5 (tons)", main = "PM2.5 Total Emissions in the United States"))
+   #dev.off()
+ }
> 
> plot1()
