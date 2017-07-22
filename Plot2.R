 plot2 <- function(){
+   subNEI <- NEI[NEI$fips=="24510", c("Emissions", "year")]
+   subNEI$year=as.factor(subNEI$year)
+   freqPerYear <- aggregate(Emissions~year,subNEI, FUN=sum)
+   subNEI <- NULL
+   with(freqPerYear,barplot(Emissions, names = year, xlab = "Year", ylab = "PM2.5 (tons)", main = "PM2.5 Total Emissions in Baltimore City, Marland"))
+   
+   ## Save file and close device
+   #dev.copy(png,"plot2.png", width=480, height=480)
+   #dev.off()
+ }
> 
> plot2()

