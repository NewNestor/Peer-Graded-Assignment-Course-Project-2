 plot3 <- function(){
+   library("ggplot2")
+   subNEI <- NEI[NEI$fips=="24510", c("Emissions", "year", "type")]
+   
+   subNEI$year <- as.factor(subNEI$year)
+   subNEI$type <- as.factor(subNEI$type)
+   
+   freqPerYear <- aggregate(Emissions~year + type, subNEI, FUN=sum)
+   
+   subNEI <- NULL
+   
+   plot3 <- ggplot(freqPerYear, aes(year, Emissions, fill=type))+ guides(fill=FALSE)+geom_bar(stat="identity")+facet_grid(.~type) +xlab("Year") + ylab("PM2.5 (tons)") + ggtitle("PM2.5 Total Emissions in Baltimore City, Marland (by Type)")
+   
+   print(plot3)
+   ## Save file and close device
+   #ggsave(plot3, file="plot3.png")
+ }
> 
> plot3()
