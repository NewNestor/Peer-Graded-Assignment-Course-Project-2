 plot5 <- function(){
+   vehicles <-grepl("vehicle", SCC$SCC.Level.Two, ignore.case = TRUE)
+   vSCC <- SCC[vehicles,]$SCC
+   
+   NEI$year <- as.factor(NEI$year)
+   subNEI <- NEI[NEI$fips=="24510", c("SCC", "Emissions", "year")]
+   
+   vNEI <- subNEI[subNEI$SCC %in% vSCC, ]
+   vNEI <- vNEI[,c("year", "Emissions")]
+   
+   s <- aggregate(Emissions~year, vNEI, FUN=sum)
+   vNEI <- NULL
+   
+   plot5 <- ggplot(s,aes(year,Emissions))+geom_bar(stat="identity")+xlab("Year")+ylab("PM2.5 (tons)")+ggtitle("Emissions from Vehicles in Baltimore City, Maryland")
+   
+   print(plot5)
+   ## Save file and close device
+   #ggsave(plot5, file="plot5.png")
+ }
> 
> plot5()
