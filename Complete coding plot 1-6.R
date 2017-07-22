> plot1 <- function(){
+   subNEI <- NEI[ , c("Emissions", "year")]
+   subNEI$year=as.factor(subNEI$year)
+   freqPerYear <- aggregate(Emissions~year,subNEI, FUN=sum)
+   subNEI <- NULL
+   with(freqPerYear,barplot(Emissions, names=year, xlab = "Year", ylab = "PM2.5 (tons)", main = "PM2.5 Total Emissions in the United States"))
+   #dev.off()
+ }
> 
> plot1()
> 
> plot2 <- function(){
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
> 
> plot3 <- function(){
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
> 
> plot4 <- function(){
+   combustion <-grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
+   coal <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
+   coalCombustion <- (coal & combustion)
+   ccSCC <- SCC[coalCombustion,]$SCC
+   SCC <- NULL
+   coal <- NULL
+   combustion <- NULL
+   
+   NEI$year <- as.factor(NEI$year)
+   
+   ccNEI <- NEI[NEI$SCC %in% ccSCC,]
+   ccNEI <- ccNEI[,c("year", "Emissions")]
+   
+   s <- aggregate(Emissions~year,ccNEI,FUN=sum)
+   ccNEI <- NULL
+   
+   plot4 <- ggplot(s,aes(year,Emissions))+geom_bar(stat="identity")+xlab("Year")+ylab("PM2.5 (tons)")+ggtitle("Coal Combustion-related Emissions in US")
+   
+   print(plot4)
+   ## Save file and close device
+   #ggsave(plot4, file="plot4.png")
+ }
> 
> plot4()
> 
> plot5 <- function(){
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
> 
> plot6 <- function(){
+   vehicles <-grepl("vehicle", SCC$SCC.Level.Two, ignore.case = TRUE)
+   vSCC <- SCC[vehicles,]$SCC
+   
+   NEI$year <- as.factor(NEI$year)
+   subNEI <- NEI[NEI$fips=="24510" | NEI$fips == "06037", c("fips", "SCC", "Emissions", "year")]
+   subNEI$fips <- gsub("24510", "Baltimore City", subNEI$fips)  
+   subNEI$fips <- gsub("06037", "LA County, California", subNEI$fips) 
+   subNEI$fips <- as.factor(subNEI$fips)
+   
+   vNEI <- subNEI[subNEI$SCC %in% vSCC, ]
+   subNEI <- NULL
+   vNEI <- vNEI[,c("fips", "year", "Emissions")]
+   
+   s <- aggregate(Emissions~year+fips, vNEI,FUN=sum)
+   
+   plot6 <- ggplot(s,aes(year, Emissions, fill=fips))+geom_bar(stat="identity")+facet_grid(.~fips)+xlab("Year")+ylab("PM2.5 (tons)")+ggtitle("PM2.5 Emissions from Motor Vehicles in LA County and Baltimore City")+guides(fill=FALSE)
+   
+   print(plot6)
+   ## Save file and close device
+   #ggsave(plot6, file="plot6.png")
+   
+   ## graph with another method
+   #png(file = "plot6_V2.png")
+   #q <- ggplot(RoadData2, aes(x = factor(year), y = Emissions, fill = city))
+   #q1 <- q + stat_summary(fun.y = sum, position = position_dodge(), geom = "bar")
+   #q2 <- q1 + labs(title = "Motor Vehicle Emissions from LA County, CA and Baltimore, MD", x = "Year", y = expression ( "Emissions:  " * PM[2.5](tons), fill = "city"))
+   #q2 + scale_y_continuous(breaks=seq(0,4500,500))
+   #dev.off()
+ }
> 
> plot6()
> 
