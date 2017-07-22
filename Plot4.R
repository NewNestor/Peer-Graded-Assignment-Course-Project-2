 plot4 <- function(){
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
