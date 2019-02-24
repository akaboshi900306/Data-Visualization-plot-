library(ggthemes)
library(ggplot)
library(reshape)

air<- read.csv("C:/Users/akabo/Downloads/data visualization/AirlineDelayData2003-2017.csv")
air1<-air[,c(3,9,10,11,12,13)]
air2 <-aggregate(cbind(carrier_ct, weather_ct,nas_ct,security_ct,late_aircraft_ct) ~ carrier, data=air1, FUN=sum)
air2$id <- seq.int(nrow(air2))
air2$sum <- air2$carrier_ct+ air2$weather_ct+ air2$nas_ct + air2$security_ct+ air2$late_aircraft_ct
Air<- melt(air2, id=c("id", "carrier","sum"))
colnames(Air)[4] <- "causes"
colnames(Air)[5] <- "numbers"
p1 <- ggplot(Air) + geom_bar(aes(y= numbers, x=carrier, fill= causes ), stat="identity")
p1
#install.packages("forcats")
#library(forcats)

p2 <- p1 + scale_fill_manual("", labels=c("Air carrier Delay", "Weather Delay", "National Aviation System Delay","Security Delay","Aircraft arriving late"), values=c("#D7D6CB", "#B1C0C9", "#809EAD","#d6cbd7","#fbdc10")) + scale_x_discrete("") + scale_y_continuous("numbers of cases",breaks=seq(0, 310000000, by=500000)) +theme_fivethirtyeight() +
theme(axis.text.x=element_text(angle=45, hjust = 1), legend.position = "right", legend.direction = "vertical")

p3 <- p2 + geom_text(aes(y= numbers, x=carrier,label=ifelse(Air$per >=0.35 & Air$numbers>250000, paste(sprintf("%.0f",Air$per*100,"%")),"")), position=position_stack(vjust=0.0295), color="white") +labs(title="Flight Delay by Causes of different carriers from 2003 to 2017", subtitle=" What's the major reason that so many flights delayed every year?")
p3
p4<- p3+labs(caption="The numbers on the chart stands for %")
p4
