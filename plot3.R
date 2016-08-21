library(data.table)

readDataSet<-function()
{
    graphDataSet<-read.table("household_power_consumption.txt",
                             sep = ";",
                             na.strings = "?",
                             header = TRUE)
    selectDataSet<-graphDataSet[graphDataSet$Date %in% c("1/2/2007","2/2/2007"),]
    selectDataSet[,1]<-as.Date(selectDataSet[,1],format="%d/%m/%Y")
    mergedDateTime<-as.data.frame(paste(selectDataSet[,1],selectDataSet[,2],sep = " "))
    mergedDateTime[,1]<-as.data.frame(strptime(mergedDateTime[,1],format="%Y-%m-%d %H:%M:%S"))
    names(mergedDateTime)<-c("DateTime")
    
    mergedDS<-cbind(mergedDateTime,selectDataSet[,-(1:2)])
    
    mergedDS
}

plot3<-function()
{
    graphDS<-readDataSet()
    png(file="plot3.png",height=480,width = 480)
    plot( graphDS$DateTime,
          graphDS$Sub_metering_1,
          ylab = "Energy Sub Metering",
          type = "l",
          xlab=""
    )
    lines( graphDS$DateTime,
          graphDS$Sub_metering_2,
          col="red"
    )
    lines( graphDS$DateTime,
           graphDS$Sub_metering_3,
           col="blue"
    )
    legend("topright",
           c("Sub_metering_1","Sub_metering_2","Sub_metering_3") , 
           lty=1, 
           col=c("black", "red", "blue"))
    dev.off()
}