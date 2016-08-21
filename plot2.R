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

plot2<-function()
{
    graphDS<-readDataSet()
    png(file="plot2.png",height=480,width = 480)
    plot( graphDS$DateTime,
          graphDS$Global_active_power,
          ylab = "Global Active Power (Kilowatts)",
          type = "l",
          xlab=""
    )
    dev.off()
}