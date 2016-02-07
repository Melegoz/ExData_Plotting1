## Plot4

Plot4<- function(){
  
  ## Code to source the data
  ## please note: code written for Windows, if you have a Mac you need to add Method="Curl" to be able to download the file
  
  
  library(httr) 
  
  # create a directory named "data" where to place the downloaded data
  data <- "data"
  if(!file.exists(data)){
    dir.create(data)
  } 
  
  url_powerConsumption <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  
  # check if the data set has been downloaded, otherwise download the data into folder "data"
  archive <- paste(getwd(), "/data/household_power_consumption.zip", sep = "")
  if(!file.exists(archive)){
    download.file(url_powerConsumption, archive, mode="wb")
  }
  
  
  # unzip the row data file and extract the txt dataset
  hpc <- paste(getwd(), "/data/household_power_consumption.txt", sep = "")
  if(!file.exists(hpc)){
    unzip(archive, list = FALSE, overwrite = FALSE, exdir = data)
  }
  
  # subset the dataset to the time period assigned (days between 1/02/2007 and 2/2/2007)
  df_hpc <- read.table(hpc, header=TRUE, sep=";", colClasses=c("character", "character", rep("numeric",7)), na.strings = "?")
  df_hpc$Date <- as.Date(df_hpc$Date, "%d/%m/%Y")
  selectedPeriod <- as.Date(c("2007-02-01", "2007-02-02"), "%Y-%m-%d")
  df_hpc<-subset(df_hpc, Date %in% selectedPeriod)
  

  ## Converting dates
  datetime <- paste(as.Date(df_hpc$Date), df_hpc$Time)
  df_hpc$Datetime <- as.POSIXct(datetime)
  
  
  # create Plots
  # First i prepare the area where to plot, therefore a png file with given dimension
  png(file="Plot4.png", height=480, width=480)
  # then i populaate the png with the plot
  ## with par function i set up the area and how i want the plot to be distributed 2 by 2
  par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
  ## i use the with function so to avoid recalling the dataframe name for each plot call
  with(df_hpc, {
    plot(Global_active_power~Datetime, type="l",
         ylab="Global Active Power", xlab="")
    plot(Voltage~Datetime, type="l",
         ylab="Voltage (volt)", xlab="")
    plot(Sub_metering_1~Datetime, type="l",
         ylab="Energy sub metering", xlab="")
    lines(Sub_metering_2~Datetime,col='Red')
    lines(Sub_metering_3~Datetime,col='Blue')
    legend("topright", col=c("black", "red", "blue"),
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), bty="n")
    plot(Global_reactive_power~Datetime, type="l",
         ylab="Global Rective Power",xlab="", lwd=.001)
  })
  
  # Close the print device before exit, otherwise you won't be able to open the png file
  dev.off()
  
}