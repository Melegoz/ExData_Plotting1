## Plot2

Plot2<- function(){
  
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
  
  # create Plot2
  # First i prepare the area where to plot, therefore a png file with given dimension
  png(file="Plot2.png", height=480, width=480)
  # then i populaate the png with the plot
  plot(df_hpc$Global_active_power~df_hpc$Datetime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  
  # Close the print device before exit, otherwise you won't be able to open the png file
  dev.off()
  
  
}