plot3 <- function() {
  # get file
  zipFile <- "source.zip"
  if (!file.exists(zipFile)) {
    loadStatus <- download.file(
      "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
      destfile = zipFile
    )
    if (loadStatus != 0) {
      stop(paste("failed to get file: ", as.character(loadStatus), sep = ""))
    }
  } else {
    message(paste("Use cached file ", zipFile, sep = ""))
  }
  
  # unzip file
  unzip(zipFile, overwrite = F, exdir = "data")
  
  # read file and get value
  library(readr)
  data <- read_delim("data/household_power_consumption.txt", ";", na = c("?"))
  
  # preprocess data
  library(dplyr)
  filtered_data <- data %>%
    mutate(date_and_time = as.POSIXct(strptime(paste(Date, as.character(Time)), format = "%d/%m/%Y %H:%M:%S"))) %>%
    filter(date_and_time >= as.POSIXct("2007-02-01 00:00:00") & date_and_time <= as.POSIXct("2007-02-02 23:59:59"))
  
  # make plot
  plot(
    filtered_data$Sub_metering_1 ~ filtered_data$date_and_time, 
    xlab = "", 
    ylab = "Energy sub metering", 
    type = "n"
  )
  lines(filtered_data$Sub_metering_1 ~ filtered_data$date_and_time)
  lines(filtered_data$Sub_metering_2 ~ filtered_data$date_and_time, col = "red")
  lines(filtered_data$Sub_metering_3 ~ filtered_data$date_and_time, col = "blue")
  legend(
    "topright", 
    legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
    col = c("black", "red", "blue")
  )
  dev.copy(png, "plot3.png")
  dev.off()
  
  # return data for tests
  filtered_data
}
