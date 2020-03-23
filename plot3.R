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
  data <- read_delim(
    "data/household_power_consumption.txt", 
    ";", 
    col_names = TRUE,
    col_types = cols("c", "c", "d", "d", "d", "d", "d", "d", "d"),
    na = c("?")
  )
  
  # preprocess data
  library(dplyr)
  filtered_data <- data %>%
    filter(Date %in% c("1/2/2007", "2/2/2007")) %>%
    mutate(date_and_time = as.POSIXct(strptime(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")))
  
  # preset layout
  par(mfcol = c(1, 1))
  
  # make plot
  plot(
    filtered_data$Sub_metering_1 ~ filtered_data$date_and_time, 
    xlab = "", 
    ylab = "Energy sub metering"
  )
  lines(filtered_data$Sub_metering_2 ~ filtered_data$date_and_time, col = "red")
  lines(filtered_data$Sub_metering_3 ~ filtered_data$date_and_time, col = "blue")
  legend(
    "topright", 
    legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
    col = c("black", "red", "blue"),
    lty = 1,
    cex = 0.75
  )
  dev.copy(png, width = 480, height = 480, "plot3.png")
  dev.off()
  
  # return data for tests
  filtered_data
}
