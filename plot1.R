plot1 <- function() {
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
  hist(
    filtered_data$Global_active_power, 
    col = "red", 
    xlab = "Global Active Power (kilowatts)", 
    main = "Global Active Power"
  )
  dev.copy(png, width = 480, height = 480, "plot1.png")
  dev.off()
  
  # return data for tests
  filtered_data
}
