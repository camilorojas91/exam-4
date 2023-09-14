library(rio)
library(dplyr)
library(lubridate)


data <- import("./data/household_power_consumption.txt")

data <- data %>% 
  mutate_all(~replace(., . == "?", NA))

data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data$Time <- as.POSIXct(data$Time, format =  "%H:%M:%S")
data$Time <- format(data$Time, format = "%H:%M:%S")

data$Global_active_power <- as.numeric(data$Global_active_power)
data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
data$Voltage <- as.numeric(data$Voltage)
data$Global_intensity <- as.numeric(data$Global_intensity)
data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)

summary(data)

hist(data$Global_active_power , col = "red", freq = F, 
     xlab = "Global Active Power (kW)", ylab = "Frecuencia", main = "Global Active Power")
 

