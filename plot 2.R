library(rio)
library(dplyr)
library(lubridate)
library(ggplot2)

data <- import("./data/household_power_consumption.txt")

data <- data %>% 
  mutate_all(~replace(., . == "?", NA))

data$dia <- paste(data$Date, data$Time, sep=" ")
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data$dia <- as.POSIXct(data$dia, format =  "%d/%m/%Y %H:%M:%S")


data$Global_active_power <- as.numeric(data$Global_active_power)
data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
data$Voltage <- as.numeric(data$Voltage)
data$Global_intensity <- as.numeric(data$Global_intensity)
data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)

data$dia_sem <- weekdays(data$dia)
table(data$dia_sem)


x <- data %>% 
  select(dia, dia_sem, Global_active_power) %>% 
  group_by(dia, dia_sem) %>% 
  summarise(mean(Global_active_power, na.rm = T))

colnames(x) <- c("dia", "dia_sem","Global_active_power")


ggplot(data = x,aes(x = dia, y = "Global_active_power")) +
  geom_line() +  # Utiliza puntos para mostrar las medias
  labs(x = "Día de la Semana", y = "Media de datos_x") +
  ggtitle("Media de datos_x por Día de la Semana y Hora")+
  facet_wrap(~ dia_sem, scales = "free_x")
