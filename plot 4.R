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
  select(dia_sem, dia, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  group_by(dia_sem, dia) %>% 
  summarise(mean(Sub_metering_1, na.rm = T), mean(Sub_metering_2, na.rm = T), mean(Sub_metering_3, na.rm = T))

x$hora <- format(x$dia, format = "%H")
colnames(x) <- c("dia", "dia_sem","Sub_metering_1","Sub_metering_2", "Sub_metering_3", "hora")
x <- x %>% 
  group_by(hora, dia) %>% 
  summarise(mean(Sub_metering_1, na.rm = T), mean(Sub_metering_2, na.rm = T), mean(Sub_metering_3, na.rm = T))

colnames(x) <- c("hora", "dia_sem","Sub_metering_1","Sub_metering_2", "Sub_metering_3")

x$hora <- as.numeric(x$hora)

x1 <- data %>% 
  select(dia_sem, dia, Global_active_power) %>% 
  group_by(dia_sem, dia) %>% 
  summarise(mean(Global_active_power, na.rm = T))

x1$hora <- format(x1$dia, format = "%H")
colnames(x1) <- c("dia", "dia_sem","Global_active_power", "hora")
x1 <- x1 %>% 
  group_by(hora, dia) %>% 
  summarise(mean(Global_active_power, na.rm = T))

colnames(x1) <- c("hora", "dia_sem","Global_active_power")

x1$hora <- as.numeric(x1$hora)


par(mfrow = c(2,2), mar = c(4,4,2,1))
# Crear un lienzo de gráfico base
plot(x$hora, x$Sub_metering_1, type = "l", col = "blue", xlab = "Hora", ylab = "Valor")

# Agregar la segunda línea
lines(x$hora, x$Sub_metering_2, col = "red")

# Agregar la tercera línea
lines(x$hora, x$Sub_metering_3, col = "green")
# Crear un lienzo de gráfico base
plot(x1$hora, x1$Global_active_power, type = "l", xlab = "Hora", ylab = "Global_active_power")

# Agregar una línea
lines(x1$hora, x1$Global_active_power)

hist(data$Global_active_power , col = "red", freq = F, 
     xlab = "Global Active Power (kW)", ylab = "Frecuencia", main = "Global Active Power")
plot(data$Date, data$Voltage, type = "1")
