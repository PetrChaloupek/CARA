################################################################################
######################## CASOVE RADY - UKOL C. 1 ###############################
################################################################################

rm(list = ls())
cat("\014")

# Stazeni potrebnych balicku
library(dplyr)
library(ggplot2)
library(tidyverse)
library(zoo)
library(forecast)
library(fredr)
library(scales)

################################################################################
# Import dat

CPI <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2025-02-01")
)

IR <- fredr(
  series_id = "TB3MS",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2025-02-01")
)

############################### ULOHA C. 1 #####################################
# Vykresleni casovych rad

ggplot()+
  geom_line(data = CPI, aes(x = CPI$date, y = CPI$value), color ="#4BACC6") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "CPI") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

ggplot()+
  geom_line(data = IR, aes(x = IR$date, y = IR$value), color = "#17a589") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "IR") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

# Uprava dat

CPI$value_stac <- c(NA,diff(log(CPI$value)))

IR$value_stac <- c(NA,diff(IR$value))

# Vykresleni novych casovych rad

ggplot()+
  geom_line(data = CPI, aes(x = CPI$date, y = CPI$value_stac), color ="#4BACC6") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "Delta log CPI") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

ggplot()+
  geom_line(data = IR, aes(x = IR$date, y = IR$value_stac), color = "#17a589") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "Delta IR") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

############################### ULOHA C. 2 #####################################

# ACD a PACF casovych rad

par(mfrow = c(1,2))
acf(CPI$value_stac, lag.max = 20, na.action = na.pass, col = "#4BACC6")
pacf(CPI$value_stac, lag.max = 20, na.action = na.pass, col = "#4BACC6")

par(mfrow = c(1,2))
acf(IR$value_stac, lag.max = 20, na.action = na.pass, col = "#17a589")
pacf(IR$value_stac, lag.max = 20, na.action = na.pass, col = "#17a589")

# Odhadnuti ARMA modelu
orders <- expand.grid(p = c(0,1,2,3), q = c(0,1,2,3))

CPImodels <- lapply(1:nrow(orders), function(i) {
  order <- as.numeric(orders[i, ])
  CPImodel <- arima(CPI$value_stac, order = c(order[1], 0, order[2]))
  print(CPImodel)
  return(CPImodel)
})

IRmodels <- lapply(1:nrow(orders), function(i) {
  order <- as.numeric(orders[i, ])
  IRmodel <- arima(IR$value_stac, order = c(order[1], 0, order[2]))
  print(IRmodel)
  return(IRmodel)
})

