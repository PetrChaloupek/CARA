################################################################################
######################## CASOVE RADY - UKOL Č. 1 ###############################
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

############################### ULOHA Č. 1 #####################################
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

  