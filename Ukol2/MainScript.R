################################################################################
######################## CASOVE RADY - UKOL C. 2 ###############################
################################################################################

rm(list = ls())
cat("\014")

# Nacteni potrebnych balicku
library(dplyr)
library(ggplot2)
library(tidyverse)
library(zoo)
library(forecast)
library(fredr)
library(scales)
library(tseries)
library(gridExtra)
library(patchwork)
library(magrittr)
library(forecast)

################################################################################
# Import dat

Y <- fredr(
  series_id = "GDPC1",
  observation_start = as.Date("1980-01-01"),
  observation_end = as.Date("2008-01-01"),
  frequency = "q"
)

U <- fredr(
  series_id = "U2RATE",
  observation_start = as.Date("1980-01-01"),
  observation_end = as.Date("2008-01-01"),
  frequency = "q"
)

############################### ULOHA C. 1 #####################################