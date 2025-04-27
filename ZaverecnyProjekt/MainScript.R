################################################################################
#################### CASOVE RADY - ZAVERECNY PROJEKT ###########################
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
library(gridExtra)

# Import dat ###################################################################

# Ceny bydlení
property_prices <- fredr(
  series_id = "QCAN628BIS",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Smenny kurz (USD/CAD)
XR <- fredr(
  series_id = "CCUSMA02CAM618N",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Menove politicka urokova mira
IR <- fredr(
  series_id = "IRSTCB01CAM156N",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Index spotrebitelskych cen
CPI <- fredr(
  series_id = "CPALCY01CAM661N",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Realny Hruby domaci produkt
GDP <- fredr(
  series_id = "NGDPRSAXDCCAQ",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Menovy agregat M2
M <- fredr(
  series_id = "MAM2A2CAM189S",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Index ceny prace
WPI <- fredr(
  series_id = "CANHOUREAMISMEI",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "q"
)

# Vykresleni dat ##############################################################
datasets <- list(
  list(
    data = property_prices,
    title = "Ceny bydlení",
    color = "darkblue",
    y_label = "Index cen (2010 = 100)"
  ),
  list(
    data = XR,
    title = "Směnný kurz",
    color = "darkred",
    y_label = "USD/CAD"
  ),
  list(
    data = IR,
    title = "Měnově politická úroková míra",
    color = "darkgreen",
    y_label = "%"
  ),
  list(
    data = CPI,
    title = "Index spotřebitelských cen",
    color = "purple",
    y_label = "CPI (2015 = 100)"
  ),
  list(
    data = GDP,
    title = "Reálný HDP",
    color = "orange",
    y_label = "Miliony CAD (stálé ceny)"
  ),
  list(
    data = M,
    title = "Měnový agregát M2",
    color = "brown",
    y_label = "Miliony CAD"
  ),
  list(
    data = WPI,
    title = "Index ceny práce",
    color = "darkturquoise",
    y_label = "Index (2015 = 100)"
  )
)

plots <- list()
for (i in 1:length(datasets)) {
  plots[[i]] <- ggplot(datasets[[i]]$data, aes(x = date, y = value)) +
    geom_line(color = datasets[[i]]$color, size = 1) +
    labs(title = datasets[[i]]$title,
         x = "Rok",
         y = datasets[[i]]$y_label) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8))
}

grid.arrange(grobs = plots, 
             ncol = 2, 
             top = "Původní časové řady")

# Transformace dat #############################################################


