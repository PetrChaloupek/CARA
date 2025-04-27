################################################################################
######################## CASOVE RADY - UKOL C. 2 ###############################
################################################################################
rm(list = ls())
cat("\014")

fredr_set_key("95afb798f45e5bc52f67c5ae1ab7ef19") #toto je pro Dobi, kdyztak si to zakomentujte

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
library(strucchange) #napr pro cusum test
library(urca)
################################################################################
# Import dat

Y <- fredr(
    series_id = "GDPC1",
    observation_start = as.Date("1980-01-01"),
    observation_end = as.Date("2019-01-01"),
    frequency = "q"
)

U <- fredr(
    series_id = "U2RATE",
    observation_start = as.Date("1980-01-01"),
    observation_end = as.Date("2019-01-01"),
    frequency = "q"
)

############################### ULOHA C. 1 #####################################
# Vykresleni casovych rad
#HDP
p1 <- ggplot() +
    geom_line(data = Y, aes(x = date, y = value), color ="#4BACC6") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Datum", y = "HDP") +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years")

p1

ggsave(filename = "grafy/y_original.png", plot = p1, width = 8, height = 4, dpi = 300)

#Nezamestnanost
p2 <- ggplot()+
    geom_line(data = U, aes(x = date, y = value), color = "#17a589") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Datum", y = "Nezamestnanost") +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years")

p2

ggsave(filename = "grafy/u_original.png", plot = p2, width = 8, height = 4, dpi = 300)

#Overenie stacionarity
adf.test(Y$value)# nestacionarna
adf.test(U$value)# nestacionarna

#Stacionarizacia pomocou obycajnych prvych diferencii
Y$value_stac <- c(NA, diff(log(Y$value)))
U$value_stac <- c(NA,diff(U$value))

# Test stacionarity po transformácii
adf.test(Y$value_stac[-1]) # stacionarne
adf.test(U$value_stac[-1]) # stacionarne
?adf.test


# Vykreslenie grafu pre Y_stac
p3 <- ggplot(Y, aes(x = date, y = value_stac)) +
    geom_line(color = "#4BACC6") +  # Farba čiar
    theme_bw() +  # Použitie čistého štýlu
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Datum", y = "HDP") +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
    ggtitle("Stacionarizovaný HDP")

p3

ggsave(filename = "grafy/y_stac.png", plot = p3, width = 8, height = 4, dpi = 300)

# Vykreslenie grafu pre U_stac
p4 <- ggplot(U, aes(x = date, y = value_stac)) +
    geom_line(color = "#17a589") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Datum", y = "Nezamestnanosť") +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
    ggtitle("Stacionarizovaná nezamestnanost")

p4

ggsave(filename = "grafy/u_stac.png", plot = p4, width = 8, height = 4, dpi = 300)

# AR model pre Y_diff (hladanie najlepsieho modelu)
#zvolili jsme BIC, protoze BIC je nejlepsi
ar_model_Y <- auto.arima(Y$value_stac, d = 0, max.q = 0, ic = "bic", stepwise = FALSE, approximation = FALSE)
summary(ar_model_Y) # Najlepsi pre Y je AR(1)

# AR model pre U_diff
ar_model_U <- auto.arima(U$value_stac, d = 0, max.q = 0, ic = "bic", stepwise = FALSE, approximation = FALSE)
summary(ar_model_U) # Najlepsi pre U je AR(1)



# Detekcia zlomu pre HDP a nezamestnanost
cusum_test_Y <- efp(ar_model_Y$residuals ~ 1, type = "Rec-CUSUM")
plot(cusum_test_Y)
cusum_test_U <- efp(ar_model_U$residuals ~ 1, type = "Rec-CUSUM")
plot(cusum_test_U)


bp_Y <- breakpoints(ar_model_Y$residuals ~ 1) #nenasla jsem zlom
bp_U <- breakpoints(ar_model_U$residuals ~ 1) #nema zlom

#ten zlom tam neni a uplne nevim, proc, ale mam za to, ze to delam dobre
# Graficky iba HDP so zlomom
p5 <- ggplot(Y, aes(x = date, y = value_stac)) +
    geom_line(color = "#4BACC6") +
    geom_vline(xintercept = Y$date[bp_Y$breakpoints],
               color = "red", linetype = "dashed") +
    theme_bw() +
    labs(x = "Datum", y = "HDP") +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
    ggtitle("Štrukturálny zlom v stacionárnej rade HDP")

p5

ggsave(filename = "grafy/y_struc_break.png", plot = p5, width = 8, height = 4, dpi = 300)

# Zivot-Andrews test jednotkoveho korene - hledá potenciální strukturální zlomy
za_U <- ur.za(U$value_stac, model = c("intercept"), lag=NULL)
summary(za_U)
U$date[117]
za_Y <- ur.za(Y$value_stac, model = c("intercept"), lag=NULL)
summary(za_Y)
Y$date[11]
