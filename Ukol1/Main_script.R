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
library(tseries)

################################################################################
# Import dat

CPI <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2008-02-01")
)

IR <- fredr(
  series_id = "TB3MS",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2002-02-01")
)

############################### ULOHA C. 1 #####################################
# Vykresleni casovych rad

ggplot()+
  geom_line(data = CPI, aes(x = date, y = value), color ="#4BACC6") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "CPI") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

ggplot()+
  geom_line(data = IR, aes(x = date, y = value), color = "#17a589") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "IR") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

# Uprava dat

CPI$value_stac <- c(NA,diff(log(CPI$value)))

IR$value_stac <- c(NA,diff(log(IR$value)))

# Extrakce dat

CPI_value_stac <- na.omit(CPI$value_stac)
IR_value_stac <- na.omit(IR$value_stac)

# Testovani stacionarity

adf.test(CPI_value_stac)
adf.test(IR_value_stac)

# Vykresleni novych casovych rad

ggplot()+
  geom_line(data = CPI, aes(x = date, y = value_stac), color ="#4BACC6") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "Delta log CPI") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

ggplot()+
  geom_line(data = IR, aes(x = date, y = value_stac), color = "#17a589") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "Delta log IR") +
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

############################### ULOHA C. 3 #####################################
# Hledani nejlepsiho modelu pro CPI

cpi_aic <- sapply(CPImodels, AIC)
names(cpi_aic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))

best_cpi_model <- CPImodels[[which.min(cpi_aic)]]
print("Nejlepčí CPI model:")
print(best_cpi_model)

# Hledani nejlepsiho modelu pro IR
ir_aic <- sapply(IRmodels, AIC)
names(ir_aic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))

best_ir_model <- IRmodels[[which.min(ir_aic)]]
print("Nejlepčí IR model:")
print(best_ir_model)

# Analýza reziduí

checkresiduals(best_cpi_model)
checkresiduals(best_ir_model)

# Q-testy

Box.test(e_best_cpi, lag = 20, type = "Ljung-Box")
Box.test(e_best_ir, lag = 20, type = "Ljung-Box")


############################### ULOHA C. 4 #####################################
#Zobrazenie povodnej casovej rady s fittovanymi hodnotami
library(dplyr)

# Získanie fittovaných hodnôt a ich orezanie na správnu dĺžku
CPI_stac <- CPI[-1, ] %>% mutate(fitted = fitted(best_cpi_model)[1:(nrow(.) )])
IR_stac <- IR[-1, ] %>% mutate(fitted = fitted(best_ir_model)[1:(nrow(.) )])

# Funkcia na vykreslenie časového radu s fittovanými hodnotami
plot_fit <- function(data, title, color) {
  ggplot(data) +
    geom_line(aes(x = date, y = value_stac), color = "black", lwd = 1) +   # Pôvodná séria
    geom_line(aes(x = date, y = fitted), color = color, lwd = 1, linetype = "dashed") +  # Fit ARMA
    theme_bw() +
    labs(title = title, x = "Čas", y = "Delta log hodnoty") +
    scale_x_date(date_labels = "%Y", date_breaks = "10 years") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# Vykreslenie grafov
plot_fit(CPI_stac, "Časová rada CPI + Fit", "indianred")
plot_fit(IR_stac, "Časová rada IR + Fit", "skyblue")

############################### ULOHA C. 5 #####################################
#Predikovanie 1 až 4 krokmi









