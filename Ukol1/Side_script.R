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

IR$value_stac <- c(NA,diff(log(IR$value)))

# Extrakce dat
CPI_value_stac <- na.omit(CPI$value_stac)
IR_value_stac <- na.omit(IR$value_stac)

# Testovani stacionarity
adf.test(CPI_value_stac)
adf.test(IR_value_stac)

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
#####
# Extrakce AIC pro CPI modely
cpi_aic <- sapply(CPImodels, AIC)
names(cpi_aic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))
print("AIC hodnoty pro CPI modely:")
print(cpi_aic)

# Najít model s nejnižším AIC pro CPI
best_cpi_model_idx <- which.min(cpi_aic)
best_cpi_model <- CPImodels[[best_cpi_model_idx]]
print("Nejlepší CPI model (index a kombinace p,q):")
print(best_cpi_model_idx)
print(orders[best_cpi_model_idx, ])

# Extrakce AIC pro IR modely
ir_aic <- sapply(IRmodels, AIC)
names(ir_aic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))
print("AIC hodnoty pro IR modely:")
print(ir_aic)

# Najít model s nejnižším AIC pro IR
best_ir_model_idx <- which.min(ir_aic)
best_ir_model <- IRmodels[[best_ir_model_idx]]
print("Nejlepší IR model (index a kombinace p,q):")
print(best_ir_model_idx)
print(orders[best_ir_model_idx, ])



# Definice orders (pokud ještě není definováno)
orders <- expand.grid(p = c(0, 1, 2, 3), q = c(0, 1, 2, 3))

# Extrakce AIC pro CPI modely
cpi_aic <- sapply(CPImodels, AIC)
names(cpi_aic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))
print("AIC hodnoty pro CPI modely:")
print(cpi_aic)

# Najít model s nejnižším AIC pro CPI
best_cpi_model_idx <- which.min(cpi_aic)
best_cpi_model <- CPImodels[[best_cpi_model_idx]]
print("Nejlepší CPI model (index a kombinace p,q):")
print(best_cpi_model_idx)
print(orders[best_cpi_model_idx, ])

# Extrakce AIC pro IR modely
ir_aic <- sapply(IRmodels, AIC)
names(ir_aic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))
print("AIC hodnoty pro IR modely:")
print(ir_aic)

# Najít model s nejnižším AIC pro IR
best_ir_model_idx <- which.min(ir_aic)
best_ir_model <- IRmodels[[best_ir_model_idx]]
print("Nejlepší IR model (index a kombinace p,q):")
print(best_ir_model_idx)
print(orders[best_ir_model_idx, ])

# Výpis detailů nejlepších modelů
print("Detail nejlepšího CPI modelu:")
print(summary(best_cpi_model))
print("Detail nejlepšího IR modelu:")
print(summary(best_ir_model))

# Rezidua nejlepšího CPI modelu
e_best_cpi <- residuals(best_cpi_model)
e_best_cpi <- na.omit(e_best_cpi)

# Analýza reziduí CPI
acf(e_best_cpi, lag.max = 20, na.action = na.pass, col = "#4BACC6", main = "ACF reziduí - CPI")
pacf(e_best_cpi, lag.max = 20, na.action = na.pass, col = "#4BACC6", main = "PACF reziduí - CPI")
hist(e_best_cpi, col = "#4BACC6", main = "Histogram reziduí - CPI", xlab = "Reziduum", prob = TRUE)
curve(dnorm(x, mean = mean(e_best_cpi), sd = sd(e_best_cpi)), add = TRUE, col = "red", lwd = 2)

# Test normality pro CPI
print("Shapiro-Wilk test pro CPI rezidua:")
print(shapiro.test(e_best_cpi))

# Rezidua nejlepšího IR modelu
e_best_ir <- residuals(best_ir_model)
e_best_ir <- na.omit(e_best_ir)

# Analýza reziduí IR
acf(e_best_ir, lag.max = 20, na.action = na.pass, col = "#4BACC6", main = "ACF reziduí - IR")
pacf(e_best_ir, lag.max = 20, na.action = na.pass, col = "#4BACC6", main = "PACF reziduí - IR")
hist(e_best_ir, col = "#4BACC6", main = "Histogram reziduí - IR", xlab = "Reziduum", prob = TRUE)
curve(dnorm(x, mean = mean(e_best_ir), sd = sd(e_best_ir)), add = TRUE, col = "red", lwd = 2)

# Test normality pro IR
print("Shapiro-Wilk test pro IR rezidua:")
print(shapiro.test(e_best_ir))

# Výpis detailů nejlepších modelů
print("Detail nejlepšího CPI modelu:")
print(summary(best_cpi_model))
print("Detail nejlepšího IR modelu:")
print(summary(best_ir_model))


# Rezidua nejlepšího CPI modelu
e_best_cpi <- residuals(best_cpi_model)
e_best_cpi <- na.omit(e_best_cpi)

acf(e_best_cpi, lag.max = 20, na.action = na.pass, col = "#4BACC6")
pacf(e_best_cpi, lag.max = 20, na.action = na.pass, col = "#4BACC6")
hist(e_best_cpi, col = "#4BACC6")

shapiro.test(e_best_cpi)



