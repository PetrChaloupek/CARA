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
FRED_API_KEY=95afb798f45e5bc52f67c5ae1ab7ef19

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
orders <- expand.grid(p = c(0:10), q = c(0:10))

CPImodels <- lapply(1:nrow(orders), function(i) {
  order <- as.numeric(orders[i, ])
  CPImodel <- arima(CPI$value_stac, order = c(order[1], 0, order[2]))
  # print(CPImodel)
  return(CPImodel)
})

IRmodels <- lapply(1:nrow(orders), function(i) {
  order <- as.numeric(orders[i, ])
  IRmodel <- arima(IR$value_stac, order = c(order[1], 0, order[2]))
  # print(IRmodel)
  return(IRmodel)
})

############################### ULOHA C. 3 #####################################
# Hledani nejlepsiho modelu pro CPI
cpi_aic <- sapply(CPImodels, AIC)
names(cpi_aic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))

best_cpi_model <- CPImodels[[which.min(cpi_aic)]]
print("Nejlepčí CPI model:")
print(best_cpi_model)

#Hledani nejlepsich modelu pro CPI podle BIC
cpi_bic <- sapply(CPImodels, BIC)
names(cpi_bic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))

# Seřazení modelů podle BIC
sorted_cpi_indices <- order(cpi_bic)[1]  # První nejlepší model podle BIC
best_cpi_models_bic <- CPImodels[sorted_cpi_indices]

print("Nejlepší CPI modely podle BIC:")
print(best_cpi_models_bic)

# Hledani nejlepsiho modelu pro IR pomocí AIC
ir_aic <- sapply(IRmodels, AIC)
names(ir_aic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))

best_ir_model <- IRmodels[[which.min(ir_aic)]]
print("Nejlepčí IR model:")
print(best_ir_model)

# Hledání nejlepšího modelů pro IR pomocí BIC
ir_bic <- sapply(IRmodels, BIC)
names(ir_bic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))

sorted_ir_indices <- order(ir_bic)[1]  # Nejlepší model podle BIC
best_ir_models_bic <- IRmodels[sorted_ir_indices]

print("Nejlepší IR modely podle BIC:")
print(best_ir_models_bic)

# Analýza reziduí

checkresiduals(best_cpi_model)
checkresiduals(best_ir_model)

# Q-testy

Box.test(e_best_cpi, lag = 20, type = "Ljung-Box")
Box.test(e_best_ir, lag = 20, type = "Ljung-Box")

# Vykreslenie inverznych korenov
library(ggplot2)

plot_inv_roots <- function(model, title) {
  roots <- lapply(c("ar", "ma"), function(x) 1 / polyroot(c(1, -model$coef[grep(x, names(model$coef))])))
  df <- data.frame(Re = unlist(lapply(roots, Re)), 
                   Im = unlist(lapply(roots, Im)), 
                   Type = rep(c("AR", "MA"), sapply(roots, length)))
  
  ggplot(df, aes(Re, Im, color = Type)) + 
    geom_point(size = 3) +
    annotate("path", x = cos(seq(0, 2*pi, length.out = 100)), 
             y = sin(seq(0, 2*pi, length.out = 100)), linetype = "dashed") + 
    scale_color_manual(values = c("indianred", "skyblue")) +  # Červená pre AR, modrá pre MA
    labs(title = title, x = "Reálna časť", y = "Imaginárna časť") + 
    coord_fixed() + theme_minimal()
}

plot_inv_roots(best_cpi_model, "Inverzné korene - CPI") 
plot_inv_roots(best_ir_model, "Inverzné korene - IR")

############################### ULOHA C. 4 #####################################
  library(dplyr)
  library(ggplot2)
  library(gridExtra)  
  
  # Orezanie dát na správnu dĺžku
  CPI_stac <- CPI[-1, ]
  IR_stac <- IR[-1, ]
  
  CPI_stac$fitted_AIC <- fitted(best_cpi_model)[1:nrow(CPI_stac)]
  CPI_stac$fitted_BIC_1 <- fitted(best_cpi_models_bic[[1]])[1:nrow(CPI_stac)]
  
  IR_stac$fitted_AIC <- fitted(best_ir_model)[1:nrow(IR_stac)]
  IR_stac$fitted_BIC_1 <- fitted(best_ir_models_bic[[1]])[1:nrow(IR_stac)]
  
# Vykreslenie fittovaných hodnôt v jednom grafe
  plot_fit <- function(data, fitted_column, title, color) {
    ggplot(data, aes(x = date)) +
      geom_line(aes(y = value_stac), color = "black", lwd = 1) +  # Skutočné hodnoty
      geom_line(aes(y = .data[[fitted_column]]), color = color, lwd = 1, linetype = "dashed") +  # Fittované hodnoty
      theme_bw() +
      labs(title = title, x = "Čas", y = "Delta log hodnoty") +
      scale_x_date(date_labels = "%Y", date_breaks = "10 years") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  
# Vytvorenie 4 samostatných grafov
  p1 <- plot_fit(CPI_stac, "fitted_AIC", "CPI - Najlepší model podľa AIC ARMA (10,10)", "indianred")
  p2 <- plot_fit(CPI_stac, "fitted_BIC_1", "CPI - Najlepší model podľa BIC ARMA (1,2)", "skyblue")
  p3 <- plot_fit(IR_stac, "fitted_AIC", "IR - Najlepší model podľa AIC ARMA (4,5)", "indianred")
  p4 <- plot_fit(IR_stac, "fitted_BIC_1", "IR - Najlepší model podľa BIC ARMA (3,2)", "skyblue")
  
# Usporiadanie všetkých 4 grafov do mriežky 2x2
  grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

############################### ULOHA C. 5 #####################################








