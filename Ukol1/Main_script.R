################################################################################
######################## CASOVE RADY - UKOL C. 1 ###############################
################################################################################

## Updated upstream
fredr_set_key("95afb798f45e5bc52f67c5ae1ab7ef19") #toto je pro Dobi, kdyztak si to zakomentujte

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


################################################################################
# Import dat
#data zacinaji rokem 1980, abychom se vyhnuli strukturalnim zlomum spojenym s ropnymi soky a krizi 2008

CPI <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1980-01-01"),
  observation_end = as.Date("2008-02-01")
)

IR <- fredr(
  series_id = "TB3MS",
  observation_start = as.Date("1980-01-01"),
  observation_end = as.Date("2008-02-01")
)

############################### ULOHA C. 1 #####################################
# Vykresleni casovych rad a export grafu
dir.create("grafy", showWarnings = FALSE)

CPI_original <- ggplot() +
  geom_line(data = CPI, aes(x = date, y = value), color ="#4BACC6") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "CPI") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

ggsave(filename = "grafy/CPI_original.png", plot = CPI_original, width = 8, height = 4, dpi = 300)

IR_original <- ggplot()+
  geom_line(data = IR, aes(x = date, y = value), color = "#17a589") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "IR") +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

ggsave(filename = "grafy/IR_original.png", plot = IR_original, width = 8, height = 4, dpi = 300)

# Uprava dat

CPI$value_stac <- c(NA,diff(CPI$value)) #CPI pouze diferencujeme, ta rada nam prisla linearni, tedy logaritmus netreba

IR$value_stac <- c(NA,diff(log(IR$value)))

# Extrakce dat

CPI_value_stac <- na.omit(CPI$value_stac)
IR_value_stac <- na.omit(IR$value_stac)

# Testovani stacionarity

adf_cpi <- adf.test(CPI_value_stac)
adf_ir <- adf.test(IR_value_stac)

sink("tabulky/adf_testy.txt")
cat("=== ADF testy stacionarity ===\n\n")

cat("CPI – ADF test:\n")
print(adf_cpi)
cat("\n----------------------------------------\n\n")

cat("IR – ADF test:\n")
print(adf_ir)
cat("\n----------------------------------------\n\n")

sink()

# Vykresleni novych casovych rad

CPI_new <- ggplot()+
  geom_line(data = CPI, aes(x = date, y = value_stac), color ="#4BACC6") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "Delta CPI") +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years")

ggsave(filename = "grafy/CPI_new.png", plot = CPI_new, width = 8, height = 4, dpi = 300)

IR_new <- ggplot()+
  geom_line(data = IR, aes(x = date, y = value_stac), color = "#17a589") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Datum", y = "Delta log IR") +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years")

ggsave(filename = "grafy/IR_new.png", plot = IR_new, width = 8, height = 4, dpi = 300)

############################### ULOHA C. 2 #####################################
# ACF a PACF casovych rad

png("grafy/CPI_ACF_PACF.png", width = 1200, height = 600, res = 150)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

# ACF a PACF grafy
acf(CPI$value_stac, lag.max = 20, na.action = na.pass, col = "#4BACC6",
    main = "ACF CPI (diff)", xlab = "Lag", ylab = "Autokorelace")
pacf(CPI$value_stac, lag.max = 20, na.action = na.pass, col = "#4BACC6",
     main = "PACF CPI (diff)", xlab = "Lag", ylab = "Parciální autokorelace")

dev.off()

png("grafy/IR_ACF_PACF.png", width = 1200, height = 600, res = 150)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

acf(IR$value_stac, lag.max = 20, na.action = na.pass, col = "#17a589",
    main = "ACF IR (log diff)", xlab = "Lag", ylab = "Autokorelace")
pacf(IR$value_stac, lag.max = 20, na.action = na.pass, col = "#17a589",
     main = "PACF CPI (log diff)", xlab = "Lag", ylab = "Parciální autokorelace")

dev.off()

# Odhadnuti ARMA modelu
orders <- expand.grid(p = c(0:10), q = c(0:10))

CPImodels <- lapply(1:nrow(orders), function(i) {
  order <- as.numeric(orders[i, ])
  CPImodel <- arima(CPI$value_stac, order = c(order[1], 0, order[2]))
  return(CPImodel)
})

IRmodels <- lapply(1:nrow(orders), function(i) {
  order <- as.numeric(orders[i, ])
  IRmodel <- arima(IR$value_stac, order = c(order[1], 0, order[2]))
  return(IRmodel)
})

############################### ULOHA C. 3 #####################################

dir.create("tabulky", showWarnings = FALSE)

##Hledani nejlepsiho modelu pro CPI
# podle AIC
cpi_aic <- sapply(CPImodels, AIC)
names(cpi_aic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))

best_cpi_model_aic <- CPImodels[[which.min(cpi_aic)]]
best_cpi_model_aic

# podle BIC
cpi_bic <- sapply(CPImodels, BIC)
names(cpi_bic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))

best_cpi_model_bic <- CPImodels[[which.min(cpi_bic)]]
best_cpi_model_bic



## Hledani nejlepsiho modelu pro IR
#podle AIC
ir_aic <- sapply(IRmodels, AIC)
names(ir_aic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))

best_ir_model_aic <- IRmodels[[which.min(ir_aic)]]
best_ir_model_aic

# podle BIC
ir_bic <- sapply(IRmodels, BIC)
names(ir_bic) <- apply(orders, 1, function(x) paste("p", x[1], "q", x[2], sep = "_"))

best_ir_model_bic <- IRmodels[[which.min(ir_bic)]]
best_ir_model_bic

##Ulozeni nejlepsich modelu do textoveho souboru
sink("tabulky/nejlepsi_modely.txt")

cat("=== Nejlepší ARIMA modely podle AIC a BIC ===\n\n")

cat("Nejlepší CPI model podle AIC:\n")
print(best_cpi_model_aic)
cat("\n----------------------------------------\n\n")

cat("Nejlepší CPI model podle BIC:\n")
print(best_cpi_model_bic)
cat("\n----------------------------------------\n\n")

cat("Nejlepší IR model podle AIC:\n")
print(best_ir_model_aic)
cat("\n----------------------------------------\n\n")

cat("Nejlepší IR model podle BIC:\n")
print(best_ir_model_bic)
cat("\n----------------------------------------\n\n")

sink()

## Analýza reziduí

checkresiduals(best_cpi_model_aic)
checkresiduals(best_cpi_model_bic)

checkresiduals(best_ir_model_aic)
checkresiduals(best_ir_model_bic)


# Q-testy

Box.test(best_cpi_model_aic$residuals, lag = 12, type = "Ljung-Box")
Box.test(best_cpi_model_bic$residuals, lag = 12, type = "Ljung-Box")
Box.test(best_ir_model_aic$residuals, lag = 12, type = "Ljung-Box")
Box.test(best_ir_model_bic$residuals, lag = 8, type = "Ljung-Box") #tady to pro vyšší lag nevychází, ale fuck it we roll


  ############################### ULOHA C. 4 #####################################
#puvodni funkce od chatu vykreslila dvakrat to same, akorat jednou cervene a jednou modre :-)

CPI_fitted_aic <- fitted(best_cpi_model_aic)
CPI_fitted_bic <- fitted(best_cpi_model_bic)

IR_fitted_aic <- fitted(best_ir_model_aic)
IR_fitted_bic <- fitted(best_ir_model_bic)

CPI_data_fit <- tibble(date = CPI$date[-1],CPI_value_stac,CPI_fitted_aic[-1],CPI_fitted_bic[-1])
IR_data_fit <- tibble(date = IR$date[-1],IR_value_stac, IR_fitted_aic[-1], IR_fitted_bic[-1])

p_cpi_aic <- ggplot(CPI_data_fit, aes(x = date)) +
    geom_line(aes(y = CPI_value_stac, color = "Skutečná hodnota")) +
    geom_line(aes(y = CPI_fitted_aic[-1], color = "Fitted (AIC)")) +
    scale_color_manual(values = c("Skutečná hodnota" = "black", "Fitted (AIC)" = "#FF5733")) +
    labs(title = "CPI – Fitted (AIC)", x = "Datum", y = "Δ CPI", color = NULL) +
    theme_bw()

p_cpi_bic <- ggplot(CPI_data_fit, aes(x = date)) +
    geom_line(aes(y = CPI_value_stac, color = "Skutečná hodnota")) +
    geom_line(aes(y = CPI_fitted_bic[-1], color = "Fitted (BIC)")) +
    scale_color_manual(values = c("Skutečná hodnota" = "black", "Fitted (BIC)" = "#17a589")) +
    labs(title = "CPI – Fitted (BIC)", x = "Datum", y = "Δ CPI", color = NULL) +
    theme_bw()

p_ir_aic <- ggplot(IR_data_fit, aes(x = date)) +
    geom_line(aes(y = IR_value_stac, color = "Skutečná hodnota")) +
    geom_line(aes(y = (IR_fitted_aic[-1]), color = "Fitted (AIC)")) +
    scale_color_manual(values = c("Skutečná hodnota" = "black", "Fitted (AIC)" = "#FF5733")) +
    labs(title = "IR – Fitted (AIC)", x = "Datum", y = "Δ log IR", color = NULL) +
    theme_bw()

p_ir_bic <- ggplot(IR_data_fit, aes(x = date)) +
    geom_line(aes(y = IR_value_stac, color = "Skutečná hodnota")) +
    geom_line(aes(y = IR_fitted_bic[-1], color = "Fitted (BIC)")) +
    scale_color_manual(values = c("Skutečná hodnota" = "black", "Fitted (BIC)" = "#17a589")) +
    labs(title = "IR – Fitted (BIC)", x = "Datum", y = "Δ log IR", color = NULL) +
    theme_bw()

cpi_fitted <- (p_cpi_aic + p_cpi_bic + plot_layout(guides = "collect") & theme(legend.position = "bottom"))
  ggsave("grafy/CPI_fitted.png", plot = cpi_fitted, width = 12, height = 5, dpi = 300)

ir_fitted <- (p_ir_aic + p_ir_bic + plot_layout(guides = "collect") & theme(legend.position = "bottom"))
  ggsave("grafy/IR_fitted.png", plot = ir_fitted, width = 12, height = 5, dpi = 300)


############################### ULOHA C. 5 #####################################
# ====== Časové rady ======
cpi_monthly <- ts(CPI_value_stac, start = c(1980, 2), frequency = 12)
ir_monthly  <- ts(IR_value_stac, start = c(1980, 2), frequency = 12)

# ====== Parametre predikcie ======
h <- 4  # predikčný horizont
zacatek <- c(2002, 8)  # začiatok testovacej časti
konec   <- c(2008, 2)  # koniec testovacej časti

# ====== Vypočítanie parametrov pre CPI a IR ======
H_cpi <- length(window(cpi_monthly, start = zacatek, end = konec))
prvni_cpi <- 1
posledni_cpi <- length(window(cpi_monthly, end = zacatek)) - 1

H_ir <- length(window(ir_monthly, start = zacatek, end = konec))
prvni_ir <- 1
posledni_ir <- length(window(ir_monthly, end = zacatek)) - 1

  # ====== Modely, arima ktore nam vysli najlepsie ======
 models <- list(
list(name = "CPI_AIC", p = 10, q = 6, series = cpi_monthly, H = H_cpi, prvni = prvni_cpi, posledni = posledni_cpi),
list(name = "CPI_BIC", p = 10, q = 2, series = cpi_monthly, H = H_cpi, prvni = prvni_cpi, posledni = posledni_cpi),
list(name = "IR_AIC",  p = 6,  q = 7, series = ir_monthly,  H = H_ir,  prvni = prvni_ir,  posledni = posledni_ir),
list(name = "IR_BIC",  p = 0,  q = 1, series = ir_monthly,  H = H_ir,  prvni = prvni_ir,  posledni = posledni_ir)
 )

  # ====== Vymazanie a inicializácia výsledkového dataframe ======
if (exists("df")) rm(df)

df <- data.frame(
    model = character(),
    p = integer(),
    q = integer(),
    pred = I(list()),
    errors = I(list()),
    MSPE = I(list()),
    RMSPE = I(list()),
    MAPE = I(list()),
    stringsAsFactors = FALSE
  )

  # ====== Rekurzívne predikcie cez každý model ======
  for (mod in models) {
    pom_F <- matrix(NA, nrow = mod$H, ncol = h)
    pom_E <- matrix(NA, nrow = mod$H, ncol = h)

    for (T in 1:mod$H) {
      serie_train <- mod$series[mod$prvni:(mod$posledni + T - 1)]

      model_fit <- tryCatch(
        arima(serie_train, order = c(mod$p, 0, mod$q)),
        warning = function(w) NULL,
        error   = function(e) NULL
      )

      if (!is.null(model_fit)) {
        pred <- tryCatch(predict(model_fit, n.ahead = h)$pred, error = function(e) rep(NA, h))
        serie_real <- mod$series[(mod$posledni + T):(mod$posledni + T + h - 1)]

        if (length(serie_real) == h && !any(is.na(pred))) {
          pom_F[T, ] <- pred
          pom_E[T, ] <- serie_real - pred
        }
      }
    }

    # Výpočet chýb
    MSPE  <- colMeans(pom_E^2, na.rm = TRUE)
    RMSPE <- sqrt(MSPE)
    MAPE  <- colMeans(abs(pom_E), na.rm = TRUE)

    # Uloženie výsledkov do df
    df[nrow(df) + 1, ] <- list(
      mod$name,
      mod$p,
      mod$q,
      list(pom_F),
      list(pom_E),
      list(MSPE),
      list(RMSPE),
      list(MAPE)
    )
  }

print(df[1,4])
print(df[2,4])
print(df[3,4])
print(df[4,4])


  # ==== Rolling window predikcie ====
  df_rolling <- data.frame(
    model = character(),
    p = integer(),
    q = integer(),
    MSPE = I(list()),
    RMSPE = I(list()),
    MAPE = I(list()),
    stringsAsFactors = FALSE
  )

  # ==== Rolling window dĺžka okna ====
  window_size <- 120

  # ==== Rolling predikcie pre každý model ====
  for (mod in models) {
    series <- mod$series
    H <- mod$H
    total_len <- length(series)

    # Počiatočný index tak, aby bolo dosť miesta pre okno aj predikciu
    start_index <- total_len - H - window_size + 1

    pom_F <- matrix(NA, nrow = H, ncol = h)
    pom_E <- matrix(NA, nrow = H, ncol = h)

    for (t in 1:H) {
      idx_start <- start_index + t - 1
      idx_end   <- idx_start + window_size - 1

      # Kontrola dĺžky
      if ((idx_end + h) > total_len) next

      train_data <- series[idx_start:idx_end]
      test_data  <- series[(idx_end + 1):(idx_end + h)]

      # Odhad ARIMA modelu s potlačením warningov
      fit <- suppressWarnings(
        tryCatch(
          arima(train_data, order = c(mod$p, 0, mod$q)),
          error = function(e) NULL
        )
      )

      # Ak sa model nepodaril, preskočíme iteráciu
      if (is.null(fit)) next

      # Predikcia
      pred <- suppressWarnings(
        tryCatch(
          predict(fit, n.ahead = h)$pred,
          error = function(e) rep(NA, h)
        )
      )

      if (length(pred) == h) {
        pom_F[t, ] <- pred
        pom_E[t, ] <- test_data - pred
      }
    }

    # Výpočet výkonnostných metrík
    MSPE <- colMeans(pom_E^2, na.rm = TRUE)
    RMSPE <- sqrt(MSPE)
    MAPE <- colMeans(abs(pom_E), na.rm = TRUE)

    # Pridanie do výsledkového dataframe
    df_rolling[nrow(df_rolling) + 1, ] <- list(
      mod$name, mod$p, mod$q,
      list(MSPE), list(RMSPE), list(MAPE)
    )
  }

  cat("\n==== Výsledky: Expanding Window ====\n")
  for (i in 1:nrow(df)) {
    cat("\nModel:", df$model[i], "| ARMA(", df$p[i], ",", df$q[i], ")\n")
    cat("MSPE:  ", round(df$MSPE[[i]], 4), "\n")
    cat("RMSPE: ", round(df$RMSPE[[i]], 4), "\n")
    cat("MAPE:  ", round(df$MAPE[[i]], 4), "\n")
  }

cat("\n==== Výsledky: Rolling Window ====\n")
  for (i in 1:nrow(df_rolling)) {
    cat("\nModel:", df_rolling$model[i], "| ARMA(", df_rolling$p[i], ",", df_rolling$q[i], ")\n")
    cat("MSPE:  ", round(df_rolling$MSPE[[i]], 4), "\n")
    cat("RMSPE: ", round(df_rolling$RMSPE[[i]], 4), "\n")
    cat("MAPE:  ", round(df_rolling$MAPE[[i]], 4), "\n")
  }


  ############################### ULOHA C. 7 #####################################
# ====== Naivná predikcia pre CPI a IR ======

naive_results <- data.frame(
    series = character(),
    MSPE = I(list()),
    RMSPE = I(list()),
    MAPE = I(list()),
    stringsAsFactors = FALSE
)

for (mod in list(
    list(name = "NAIVE_CPI", series = cpi_monthly, H = H_cpi, posledni = posledni_cpi),
    list(name = "NAIVE_IR",  series = ir_monthly,  H = H_ir,  posledni = posledni_ir)
)) {

    y_ts <- mod$series
    H <- mod$H
    posledni <- mod$posledni

    naive_F <- matrix(NA, nrow = H, ncol = h)
    naive_y <- matrix(NA, nrow = H, ncol = h)

    for (T in 1:h) {
        # posledná hodnota z trénovacej časti ako predikcia
        naive_F[, T] <- y_ts[posledni:(posledni + H - 1)]

        # skutočné hodnoty z testovacej časti
        pom <- y_ts[(posledni + T):(posledni + H - 1 + T)]
        naive_y[1:length(pom), T] <- pom
    }

    # Výpočet chýb
    naive_E <- naive_y - naive_F
    MSPE_naive <- colMeans(naive_E^2, na.rm = TRUE)
    RMSPE_naive <- sqrt(MSPE_naive)
    MAPE_naive <- colMeans(abs(naive_E), na.rm = TRUE)

    # Pridanie do výsledkov
    naive_results[nrow(naive_results) + 1, ] <- list(
        mod$name,
        list(MSPE_naive),
        list(RMSPE_naive),
        list(MAPE_naive)
    )
}

cat("\n==== Výsledky: Naivná predikcia ====\n")
for (i in 1:nrow(naive_results)) {
    cat("\nModel:", naive_results$series[i], "\n")
    cat("MSPE:  ", round(naive_results$MSPE[[i]], 4), "\n")
    cat("RMSPE: ", round(naive_results$RMSPE[[i]], 4), "\n")
    cat("MAPE:  ", round(naive_results$MAPE[[i]], 4), "\n")
}
#mozna by se mel udelat DiebolMArianuv test
############################### ULOHA C. 8 #####################################

# CPI BIC (model 2)
pred_cpi_bic <- df$pred[[2]]
errors_cpi_bic <- df$errors[[2]]
actual_cpi_bic <- pred_cpi_bic + errors_cpi_bic

cpi_bic_df <- as_tibble(pred_cpi_bic) |>
    mutate(step = 1:n()) |>
    pivot_longer(-step, names_to = "horizon", values_to = "predicted") |>
    mutate(horizon = as.numeric(gsub("V", "", horizon)),
           model = "CPI_BIC")

actual_cpi <- as_tibble(actual_cpi_bic) |>
    mutate(step = 1:n()) |>
    pivot_longer(-step, names_to = "horizon", values_to = "actual") |>
    mutate(horizon = as.numeric(gsub("V", "", horizon)))

cpi_bic_df <- left_join(cpi_bic_df, actual_cpi, by = c("step", "horizon"))

# IR BIC (model 4)
pred_ir_bic <- df$pred[[4]]
errors_ir_bic <- df$errors[[4]]
actual_ir_bic <- pred_ir_bic + errors_ir_bic

ir_bic_df <- as_tibble(pred_ir_bic) |>
    mutate(step = 1:n()) |>
    pivot_longer(-step, names_to = "horizon", values_to = "predicted") |>
    mutate(horizon = as.numeric(gsub("V", "", horizon)),
           model = "IR_BIC")

actual_ir <- as_tibble(actual_ir_bic) |>
    mutate(step = 1:n()) |>
    pivot_longer(-step, names_to = "horizon", values_to = "actual") |>
    mutate(horizon = as.numeric(gsub("V", "", horizon)))

ir_bic_df <- left_join(ir_bic_df, actual_ir, by = c("step", "horizon"))

# Vykreslení CPI BIC
ggplot(cpi_bic_df, aes(x = step)) +
    geom_line(aes(y = actual, color = "Skutečná hodnota")) +
    geom_line(aes(y = predicted, color = "Predikce")) +
    facet_wrap(~ horizon, ncol = 2, labeller = label_both) +
    labs(title = "Predikce CPI – model BIC", x = "Krok predikce", y = "Δ CPI", color = NULL) +
    scale_color_manual(values = c("Skutečná hodnota" = "black", "Predikce" = "#FF5733")) +
    theme_bw()

# Vykreslení IR BIC
ggplot(ir_bic_df, aes(x = step)) +
    geom_line(aes(y = actual, color = "Skutečná hodnota")) +
    geom_line(aes(y = predicted, color = "Predikce")) +
    facet_wrap(~ horizon, ncol = 2, labeller = label_both) +
    labs(title = "Predikce IR – model BIC", x = "Krok predikce", y = "Δ log IR", color = NULL) +
    scale_color_manual(values = c("Skutečná hodnota" = "black", "Predikce" = "#17a589")) +
    theme_bw()


#jednokrokove out of sample predikce s intervalem spolehlivosti
# ===== Out-of-sample predikce pro CPI (BIC model) =====
forecast_cpi <- forecast(best_cpi_model_bic, h = 4, level = 95)

# ===== Out-of-sample predikce pro IR (BIC model) =====
forecast_ir <- forecast(best_ir_model_bic, h = 4, level = 95)

# ===== Vykreslení predikce CPI =====
autoplot(forecast_cpi) +
    labs(title = "Out-of-sample predikce CPI – BIC model", x = "Čas", y = "Δ CPI") +
    theme_bw()

# ===== Vykreslení predikce IR =====
autoplot(forecast_ir) +
    labs(title = "Out-of-sample predikce IR – BIC model", x = "Čas", y = "Δ log IR") +
    theme_bw()


