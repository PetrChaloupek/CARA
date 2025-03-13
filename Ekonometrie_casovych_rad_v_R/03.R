################################################################################
############ EKONOMETRIE CASOVYCH RAD V SYSTEMU R - UKOL Č. 2 ##################
################################################################################
# Promazani promennych a konzole
rm(list = ls())
cat("\014")
# Stazeni potrebnych balicku
library(forecast)
library(readxl)
library(ggfortify)
library(dynlm)
library(car)
library(sandwich)
library(gridExtra)
library(tseries)
library(ggcorrplot)
library(tidyverse)
library(tstools)
library(greybox)
library(seasonal)
library(strucchange)
library(fUnitRoots)
library(urca)
library(nortsTest)
library(uroot)
library(mFilter)
library(ggplot2)
library(patchwork)
library(tseries)
library(reshape2)
library(fixest)

# Načtení dat
rm(list = ls())
data <- read_excel("data/data.xlsx", col_types = c("date",
                                                   "numeric", "numeric"))
# Priprava dat
data <- data %>% slice(5:nrow(data))
names(data) <- (c("date", "realGDP_raw", "U_raw"))
data <- data %>% mutate("id" = c(1:nrow(data)),
                        "logrealGDP_raw" = log(realGDP_raw)
                        )

realGDP <- ts(data$realGDP_raw, start = c(1961,1),frequency = 4)
logrealGDP <- ts(data$logrealGDP_raw, start = c(1961,1),frequency = 4)
U <- ts(data$U_raw, start = c(1961,1),frequency = 4)

############################### ULOHA Č. 1 #####################################

p1 <-  autoplot(realGDP, size = 1, color = "red",facets = FALSE)
p2 <- autoplot(logrealGDP, size = 1, color = "red",facets = FALSE)
p3 <- autoplot(U, size = 1, color = "red",facets = FALSE)
p4 <- ggAcf(realGDP, lag.max = 20) #ACF - az 20 zpozdeni
p5 <- ggAcf(logrealGDP, lag.max = 20)
p6 <- ggAcf(U, lag.max = 20)
p7 <- ggPacf(realGDP, lag.max = 20) #PACF - az 20 zpozdeni
p8 <- ggPacf(logrealGDP, lag.max = 20)
p9 <- ggPacf(U, lag.max = 20)

# Usporadani do mrizky
wrap <- wrap_plots(p1, p2, p3, p4, p5, p6, p7,p8, p9, ncol = 3, nrow = 3)
ggsave("patchwork_plot.png", plot = wrap, width = 10, height = 10)


# ADF test 
# Augmented Dickey-Fuller test for unit roots real GDP
adf_realGDP_1 <- adf.test(realGDP)
print(adf_realGDP_1)
adf_realGDP_2 <- urdfTest(realGDP, lags = 1, type = c("nc", "c", "ct"), doplot = TRUE)
adf_realGDP_3 <- ur.df(realGDP, type = c("trend"), lags = 4,
              selectlags = c( "BIC"))
summary(adf_realGDP_3)

# Augmented Dickey-Fuller test for unit roots log real GDP
adf_logrealGDP_1 <- adf.test(logrealGDP)
print(adf_realGDP_1)
adf_logrealGDP_2 <- urdfTest(logrealGDP, lags = 1, type = c("nc", "c", "ct"), doplot = TRUE)
adf_logrealGDP_3 <- ur.df(logrealGDP, type = c("trend"), lags = 4,
                       selectlags = c( "BIC"))
summary(adf_logrealGDP_3)

# Augmented Dickey-Fuller test for unit roots unemployment
adf_U_1 <- adf.test(U)
print(adf_U_1)
adf_U_2 <- urdfTest(U, lags = 1, type = c("nc", "c", "ct"), doplot = TRUE)
adf_U_3 <- ur.df(U, type = c("trend"), lags = 4,
                          selectlags = c( "BIC"))
summary(adf_U_3)

# Transformace časových řad na stacionární pomocí HP filtru
hplogrealGDP <- hpfilter(logrealGDP, 1600)$cycle
hpU <- hpfilter(U, 1600)$cycle

p10 <- autoplot(hplogrealGDP, size = 1, color = "red",facets = FALSE)
p11 <- autoplot(hpU, size = 1, color = "red",facets = FALSE)
wrap <- wrap_plots(p2, p10, p3, p11, ncol = 2, nrow = 2)
ggsave("detrended_and_raw.png", plot = wrap, width = 10, height = 10)


adf_hpU <- ur.df(hpU, type = "drift", lags = 4)
summary(adf_hpU)
adf_hplogrealGDP <-  ur.df(hplogrealGDP, type = "drift", lags = 4)
summary(adf_hplogrealGDP)

# Zivot-Andrews test jednotkoveho korene - hledá potenciální strukturální zlomy
za_hplogrealGDP <- ur.za(hplogrealGDP, model = c("both"), lag=NULL)
summary(za_hplogrealGDP)
za2_hplogrealGDP <- ur.za(hplogrealGDP, model = c("intercept"), lag=NULL)
summary(za2_hplogrealGDP)

za_hpU <- ur.za(hpU, model = c("both"), lag=NULL)
summary(za_hpU)
za2_hpU <- ur.za(hpU, model = c("intercept"), lag=NULL)
summary(za2_hpU)
# našlo to strukturální zlomy na pozici 236 nebo 237

############################### ULOHA Č. 2 #####################################

# Použití HP filtru

yc_hp <- hpfilter(logrealGDP,freq = 1600,type = "lambda",drift = FALSE)$cycle
autoplot(yc_hp, size = 1, color = "#4BACC6")

uc_hp <- hpfilter(U,freq = 1600,type = "lambda",drift = FALSE)$cycle
autoplot(uc_hp, size = 1, color = "#4BACC6")

# Použití polynomiálního trendu se strukturálním zlomem pro y
# zlom v 2020:1 (Covid)
tt <- 1:length(logrealGDP)
df_pol_D <- data.frame(tt)

# maximalni stupen polynomu
pp <- 5
tt <- ts(tt, start = c(1961,1),frequency = 4)

pom_ts <- ts.union(logrealGDP,tt)

my_formula <- "logrealGDP ~ 1"

for (ii in 1:pp)
{
    pom_str <- paste(paste(c("+I(tt^",ii,")"), collapse = ""))
    my_formula <- paste(my_formula, pom_str, collapse = "")

    pom_mod_1 <- lm(my_formula,window(pom_ts,start = c(1961,1), end = c(2020,1)))
    pom_mod_2 <- lm(my_formula,window(pom_ts,start = c(2020,2), end = c(2023,4)))


    pom_res <- c(residuals(pom_mod_1),residuals(pom_mod_2))
    pom_trend <- c(fitted(pom_mod_1),fitted(pom_mod_2))

    pom_nam <- paste(c("yc_pol_D",ii),collapse = "")
    df_pol_D[pom_nam] <- pom_res

    pom_nam = paste(c("yt_pol_D",ii),collapse = "")
    df_pol_D[pom_nam] <- pom_trend

}

# vyber jen cyklickych slozek z data frame
yc_po_df <- df_pol_D %>% select(contains("yc"))
yc_po_ts <- ts(yc_po_df, start = c(1961,1),frequency = 4)
rm(yc_po_df)

g <- autoplot(yc_po_ts, size = 1)
g <- g + xlab("Obdobi") + ylab("Mezera vystupu") + ggtitle("Polynomialni trendy - zlom")
print(g)
# polynomický trend se strukturálním zlomem pro nezaměstnanost
pom_ts <- ts.union(U,tt)

my_formula <- "U ~ 1"

for (ii in 1:pp)
{
    pom_str <- paste(paste(c("+I(tt^",ii,")"), collapse = ""))
    my_formula <- paste(my_formula, pom_str, collapse = "")

    pom_mod_1 <- lm(my_formula,window(pom_ts,start = c(1961,1), end = c(2020,1)))
    pom_mod_2 <- lm(my_formula,window(pom_ts,start = c(2020,2), end = c(2023,4)))


    pom_res <- c(residuals(pom_mod_1),residuals(pom_mod_2))
    pom_trend <- c(fitted(pom_mod_1),fitted(pom_mod_2))

    pom_nam <- paste(c("uc_pol_D",ii),collapse = "")
    df_pol_D[pom_nam] <- pom_res

    pom_nam = paste(c("ut_pol_D",ii),collapse = "")
    df_pol_D[pom_nam] <- pom_trend

}

uc_po_df <- df_pol_D %>% select(contains("uc"))
uc_po_ts <- ts(uc_po_df, start = c(1961,1),frequency = 4)
rm(uc_po_df)

g <- autoplot(uc_po_ts, size = 1)
g <- g + xlab("Obdobi") + ylab("Mezera vystupu") + ggtitle("Polynomialni trendy - zlom")
print(g)

# Jednoduchý klouzavý průměr s použitím balíčku stats
# definice vah pro odhadove okno
m <- 10 # delka klouzaveho prumeru => sirka okenka 2m+1 = 21
w <- rep(1/(2*m + 1),2*m + 1)
yc_sma_ts <- logrealGDP - stats::filter(logrealGDP, w, sides = 2)
yc_sma_ts <- ts(yc_sma_ts, start = c(1961,1),frequency = 4)

autoplot(logrealGDP - yc_sma_ts, size = 1, color = "#4BACC6")

uc_sma_ts <- U -  stats::filter(U, w, sides = 2)
uc_sma_ts <- ts(uc_sma_ts, start = c(1961,1),frequency = 4)
autoplot(U - yc_sma_ts, size = 1, color = "#4BACC6")

# Vážený klouzavý průměr s použitím balíčku stats
XX <- cbind(matrix(1,nrow = 2*m + 1,ncol = 1),
            matrix(-m:m,nrow = 2*m + 1,ncol = 1),
            matrix((-m:m)^2,nrow = 2*m + 1,ncol = 1))
# OLS estimator (X'X)^(-1)*X'*y ... vezmeme první řádek (X'X)^(-1)*X'
# odpovidajici odhadu urovnove konstante (vazeny prumer y)
pom <- solve(t(XX) %*% XX) %*% t(XX)
w <- pom[1,]
yc_wma_ts <- logrealGDP - stats::filter(logrealGDP, w, sides = 2)
yc_wma_ts <- ts(yc_wma_ts, start = c(1961,1),frequency = 4)
autoplot(logrealGDP - yc_wma_ts, size = 1, color = "darkblue")

uc_wma_ts <- U - stats::filter(U, w, sides = 2)
uc_wma_ts <- ts(uc_wma_ts, start = c(1961,1),frequency = 4)
autoplot(U - uc_wma_ts, size = 1, color = "darkblue")

# Vytvoření korelační matice pro y
#Sloučení časových řad
pom_yc_ts <- ts.union(yc_po_ts, yc_sma_ts, yc_wma_ts, yc_hp)
colnames(pom_yc_ts) <- c(colnames(yc_po_ts),
                      "yc_sma","yc_wma","yc_hp")

#Korelační matice
corr_yc <- round(cor(pom_yc_ts, use = "complete.obs"),3)
print(corr_yc)

#Matice p hodnot
p_yc <- cor_pmat(pom_yc_ts)
yc_corr_fig <- ggcorrplot(corr_yc,
           type = "lower", p.mat = p_yc,
           outline.color  = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("navy", "white", "#4BACC6"))


# Vytvoření korelační matice pro u
#Sloučení časových řad
pom_uc_ts <- ts.union(uc_po_ts, uc_sma_ts, uc_wma_ts, uc_hp)
colnames(pom_yc_ts) <- c(colnames(uc_po_ts),
                         "uc_sma","uc_wma","uc_hp")

#Korelační matice
corr_uc <- round(cor(pom_uc_ts, use = "complete.obs"),3)
print(corr_uc)

#Matice p hodnot
p_uc <- cor_pmat(pom_uc_ts)
uc_corr_fig <- ggcorrplot(corr_uc,
                          type = "lower", p.mat = p_uc,
                          outline.color  = "white",
                          ggtheme = ggplot2::theme_gray,
                          colors = c("navy", "white", "#4BACC6"))



corr <- wrap_plots(yc_corr_fig, uc_corr_fig, ncol = 2, nrow = 1)
ggsave("corr.png", plot = corr, width = 10, height = 5)

#Podíl období, kdy estimátory měly stejné znaménko
# Funkce pro výpočet podílu shodného znaménka
sign_match_ratio <- function(var1, var2) 
{
    match_count <- sum(sign(var1) == sign(var2))
    total_count <- length(var1)
    ratio <- match_count / total_count
    return(ratio)
}


sign_match_matrix_yc <- matrix(0, nrow = 8, ncol = 8)

pom_yc_ts_map <- pom_yc_ts[11:242,]

for (i in 1:8) 
{
    for (j in 1:8) 
    {
        sign_match_matrix_yc[i, j] <- sign_match_ratio(pom_yc_ts_map[, i], pom_yc_ts_map[, j])
    }
}


colnames(sign_match_matrix_yc) <- rownames(sign_match_matrix_yc) <- colnames(pom_uc_ts)


print(sign_match_matrix_yc)

sign_match_melted <- melt(sign_match_matrix_yc)
colnames(sign_match_melted) <- c("Var1", "Var2", "Ratio")

# Vytvoření tepelné mapy
yc_ratio <- ggplot(sign_match_melted, aes(x = Var1, y = Var2, fill = Ratio)) +
    geom_tile() +
    scale_fill_gradient2(low = "#4BACC6", high = "navy", mid = "white", 
                         midpoint = 0.75, limit = c(0.5, 1), space = "Lab", 
                         name="Ratio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = "Ratio of observations with the same sign",
         x = "Variables",
         y = "Variables")

#Podíl období, kdy estimátory měly stejné znaménko
# Funkce pro výpočet podílu shodného znaménka

sign_match_matrix_uc <- matrix(0, nrow = 8, ncol = 8)

pom_uc_ts_map <- pom_uc_ts[11:242,]

for (i in 1:8) 
{
    for (j in 1:8) 
    {
        sign_match_matrix_uc[i, j] <- sign_match_ratio(pom_uc_ts_map[, i], pom_uc_ts_map[, j])
    }
}


colnames(sign_match_matrix_uc) <- rownames(sign_match_matrix_uc) <- colnames(pom_uc_ts)


print(sign_match_matrix_uc)

sign_match_melted <- melt(sign_match_matrix_uc)
colnames(sign_match_melted) <- c("Var1", "Var2", "Ratio")

# Vytvoření tepelné mapy
uc_ratio <- ggplot(sign_match_melted, aes(x = Var1, y = Var2, fill = Ratio)) +
    geom_tile() +
    scale_fill_gradient2(low = "#4BACC6", high = "navy", mid = "white", 
                         midpoint = 0.75, limit = c(0.5, 1), space = "Lab", 
                         name="Ratio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = "Ratio of observations with the same sign",
         x = "Variables",
         y = "Variables")


ratio <- wrap_plots(yc_ratio, uc_ratio, ncol = 2, nrow = 1)
ggsave("ratio.png", plot = ratio, width = 10, height = 5)

############################### ULOHA Č. 4 #####################################

# Bai and Perron test pro více strukturálních zlomů
breakpoints_results <- list()

for (i in 1:8) 
{
  for (j in 1:3) 
  {
    bp_result <- breakpoints(pom_uc_ts[, i] ~ pom_yc_ts[, i], breaks = j)
    breakpoints_results[[paste0("Variable_", i, "_Breaks_", j)]] <- list(
      breakpoints = summary(bp_result)$breakpoints,
      breakdates = summary(bp_result)$breakdates
    )
  }
}

output_file <- "breakpoints_summary.txt"
file_conn <- file(output_file, open = "wt")

for (i in 1:8) 
{
        capture.output(print(breakpoints_results[[paste0("Variable_", i, 
                             "_Breaks_", 3)]]$breakdates), file = file_conn, append = TRUE)
        cat("\n\n", file = file_conn) # Přidání prázdných řádků mezi souhrny
        capture.output(print(breakpoints_results[[paste0("Variable_", i, 
                             "_Breaks_", 3)]]$breakpoints), file = file_conn, append = TRUE)
        cat("\n\n", file = file_conn) # Přidání prázdných řádků mezi souhrny
}


close(file_conn)

# vzhledem k tomu, že zlomy vychází dost nejednoznačně, rozhodl jsem se vycházet 
# z empirie a brát v úvahu zlomy v roce 2020 a roce 2008 a 1979 (ropny sok). 
# Po roce 2020 nemáme zvlášť v případě čas. řad. vytvořených klouzavými průměry 
# dost pozorování,proto odhadujeme Okunův koeficient před rokem 1979, mezi lety 
# 1979 a 2008 a po roce 2008.

#vytvoreni umelych promennych
uc <- as.tibble(pom_uc_ts)
yc <- as.tibble(pom_yc_ts)

names(yc) <- c("yc_1", "yc_2", "yc_3", "yc_4", "yc_5", "yc_6", "yc_7", "yc_8")
names(uc) <- c("uc_1", "uc_2", "uc_3", "uc_4", "uc_5", "uc_6", "uc_7", "uc_8")
data_okun <- bind_cols(uc,yc)
rm(uc,yc)

data_okun$break_1979 <- ifelse(data$date >= "1979-01-01", 1, 0)
data_okun$break_2008 <- ifelse(data$date >= "2008-10-01", 1, 0)

#vytvoreni interakcnich clenu
data_okun <- data_okun %>%
    mutate(date = seq.Date(from = as.Date("1961-01-01"), by = "quarter", 
                           length.out = nrow(data_okun)),
           break_1979 = ifelse(date >= "1979-01-01", 1, 0),
           break_2008 = ifelse(date >= "2008-10-01", 1, 0))

# Seznam názvů původních proměnných
original_vars <- names(data_okun)[1:16]

# Vytvoření interakčních členů s pojmenováním podle roku
for (var in original_vars) 
{
    data_okun <- data_okun %>%
        mutate(!!paste0(var, "_1979") := .data[[var]] * break_1979,
               !!paste0(var, "_2008") := .data[[var]] * break_2008)
}

# Odhad zakladniho tvaru modelu
results_list_base <- list()

for (i in 1:8) {
  # Dynamické generování názvů proměnných
  uc_var <- paste0("uc_", i)
  yc_var <- paste0("yc_", i)
  
  formula <- as.formula(paste(uc_var, "~", yc_var))
  
  model_base <- glm(formula, data = data_okun)
  
  # Extrakce koeficientů a p-hodnot
  coefficients <- summary(model_base)$coefficients
  
  # Korektní vytvoření datového rámce pro koeficienty a p-hodnoty
  coefs_pvals <- data.frame(
    Variable = rownames(coefficients),
    Estimate = coefficients[, 1],
    `Std. Error` = coefficients[, 2],
    `t value` = coefficients[, 3],
    `Pr(>|t|)` = coefficients[, 4]
  )
  
  # Uložení coefs_pvals do results_list_base
  results_list_base[[i]] <- coefs_pvals
}


#  Odhad modelu pro rok 1979

results_list_1979 <- list()

for (i in 1:8) 
{
    # Dynamické generování názvů proměnných
    uc_var <- paste0("uc_", i)
    yc_var <- paste0("yc_", i)
    yc_1979_var <- paste0("yc_", i, "_1979")


    formula <- as.formula(paste(uc_var, "~", yc_var, "+", yc_1979_var))


    model_1979 <- glm(formula, data = data_okun)

    # Extrakce koeficientů a p-hodnot
    coefficients <- summary(model_1979)$coefficients
    coefs_pvals <- data.frame
    (
        Variable = rownames(coefficients),
        Coefficient = coefficients[, "Estimate"],
        PValue = coefficients[, "Pr(>|t|)"]
    )

    coefs_pvals$Model <- paste0("Model_", i)


    results_list_1979[[i]] <- coefs_pvals
}

# Kombinace výsledků do jedné tabulky
results_table_1979 <- bind_rows(results_list_1979)


#  Odhad modelu pro rok 2008
results_list_2008 <- list()

for (i in 1:8) 
{
    # Dynamické generování názvů proměnných
    uc_var <- paste0("uc_", i)
    yc_var <- paste0("yc_", i)
    yc_2008_var <- paste0("yc_", i, "_2008")


    formula <- as.formula(paste(uc_var, "~", yc_var, "+", yc_2008_var))


    model_2008 <- glm(formula, data = data_okun)

    # Extrakce koeficientů a p-hodnot
    coefficients <- summary(model_2008)$coefficients
    coefs_pvals <- data.frame
    (
        Variable = rownames(coefficients),
        Coefficient = coefficients[, "Estimate"],
        PValue = coefficients[, "Pr(>|t|)"]
    )

    coefs_pvals$Model <- paste0("Model_", i)


    results_list_2008[[i]] <- coefs_pvals
}
results_table_2008 <- bind_rows(results_list_2008)
