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
library(mFilter)
library(ggcorrplot)
library(reshape2)
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
cusum_test_Y <- efp(Y$value_stac ~ 1, type = "Rec-CUSUM")
plot(cusum_test_Y)
cusum_test_U <- efp(U$value_stac ~ 1, type = "Rec-CUSUM")
plot(cusum_test_U)


bp_Y <- breakpoints(Y$value_stac ~ 1) #cusum test na Y neidentifikuje zlom
bp_U <- breakpoints(U$value_stac ~ 1)

# Zivot-Andrews test jednotkoveho korene - hledá potenciální strukturální zlomy
za_U <- ur.za(U$value_stac, model = c("intercept", "trend", "both"), lag=NULL)
summary(za_U)

za_Y <- ur.za(Y$value_stac, model = "trend", lag=NULL)
summary(za_Y)

date_breakpoint <- Y$date[as.integer(za_Y@bpoint)]


# HDP se zlomem
p5 <- ggplot(Y, aes(x = date, y = value_stac)) +
    geom_line(color = "#4BACC6") +
    geom_vline(xintercept = Y$date[za_Y@bpoint],
               color = "red", linetype = "dashed") +
    theme_bw() +
    labs(x = "Datum", y = "HDP") +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
    ggtitle("Štrukturálny zlom v stacionárnej rade HDP")

p5

ggsave(filename = "grafy/y_struc_break.png", plot = p5, width = 8, height = 4, dpi = 300)

# Nezamestnanost se zlomem
p6 <- ggplot(U, aes(x = date, y = value_stac)) +
    geom_line(color = "#17a589") +
    geom_vline(xintercept = U$date[za_U@bpoint],
               color = "red", linetype = "dashed") +
    theme_bw() +
    labs(x = "Datum", y = "Nezamestnanosť") +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
    ggtitle("Štrukturálny zlom v stacionárnej rade nezamestnanosti")

p6

ggsave(filename = "grafy/u_struc_break.png", plot = p6, width = 8, height = 4, dpi = 300)


############################### ULOHA C. 2 #####################################
# Použití HP filtru

yc_hp <- hpfilter(Y$value,freq = 1600,type = "lambda",drift = FALSE)$cycle %>% ts(start = c(1980,1),frequency = 4)
autoplot(yc_hp, size = 1, color = "orange")

uc_hp <- hpfilter(U$value, freq = 1600,type = "lambda", drift = FALSE)$cycle %>% ts(start = c(1980,1), frequency = 4)
autoplot(uc_hp, size = 1, color = "orange")

Y$log_value <- log(Y$value)

# Použití polynomiálního trendu se strukturálním zlomem pro výstup - strukturální zlom v roce 2008
tt <- ts(1:nrow(Y), start = c(1980,1),frequency = 4)

df_pol_D <- data.frame(tt)


pp <- 5 # maximalni stupen polynomu

y <- ts(Y$log_value, start = c(1980,1),frequency = 4)

pom_ts <- ts.union(y,tt)
my_formula <- "y ~ 1"

for (ii in 1:pp){
    pom_str <- paste(paste(c("+I(tt^",ii,")"), collapse = ""))
    my_formula <- paste(my_formula, pom_str, collapse = "")

    pom_mod_1 <- lm(my_formula,window(pom_ts,start = c(1980,1), end = c(2008,4)))
    pom_mod_2 <- lm(my_formula,window(pom_ts,start = c(2009,1), end = c(2019,1)))


    pom_res <- c(residuals(pom_mod_1),residuals(pom_mod_2))
    pom_trend <- c(fitted(pom_mod_1),fitted(pom_mod_2))

    pom_nam <- paste(c("yc_pol_D",ii),collapse = "")
    df_pol_D[pom_nam] <- pom_res

    pom_nam = paste(c("yt_pol_D",ii),collapse = "")
    df_pol_D[pom_nam] <- pom_trend

}

# vyber jen cyklickych slozek z data frame
yc_po_ts <- df_pol_D %>% select(contains("yc")) %>% ts(start = c(1980,1),frequency = 4)

p7 <- autoplot(yc_po_ts, size = 1)
p7 <- p7 + xlab("Obdobi") + ylab("Mezera vystupu") + ggtitle("Polynomialni trendy - zlom")
print(p7)

ggsave(filename = "grafy/polynomialni_trendy_y.png", plot = p7, width = 8, height = 6, dpi = 300)

# polynomický trend se strukturálním zlomem pro nezaměstnanost -  strukturální zlom v roce 2008
u <- ts(U$value, start = c(1980,1),frequency = 4)

pom_ts <- ts.union(u,tt)

my_formula <- "u ~ 1"

for (ii in 1:pp){
    pom_str <- paste(paste(c("+I(tt^",ii,")"), collapse = ""))
    my_formula <- paste(my_formula, pom_str, collapse = "")

    pom_mod_1 <- lm(my_formula,window(pom_ts,start = c(1980,1), end = c(2008,4)))
    pom_mod_2 <- lm(my_formula,window(pom_ts,start = c(2009,1), end = c(2019,1)))


    pom_res <- c(residuals(pom_mod_1),residuals(pom_mod_2))
    pom_trend <- c(fitted(pom_mod_1),fitted(pom_mod_2))

    pom_nam <- paste(c("uc_pol_D",ii),collapse = "")
    df_pol_D[pom_nam] <- pom_res

    pom_nam = paste(c("ut_pol_D",ii),collapse = "")
    df_pol_D[pom_nam] <- pom_trend

}

uc_po_ts <- df_pol_D %>% select(contains("uc")) %>% ts(start = c(1980,1),frequency = 4)

p8 <- autoplot(uc_po_ts, size = 1)
p8 <- p8 + xlab("Obdobi") + ylab("Nezaměstnanost") + ggtitle("Polynomialni trendy - zlom")
print(p8)

ggsave(filename = "grafy/polynomialni_trendy_u.png", plot = p8, width = 8, height = 6, dpi = 300)



# Jednoduchý klouzavý průměr
# definice vah pro odhadove okno
m <- 8 # delka klouzaveho prumeru => sirka okenka 2m+1 = 17
w <- rep(1/(2*m+1),2*m+1)

yc_sma_ts <- y - stats::filter(y, w, sides = 2)
p9 <- autoplot(yc_sma_ts, size = 1, color = "green")
p9 <- p9 + xlab("Obdobi") + ylab("Mezera vystupu") + ggtitle("Jednoduchý klouzavý průměr")
print(p9)

ggsave(filename = "grafy/jednoduchy_klouzavy_prumer_y.png", plot = p9, width = 8, height = 6, dpi = 300)


uc_sma_ts <- u - stats::filter(u, w, sides = 2)
p10 <- autoplot(uc_sma_ts, size = 1, color = "green")
p10 <- p10 + xlab("Obdobi") + ylab("Nezaměstnanost") + ggtitle("Jednoduchý klouzavý průměr")
print(p10)

ggsave(filename = "grafy/jednoduchy_klouzavy_prumer_u.png", plot = p10, width = 8, height = 6, dpi = 300)

# Vážený klouzavý průměr s použitím balíčku stats
XX <- cbind(matrix(1,nrow = 2*m + 1,ncol = 1),
            matrix(-m:m,nrow = 2*m + 1,ncol = 1),
            matrix((-m:m)^2,nrow = 2*m + 1,ncol = 1))
# OLS estimator (X'X)^(-1)*X'*y ... vezmeme první řádek (X'X)^(-1)*X'
# odpovidajici odhadu urovnove konstante (vazeny prumer y)

pom <- solve(t(XX) %*% XX) %*% t(XX)
w <- pom[1,]
yc_wma_ts <- y - stats::filter(y, w, sides = 2)

p11 <- autoplot(yc_wma_ts, size = 1, color = "darkblue")
p11 <- p11 + xlab("Obdobi") + ylab("Mezera výstupu") + ggtitle("Vážený klouzavý průměr")
print(p11)

ggsave(filename = "grafy/vazeny_klouzavy_prumer_y.png", plot = p11, width = 8, height = 6, dpi = 300)

uc_wma_ts <- u - stats::filter(u, w, sides = 2)

p12 <- autoplot(yc_wma_ts, size = 1, color = "darkblue")
p12 <- p11 + xlab("Obdobi") + ylab("Nezaměstanost") + ggtitle("Vážený klouzavý průměr")
print(p12)

ggsave(filename = "grafy/vazeny_klouzavy_prumer_u.png", plot = p12, width = 8, height = 6, dpi = 300)

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
                          colors = c("#6D9EC1", "white", "#E46726"))


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
                          colors = c("#6D9EC1", "white", "#E46726"))



corr <- wrap_plots(yc_corr_fig, uc_corr_fig, ncol = 2, nrow = 1)
corr
ggsave(filename = "grafy/corr.png", plot = corr, width = 10, height = 5)

#Podíl období, kdy estimátory měly stejné znaménko
# Funkce pro výpočet podílu shodného znaménka
sign_match_ratio <- function(var1, var2) {
    match_count <- sum(sign(var1) == sign(var2))
    total_count <- length(var1)
    ratio <- match_count / total_count
    return(ratio)
}


sign_match_matrix_yc <- matrix(0, nrow = 8, ncol = 8)

pom_yc_ts_map <- pom_yc_ts[9:148,]

for (i in 1:8) {
    for (j in 1:8) {
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
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.75, limit = c(0.5, 1), space = "Lab", name="Ratio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = "Ratio of observations with the same sign",
         x = "Variables",
         y = "Variables")

yc_ratio

#Podíl období, kdy estimátory měly stejné znaménko
# Funkce pro výpočet podílu shodného znaménka

sign_match_matrix_uc <- matrix(0, nrow = 8, ncol = 8)

pom_uc_ts_map <- pom_uc_ts[9:148,]

for (i in 1:8) {
    for (j in 1:8) {
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
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.75, limit = c(0.5, 1), space = "Lab", name="Ratio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = "Ratio of observations with the same sign",
         x = "Variables",
         y = "Variables")


ratio <- wrap_plots(yc_ratio, uc_ratio, ncol = 2, nrow = 1)
ggsave(filename = "grafy/ratio.png", plot = ratio, width = 10, height = 5)


#######################################uloha 3########################################
library(tidyverse)

# Funkce pro hvězdičky
get_significance_stars <- function(pval) {
    ifelse(pval < 0.001, "***",
           ifelse(pval < 0.01, "**",
                  ifelse(pval < 0.05, "*",
                         ifelse(pval < 0.1, ".", ""))))
}

# --- Příprava dat ---
uc <- as.tibble(pom_uc_ts)
yc <- as.tibble(pom_yc_ts)

names(yc) <- paste0("yc_", 1:8)
names(uc) <- paste0("uc_", 1:8)
data_okun <- bind_cols(uc, yc)

data_okun <- data_okun %>%
    mutate(date = seq.Date(from = as.Date("1961-01-01"), by = "quarter", length.out = nrow(data_okun)),
           break_2008 = ifelse(date >= as.Date("2008-10-01"), 1, 0))

original_vars <- names(data_okun)[1:16]

for (var in original_vars) {
    data_okun <- data_okun %>%
        mutate(!!paste0(var, "_2008") := .data[[var]] * break_2008)
}

# --- Základní modely ---
results_list_base <- list()

for (i in 1:8) {
    uc_var <- paste0("uc_", i)
    yc_var <- paste0("yc_", i)
    formula <- as.formula(paste(uc_var, "~", yc_var))
    model_base <- glm(formula, data = data_okun)

    coefficients <- summary(model_base)$coefficients
    coefs_pvals <- data.frame(
        Variable = rownames(coefficients),
        Coefficient = coefficients[, "Estimate"],
        PValue = coefficients[, "Pr(>|t|)"]
    )

    coefs_pvals$Model <- paste0("Model_", i)
    coefs_pvals$Dependent <- uc_var
    coefs_pvals$Signif <- get_significance_stars(coefs_pvals$PValue)

    results_list_base[[i]] <- coefs_pvals
}

results_table_base <- bind_rows(results_list_base)

# --- Modely se zlomem v roce 2008 ---
results_list_2008 <- list()

for (i in 1:8) {
    uc_var <- paste0("uc_", i)
    yc_var <- paste0("yc_", i)
    yc_2008_var <- paste0("yc_", i, "_2008")
    formula <- as.formula(paste(uc_var, "~", yc_var, "+", yc_2008_var))
    model_2008 <- glm(formula, data = data_okun)

    coefficients <- summary(model_2008)$coefficients
    coefs_pvals <- data.frame(
        Variable = rownames(coefficients),
        Coefficient = coefficients[, "Estimate"],
        PValue = coefficients[, "Pr(>|t|)"]
    )

    coefs_pvals$Model <- paste0("Model_", i)
    coefs_pvals$Dependent <- uc_var
    coefs_pvals$Signif <- get_significance_stars(coefs_pvals$PValue)

    results_list_2008[[i]] <- coefs_pvals
}

results_table_2008 <- bind_rows(results_list_2008)

# --- Výstup do txt: základní model ---
output_file_base <- "okun_base_model.txt"
file_conn_base <- file(output_file_base, open = "wt")

for (i in 1:8) {
    cat(paste0("Model_", i, " (Dependent: uc_", i, ")\n"), file = file_conn_base, append = TRUE)
    capture.output(print(results_list_base[[i]]), file = file_conn_base, append = TRUE)
    cat("\n\n", file = file_conn_base, append = TRUE)
}

close(file_conn_base)

# --- Výstup do txt: model se zlomem ---
output_file_2008 <- "okun_model_2008.txt"
file_conn_2008 <- file(output_file_2008, open = "wt")

for (i in 1:8) {
    cat(paste0("Model_", i, " (Dependent: uc_", i, ")\n"), file = file_conn_2008, append = TRUE)
    capture.output(print(results_list_2008[[i]]), file = file_conn_2008, append = TRUE)
    cat("\n\n", file = file_conn_2008, append = TRUE)
}

close(file_conn_2008)

