################################################################################
############ EKONOMETRIE CASOVYCH RAD V SYSTEMU R - UKOL Č. 2 ##################
################################################################################
# Promazani promennych a konzole
rm(list = ls())
cat("\014")
# Stazeni potrebnych balicku
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(zoo)
library(forecast)
library(car)
library(gridExtra)

################################################################################

# Načtení dat z CSV souboru, přeskočení prvního řádku
XR_data <- read_csv("XR_USD_EUR.csv",
                    col_types = cols(DATE = col_date(format = "%Y-%m-%d"),
                                     DEXUSEU = col_number()),
                    skip = 1,
                    col_names = c("date", "XR"))

############################### ULOHA Č. 1 #####################################

# Vykresleni puvodnich casovych rad
ggplot()+
    geom_line(data = XR_data, aes(x = date, y = XR), color ="#4BACC6")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0,5, hjust = 1))+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")

# Kontrola struktury dat
str(XR_data)
head(XR_data)

# Převod datového rámce na časovou řadu
XR_ts <- ts(XR_data$XR, start = c(1999, 1), end = c(2024, 4), frequency = 12)

# Vypočítání logaritmu časové řady
log_XR_ts <- log(XR_ts)

# Vypočítání diferenciace logaritmu časové řady
diff_log_XR_ts <- diff(log_XR_ts)

# Přidání NA na začátek pro zarovnání délky časové řady
diff_log_XR_ts <- c(NA, diff_log_XR_ts)

# Vytvoření datového rámce s původní časovou řadou a novým sloupcem
XR_combined <- data.frame(
    date = seq(as.Date("1999-01-01"), by = "month", length.out = length(XR_ts)),
    XR = as.numeric(XR_ts),
    XR_stac = as.numeric(diff_log_XR_ts)
)

# Vykresleni novych casovych rad
ggplot()+
    geom_line(data = XR_combined, aes(x = date, y = XR_stac), color ="#4BACC6")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0,5, hjust = 1))+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")

############################### ULOHA Č. 2 #####################################

# Převod sloupce XR_stac na časovou řadu
XR_stac_ts <- ts(XR_combined$XR_stac, start = c(1999, 1), frequency = 12)

# Výběr specifického intervalu (odeberu 50 poslednich pozorovani)
XR_stac_rest <- window(XR_stac_ts, start = c(1999, 1), end = c(2019, 10))


# Vytvoření ARMA (p,q) modelů
arma_00 <- arima(XR_stac_rest, order = c(0, 0, 0))
arma_01 <- arima(XR_stac_rest, order = c(0, 0, 1))
arma_10 <- arima(XR_stac_rest, order = c(1, 0, 0))
arma_02 <- arima(XR_stac_rest, order = c(0, 0, 2))
arma_20 <- arima(XR_stac_rest, order = c(2, 0, 0))
arma_12 <- arima(XR_stac_rest, order = c(1, 0, 2))
arma_21 <- arima(XR_stac_rest, order = c(2, 0, 1))
arma_22 <- arima(XR_stac_rest, order = c(2, 0, 2))

# Vytvoření seznamu modelů
arma_models <- list(
    arma_OO = arma_00,
    arma_01 = arma_01,
    arma_10 = arma_10,
    arma_02 = arma_02,
    arma_20 = arma_20,
    arma_12 = arma_12,
    arma_21 = arma_21,
    arma_22 = arma_22
)

# Výpis modelů
print(arma_models)


############################### ULOHA Č. 3a ####################################
########################## rozšiřující se okénko ###############################

h = 4 # max. horizont predikce
zacatek <- c(2019,11) # začátek rekurze
konec <- c(2023,12)   # konec rekuze

H <- length(window(XR_stac_ts,start=zacatek,end=konec))

prvni <- length(window(XR_stac_ts, start=c(1999,1),end=c(2000,1)))
posledni <- length(window(XR_stac_ts, star=c(199,1), end = zacatek))

df <- data.frame(P = numeric(),
                 Q = numeric(),
                 F_rek = numeric(),
                 E_rek = numeric(),
                 MSPE = numeric(),
                 RMSPE = numeric(),
                 MAPE = numeric())

q_max <- 2
p_max <- 2

for (pp in 0:p_max) 
  {
    for (qq in 0:q_max) 
      {
        # vytvorime matice nul pro predikce a chyby predikce a MSPE apod.
        pom_F <- matrix(0, nrow = H, ncol = h)
        pom_E <- matrix(0, nrow = H, ncol = h)
        pom_MSPE <- matrix(0, nrow = 1, ncol = h)
        pom_RSPE <- matrix(0, nrow = 1, ncol = h)
        pom_MAPE <- matrix(0, nrow = 1, ncol = h)
    
        for (T in 1:H) 
          {
            # Vytvoreni rady pro odhad
            pom_XR <- XR_stac_ts[prvni:(posledni+T-1)]
            # Odhad ARIMA(p,q) modelu
            model <- arima(pom_XR, order = c(pp, 0, qq))
            # Predikce pomocou ARIMA modelu na h kroku dopredu
            pom_pred <- predict(model, n.ahead = h)$pred
            # Skutecne hodnoty
            pom_XR_skut <- XR_stac_ts[(posledni+T-1+1):(posledni+T-1+h)]
            # Ulozeni vysledku
            pom_F[T,] <- pom_pred
            pom_E[T,] <- pom_XR_skut-pom_pred
          }
        # Kvalita predikce
        pom_MSPE = colMeans(pom_E^2,na.rm = TRUE)
        pom_RMSPE = sqrt(pom_MSPE)
        pom_MAPE = colMeans(abs(pom_E),na.rm = TRUE)
    
        pom_list_F <- list(1)
        pom_list_E <- list(1)
        pom_list_F[[1]] <- pom_F
        pom_list_E[[1]] <- pom_E
        
        pom_list_MSPE <- list(1)
        pom_list_RMSPE <- list(1)
        pom_list_MAPE <- list(1)
        pom_list_MSPE[[1]] <- pom_MSPE
        pom_list_RMSPE[[1]] <- pom_RMSPE
        pom_list_MAPE[[1]] <- pom_MAPE
        
        # pridame novy radek
        df[nrow(df)+1,] <- list(pp,qq,pom_list_F,pom_list_E,pom_list_MSPE,pom_list_RMSPE,pom_list_MAPE)
     }
  }

# hodnocení kvality modelů - nejmenší MSPE
pom <- matrix(nrow = 0, ncol = h) # převod vektoru na matice
for (ii in 1:nrow(df))
  {
    pom <- rbind(pom,df$MSPE[[ii]])
  }
pom_ind <- apply(pom, 2, which.min)
for (ii in 1:h)
  {
    writeLines(sprintf("Nejlepsi model pro horizont predikce (dle MSPE): %.0f, ARMA(%.0f,%.0f)\n",
                     ii, df$P[pom_ind[ii]], df$Q[pom_ind[ii]]))
  }

# hodnocení kvality modelů - nejlepší MAPE
pom <- matrix(nrow = 0, ncol = h) # převod vektoru na matice
for (ii in 1:nrow(df))
  {
    pom <- rbind(pom,df$MAPE[[ii]])
  }
pom_ind <- apply(pom, 2, which.min)
for (ii in 1:h)
  {
    writeLines(sprintf("Nejlepsi model pro horizont predikce (dle MAPE): %.0f, ARMA(%.0f,%.0f)\n",
                     ii, df$P[pom_ind[ii]], df$Q[pom_ind[ii]]))
}

df_roz <- df # ulozeni vysledku pro rozsirujici se okenko

# HODNOCENÍ KVALITY PREDIKCÍ

# Nestrannosti predikce

# skutecne hodnoty v horizontu predikce
y_pom <-window(XR_stac_ts, start=c(1999,1),end=c(2019,10))
# nactení nejlepich predikcnich modelu
pom1 <- df_roz %>% filter(P == 2, Q == 0)
f_pom1a <- pom1$F_rek[[1]][,2]

pom2 <- df_roz %>% filter(P == 0, Q == 1)
f_pom2 <- pom2$F_rek[[1]][,2]

pom3 <- df_roz %>% filter(P == 0, Q == 0)
f_pom3 <- pom3$F_rek[[1]][,2]

pom4 <- df_roz %>% filter(P == 2, Q == 2)
f_pom4 <- pom4$F_rek[[1]][,2]

# Vytvorení LRM a F-test
linear_model1 <- lm(y_pom~f_pom1a, data.frame(y_pom, f_pom1a))
car::linearHypothesis(linear_model1, c("(Intercept) = 0", "f_pom1a = 1"))

linear_model2 <- lm(y_pom~f_pom2, data.frame(y_pom, f_pom2))
car::linearHypothesis(linear_model2, c("(Intercept) = 0", "f_pom2 = 1"))

linear_model3 <- lm(y_pom~f_pom3, data.frame(y_pom, f_pom3))
car::linearHypothesis(linear_model3, c("(Intercept) = 0", "f_pom3 = 1"))

linear_model4 <- lm(y_pom~f_pom4, data.frame(y_pom, f_pom4))
car::linearHypothesis(linear_model4, c("(Intercept) = 0", "f_pom4 = 1"))

# Diebold-Marianuv test

# vytvoreni chyb predikce
pom <- df_roz %>% filter(P == 2, Q == 0)
e1 <- pom$E_rek[[1]][,2]
pom <- df_roz %>% filter(P == 0, Q == 0)
e2 <- pom$E_rek[[1]][,2]

#kvadratická ztratova fce
d <- e1^2-e2^2

# korelogram rady rozdilu ztratovych funkci
d_ts <- ts(d, start = c(1999, 1), frequency = 12)
p1 <- autoplot(d_ts, size=1, colour = "#4BACC6",facets = FALSE)
p2 <- ggAcf(d_ts, lag.max = 20) #ACF - az 20 zpozdeni
p3 <- ggPacf(d_ts, lag.max = 20) #PACF - az 20 zpozdeni
grid.arrange(p1, p2, p3, nrow=3, ncol=1)

stredni_hodnota <- mean(d, na.rm = TRUE)
print(stredni_hodnota)

DM_mod <- lm(d ~ 1)
HAC_vcov <- NeweyWest(DM_mod)
HAC_se <- sqrt(HAC_vcov)
DM <- coef(DM_mod)[1]/HAC_se
p_DM <- 2*pt(abs(DM),df.residual(DM_mod),lower.tail = FALSE)
writeLines(sprintf("\nDiebold-Mariano test - ARMA(2,0) vs ARMA (0,0) (kvadraticka ztratova funkce)\n"))
writeLines(sprintf("DM statistika: %.4f, p-hodnota: %.4f\n", DM, p_DM))

# ztratova funkce jako absolutni hodnota
d <- abs(e1)-abs(e2)

# korelogram rady rozdilu ztratovych funkci
d_ts <- ts(d, start = c(1999, 1), frequency = 12)
p1 <- autoplot(d_ts, size=1, colour = "navy",facets = FALSE)
p2 <- ggAcf(d_ts, lag.max = 20) #ACF - az 20 zpozdeni
p3 <- ggPacf(d_ts, lag.max = 20) #PACF - az 20 zpozdeni
grid.arrange(p1, p2, p3, nrow=3, ncol=1)

stredni_hodnota <- mean(d, na.rm = TRUE)
print(stredni_hodnota)

DM_mod <- lm( d ~ 1)
HAC_vcov <- NeweyWest(DM_mod)
HAC_se <- sqrt(HAC_vcov)
DM <- coef(DM_mod)[1]/HAC_se
p_DM <- 2*pt(abs(DM),df.residual(DM_mod),lower.tail = FALSE)
writeLines(sprintf("\nDiebold-Mariano test - ARMA(2,0) vs ARMA (0,0) (ztratova fce jako absolutní hodnota)\n"))
writeLines(sprintf("DM statistika: %.4f, p-hodnota: %.4f\n", DM, p_DM))


############################### ULOHA Č. 3b ####################################
############################# posuvné okénko ###################################

h = 4 # max. horizont predikce
zacatek <- c(2019,11) # začátek rekurze
konec <- c(2023,12)   # konec rekuze

H <- length(window(XR_stac_ts,start=zacatek,end=konec))

prvni <- length(window(XR_stac_ts, start=c(1999,1),end=c(2000,1)))
posledni <- length(window(XR_stac_ts, star=c(199,1), end = zacatek))

df <- data.frame(P = numeric(),
                 Q = numeric(),
                 F_rek = numeric(),
                 E_rek = numeric(),
                 MSPE = numeric(),
                 RMSPE = numeric(),
                 MAPE = numeric())
q_max <- 2
p_max <- 2

for (pp in 0:p_max) 
{
  for (qq in 0:q_max) 
  {
    # vytvorime matice nul pro predikce a chyby predikce a MSPE apod.
    pom_F <- matrix(NA, nrow = H, ncol = h)
    pom_E <- matrix(NA, nrow = H, ncol = h)
    pom_MSPE <- matrix(0, nrow = 1, ncol = h)
    pom_RSPE <- matrix(0, nrow = 1, ncol = h)
    pom_MAPE <- matrix(0, nrow = 1, ncol = h)
    
    for (T in 1:H) 
      {
        # moznost posunu prvniho pozorovani v ramci rekurze
        pom_XR <- XR_stac_ts[(prvni+T-1):(posledni+T-1)] # vytvoreni rady pro odhad
        tryCatch(
          {
            model <- arima(pom_XR, order = c(pp, 0, qq))
            # Predikce pomocou ARIMA modelu na h kroku dopredu
            pom_pred <- predict(model, n.ahead = h)$pred
            #Skutecne hodnoty
            pom_XR_skut <- XR_stac_ts[(posledni+T-1+1):(posledni+T-1+h)]
            # ulozeni vysledku
            pom_F[T,] <- pom_pred
            pom_E[T,] <- pom_XR_skut-pom_pred
          }
        , error = function(e) NA)
      }
    check <- sum(is.na(pom_F))
    if(check == 0)
      {
        pom_MSPE = colMeans(pom_E^2,na.rm = TRUE)
        pom_RMSPE = sqrt(pom_MSPE)
        pom_MAPE = colMeans(abs(pom_E),na.rm = TRUE)
    
        pom_list_F <- list(1)
        pom_list_E <- list(1)
        pom_list_F[[1]] <- pom_F
        pom_list_E[[1]] <- pom_E
      
        pom_list_MSPE <- list(1)
        pom_list_RMSPE <- list(1)
        pom_list_MAPE <- list(1)
        pom_list_MSPE[[1]] <- pom_MSPE
        pom_list_RMSPE[[1]] <- pom_RMSPE
        pom_list_MAPE[[1]] <- pom_MAPE
      
        # pridame novy radek
        
        df[nrow(df)+1,] <- list(pp,qq,pom_list_F,pom_list_E,pom_list_MSPE,pom_list_RMSPE,pom_list_MAPE)
      }
  }
}   
    
# hodnocení kvality modelů - nejmenší MSPE
pom <- matrix(nrow = 0, ncol = h) # převod vektoru na matice
for (ii in 1:nrow(df))
  {
    pom <- rbind(pom,df$MSPE[[ii]])
  }
pom_ind <- apply(pom, 2, which.min)
for (ii in 1:h)
  {
    writeLines(sprintf("Nejlepsi model pro horizont predikce (dle MSPE): %.0f, ARMA(%.0f,%.0f)\n",
                     ii, df$P[pom_ind[ii]], df$Q[pom_ind[ii]]))
  }

# hodnocení kvality modelů - nejlepší MAPE
pom <- matrix(nrow = 0, ncol = h) # převod vektoru na matice
for (ii in 1:nrow(df))
  {
    pom <- rbind(pom,df$MAPE[[ii]])
  }
pom_ind <- apply(pom, 2, which.min)
for (ii in 1:h)
  {
    writeLines(sprintf("Nejlepsi model pro horizont predikce (dle MAPE): %.0f, ARMA(%.0f,%.0f)\n",
                     ii, df$P[pom_ind[ii]], df$Q[pom_ind[ii]]))
  }
df_pos <- df # ulozeni vysledku pro posuvné okenko    

# HODNOCENÍ KVALITY PREDIKCÍ
# Nestrannosti predikce

# skutecne hodnoty v horizontu predikce
y_pom <-window(XR_stac_ts, start=c(1999,1),end=c(2019,10))
# nactení nejlepich predikcnich modelu - rozsirujíci se okenko
pom1 <- df_pos %>% filter(P == 1, Q == 2)
f_pom1 <- pom1$F_rek[[1]][,2]

pom2 <- df_pos %>% filter(P == 1, Q == 2)
f_pom2 <- pom2$F_rek[[1]][,2]

pom3 <- df_pos %>% filter(P == 1, Q == 2)
f_pom3 <- pom3$F_rek[[1]][,2]

pom4 <- df_pos %>% filter(P == 2, Q == 2)
f_pom4 <- pom4$F_rek[[1]][,2]
    
# Vytvorení LRM a F-test
linear_model1 <- lm(y_pom~f_pom1, data.frame(y_pom, f_pom1))
car::linearHypothesis(linear_model1, c("(Intercept) = 0", "f_pom1 = 1"))

linear_model2 <- lm(y_pom~f_pom2, data.frame(y_pom, f_pom2))
car::linearHypothesis(linear_model2, c("(Intercept) = 0", "f_pom2 = 1"))

linear_model3 <- lm(y_pom~f_pom3, data.frame(y_pom, f_pom3))
car::linearHypothesis(linear_model3, c("(Intercept) = 0", "f_pom3 = 1"))

linear_model4 <- lm(y_pom~f_pom4, data.frame(y_pom, f_pom4))
car::linearHypothesis(linear_model4, c("(Intercept) = 0", "f_pom4 = 1"))

# Diebold-Marianuv test

# vytvoreni chyb predikce
pom <- df_roz %>% filter(P == 1, Q == 2)
e1 <- pom$E_rek[[1]][,2]
pom <- df_roz %>% filter(P == 2, Q == 2)
e2 <- pom$E_rek[[1]][,2]

#kvadratická ztratova fce
d <- e1^2-e2^2

stredni_hodnota <- mean(d, na.rm = TRUE)
print(stredni_hodnota)

DM_mod <- lm(d ~ 1)
HAC_vcov <- NeweyWest(DM_mod)
HAC_se <- sqrt(HAC_vcov)
DM <- coef(DM_mod)[1]/HAC_se
p_DM <- 2*pt(abs(DM),df.residual(DM_mod),lower.tail = FALSE)
writeLines(sprintf("\nDiebold-Mariano test - ARMA(2,0) vs ARMA (0,0) (kvadraticka ztratova funkce)\n"))
writeLines(sprintf("DM statistika: %.4f, p-hodnota: %.4f\n", DM, p_DM))

# ztratova funkce jako absolutni hodnota
d <- abs(e1)-abs(e2)

stredni_hodnota <- mean(d, na.rm = TRUE)
print(stredni_hodnota)

DM_mod <- lm( d ~ 1)
HAC_vcov <- NeweyWest(DM_mod)
HAC_se <- sqrt(HAC_vcov)
DM <- coef(DM_mod)[1]/HAC_se
p_DM <- 2*pt(abs(DM),df.residual(DM_mod),lower.tail = FALSE)
writeLines(sprintf("\nDiebold-Mariano test - ARMA(2,0) vs ARMA (0,0) (ztratova fce jako absolutní hodnota)\n"))
writeLines(sprintf("DM statistika: %.4f, p-hodnota: %.4f\n", DM, p_DM))


################################ ÚLOHA Č.4 #####################################

# Vytvoreni predikovanych a skutecnych hodnot
f_pom1_ts <- ts(f_pom1,start = c(1999,1),end = c(2024,4), frequency = 12)
f_pom1a_ts <- ts(f_pom1a,start = c(1999,1), end = c(2024,4), frequency = 12)
XR_stac_rest <- window(XR_stac_ts,start = c(1999,1), end = c(2024,4), frequency = 12)

my_plot <- ts.union(XR_stac_rest, f_pom1_ts, f_pom1a_ts)
autoplot(my_plot, size=1, colour = TRUE,facets = FALSE)
ts.plot(XR_stac_rest)
lines(f_pom1_ts, col = "blue")
lines(f_pom1a_ts, col = "green")


################################ ÚLOHA Č.5 #####################################

NAIVE_F <- matrix(NA, nrow = H, ncol = h)
NAIVE_y <- matrix(NA, nrow = H, ncol = h)
for (T in 1:h) 
{
  NAIVE_F[,T] <- XR_stac_ts[(posledni):(posledni+H-1)]
  pom <- XR_stac_ts[(posledni+T):(posledni+H-1+T)]
  NAIVE_y[1:length(pom),T] <- pom
}

#chyby naivni predikce
NAIVE_E <- NAIVE_y-NAIVE_F
NAIVE_MSPE = colMeans(NAIVE_E^2,na.rm = TRUE)
NAIVE_RMSPE = sqrt(NAIVE_MSPE)
NAIVE_MAPE = colMeans(abs(NAIVE_E),na.rm = TRUE)

# HODNOCENÍ KVALITY PREDIKCÍ

# Diebold-Marianuv test

# vytvoreni chyb predikce
pom <- df_roz %>% filter(P == 1, Q == 2)
e1 <- pom$E_rek[[1]][,2]
e2 <- NAIVE_E[,2]

#kvadratická ztratova fce
d <- e1^2-e2^2

stredni_hodnota <- mean(d, na.rm = TRUE)
print(stredni_hodnota)

DM_mod <- lm(d ~ 1)
HAC_vcov <- NeweyWest(DM_mod)
HAC_se <- sqrt(HAC_vcov)
DM <- coef(DM_mod)[1]/HAC_se
p_DM <- 2*pt(abs(DM),df.residual(DM_mod),lower.tail = FALSE)
writeLines(sprintf("\nDiebold-Mariano test - ARMA(1,2) vs naivní (kvadraticka ztratova funkce)\n"))
writeLines(sprintf("DM statistika: %.4f, p-hodnota: %.4f\n", DM, p_DM))

# ztratova funkce jako absolutni hodnota
d <- abs(e1)-abs(e2)

stredni_hodnota <- mean(d, na.rm = TRUE)
print(stredni_hodnota)

DM_mod <- lm( d ~ 1)
HAC_vcov <- NeweyWest(DM_mod)
HAC_se <- sqrt(HAC_vcov)
DM <- coef(DM_mod)[1]/HAC_se
p_DM <- 2*pt(abs(DM),df.residual(DM_mod),lower.tail = FALSE)
writeLines(sprintf("\nDiebold-Mariano test - ARMA(1,2) vs naivni (ztratova fce jako absolutní hodnota)\n"))
writeLines(sprintf("DM statistika: %.4f, p-hodnota: %.4f\n", DM, p_DM))


################################# ÚLOHA Č.6 ####################################
df <- data.frame
(
  time = as.numeric(time(f_pom1_ts))
  f_pom1 = as.numeric(f_pom1_ts)
  f_pom1a = as.numeric(f_pom1a_ts)
  f_naive = as.numeric(f_naive_ts)
)

p1 <- ggplot(df, aes(x = time)) +
  geom_line(aes(y = f_pom1)) +
  labs(title = "ARMA (2,2)", x = "Time", y = "Value") +
  theme_minimal()
p2 <- ggplot(df, aes(x = time)) +
  geom_line(aes(y = f_pom1a)) +
  labs(title = "ARMA (1,2)", x = "Time", y = "Value") +
  theme_minimal()
p3 <- ggplot(df, aes(x = time)) +
  geom_line(aes(y = f_naive)) +
  labs(title = "Naivni", x = "Time", y = "Value") +
  theme_minimal()

grid.arrange(p1, p2, p3, nrow = 3)
