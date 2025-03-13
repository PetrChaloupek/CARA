################################################################################
############ EKONOMETRIE CASOVYCH RAD V SYSTEMU R - UKOL Č. 1 ##################
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

################################################################################

# Import dat
CPI <- read_csv("CPI.csv",
                col_types = cols(DATE = col_date(format = "%Y-%m-%d"),
                CPALCY01CAM661N = col_number()),
                col_names = c("date", "CPI"),
                skip = 1)
IR_3M <- read_csv("IR_3M.csv",
                col_types = cols(DATE = col_date(format = "%Y-%m-%d"),
                IR3TIB01CAM156N = col_number()),
                col_names = c("date", "IR_3M"),
                skip = 1)

# Spojení dat to dojednoho souboru
Table <- inner_join(CPI, IR_3M, by = "date")

############################### ULOHA Č. 1 #####################################

# Vykresleni casovych rad
ggplot()+
    geom_line(data = Table, aes(x = date, y = CPI), color ="#4BACC6")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0,5, hjust = 1))

ggplot()+
    geom_line(data = Table, aes(x = date, y = IR_3M), color ="#4BACC6")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0,5, hjust = 1))

# Úprava dat
Table$CPI_stac <- c(NA,diff(log(Table$CPI)))

Table$IR_3M_stac <- c(NA,diff(Table$IR_3M))

# Vykresleni novych casovych rad
ggplot()+
    geom_line(data = Table, aes(x = date, y = CPI_stac), color ="#4BACC6")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0,5, hjust = 1))

ggplot()+
    geom_line(data = Table, aes(x = date, y = IR_3M_stac), color ="#4BACC6")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0,5, hjust = 1))

############################### ULOHA Č. 2 #####################################

# Extrakce dat
cpi_stac_mod <- Table$CPI_stac
ir_3m_stac_mod <- Table$IR_3M
cpi_mod <- Table$CPI
ir_3m_mod <- Table$IR_3M

# provedeni ACF a PACF - pro transformovane casove rady
par(mfrow = c(2,1))
acf(cpi_stac_mod, lag.max = 24, na.action = na.pass)
pacf(cpi_stac_mod, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
acf(ir_3m_mod, lag.max = 24, na.action = na.pass)
pacf(ir_3m_mod, lag.max = 24, na.action = na.pass)

# namodelovani ARMA modelu pro index spotrebitelskych cen
armacpi_11 <-arima(cpi_stac_mod, order = c(1,0,1))
print(armacpi_11)

armacpi_21 <-arima(cpi_stac_mod, order = c(2,0,1))
print(armacpi_21)

armacpi_31 <-arima(cpi_stac_mod, order = c(3,0,1))
print(armacpi_31)

armacpi_12 <-arima(cpi_stac_mod, order = c(1,0,2))
print(armacpi_12)

armacpi_13 <-arima(cpi_stac_mod, order = c(1,0,3))
print(armacpi_13)

armacpi_22 <-arima(cpi_stac_mod, order = c(2,0,2))
print(armacpi_22)

# namodelovani ARMA modelu pro urokove miry
armair_11 <-arima(ir_3m_mod, order = c(1,0,1))
print(armacpi_11)

armair_21 <-arima(ir_3m_mod, order = c(2,0,1))
print(armacpi_21)

armair_31 <-arima(ir_3m_mod, order = c(3,0,1))
print(armacpi_31)

armair_12 <-arima(ir_3m_mod, order = c(1,0,2))
print(armacpi_12)

armair_13 <-arima(ir_3m_mod, order = c(1,0,3))
print(armacpi_13)

armair_22 <-arima(ir_3m_mod, order = c(2,0,2))
print(armacpi_22)

############################### ULOHA Č. 3 #####################################
# ulozeni a vykresleni rezidui pro modely CPI
par(mfrow = c(2,1))
e_cpi_11 <- residuals(armacpi_11)
acf(e_cpi_11, lag.max = 24, na.action = na.pass)
pacf(e_cpi_11, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_cpi_12 <- residuals(armacpi_12)
acf(e_cpi_12, lag.max = 24, na.action = na.pass)
pacf(e_cpi_12, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_cpi_13 <- residuals(armacpi_13)
acf(e_cpi_13, lag.max = 24, na.action = na.pass)
pacf(e_cpi_13, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_cpi_21 <- residuals(armacpi_21)
acf(e_cpi_21, lag.max = 24, na.action = na.pass)
pacf(e_cpi_21, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_cpi_22 <- residuals(armacpi_22)
acf(e_cpi_22, lag.max = 24, na.action = na.pass)
pacf(e_cpi_22, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_cpi_31 <- residuals(armacpi_31)
acf(e_cpi_31, lag.max = 24, na.action = na.pass)
pacf(e_cpi_31, lag.max = 24, na.action = na.pass)

# ulozeni a vykresleni rezidui pro modely urokovou miru
par(mfrow = c(2,1))
e_ir_11 <- residuals(armair_11)
acf(e_ir_11, lag.max = 24, na.action = na.pass)
pacf(e_ir_11, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_ir_12 <- residuals(armair_12)
acf(e_ir_12, lag.max = 24, na.action = na.pass)
pacf(e_ir_12, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_ir_13 <- residuals(armair_13)
acf(e_ir_13, lag.max = 24, na.action = na.pass)
pacf(e_ir_13, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_ir_21 <- residuals(armair_21)
acf(e_ir_21, lag.max = 24, na.action = na.pass)
pacf(e_ir_21, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_ir_22 <- residuals(armair_22)
acf(e_ir_22, lag.max = 24, na.action = na.pass)
pacf(e_ir_22, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_ir_31 <- residuals(armair_31)
acf(e_ir_31, lag.max = 24, na.action = na.pass)
pacf(e_ir_31, lag.max = 24, na.action = na.pass)

# Provedení Q-testu
# Q-test pro CPI modely
Box.test(e_cpi_11, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 2
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_11, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_cpi_12, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 3
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_12, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_cpi_13, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 4
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_13, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_cpi_21, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 3
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_21, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_cpi_22, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 4
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_22, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_cpi_31, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 4
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_31, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

# Q-testy pro urokove miry
Box.test(e_ir_11, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 2
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_ir_11, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_ir_12, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 3
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_ir_12, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_ir_13, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 4
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_ir_13, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_ir_21, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 3
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_ir_21, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_ir_22, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 4
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_ir_22, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_ir_31, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 4
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_ir_31, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

# Akaikeho identifikacni kriterium
AIC(armacpi_11)
AIC(armacpi_12)
AIC(armacpi_13)
AIC(armacpi_21)
AIC(armacpi_22)
AIC(armacpi_31)

AIC(armair_11)
AIC(armair_12)
AIC(armair_13)
AIC(armair_21)
AIC(armair_22)
AIC(armair_31)


################################################################################
############################### ULOHA Č. 5 #####################################
# Úprava časových řad
cpi_mod <- CPI$CPI
cpi_stac_mod <- ((cpi_mod/lag(cpi_mod,12))-1)*100
cpi_stac_mod_df <- data.frame(date = index(cpi_stac_mod), CPI_stac = as.vector(cpi_stac_mod))

############################### ULOHA Č. 5.1 ###################################

# Vykresleni casove rady
ggplot() +
    geom_line(data = cpi_stac_mod_df, aes(x = date, y = CPI_stac), color = "#4BACC6") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

############################### ULOHA Č. 5.2 ###################################

# provedeni ACF a PACF
par(mfrow = c(2,1))
acf(cpi_stac_mod, lag.max = 24, na.action = na.pass)
pacf(cpi_stac_mod, lag.max = 24, na.action = na.pass)

# namodelovani ARMA modelu
armacpi_11 <-arima(cpi_stac_mod, order = c(1,0,1))
print(armacpi_11)

armacpi_21 <-arima(cpi_stac_mod, order = c(2,0,1))
print(armacpi_21)

armacpi_31 <-arima(cpi_stac_mod, order = c(3,0,1))
print(armacpi_31)

armacpi_12 <-arima(cpi_stac_mod, order = c(1,0,2))
print(armacpi_12)

armacpi_13 <-arima(cpi_stac_mod, order = c(1,0,3))
print(armacpi_13)

armacpi_22 <-arima(cpi_stac_mod, order = c(2,0,2))
print(armacpi_22)

############################### ULOHA Č. 5.3 ###################################
# ulozeni a vykresleni rezidui
par(mfrow = c(2,1))
e_cpi_11 <- residuals(armacpi_11)
acf(e_cpi_11, lag.max = 24, na.action = na.pass)
pacf(e_cpi_11, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_cpi_12 <- residuals(armacpi_12)
acf(e_cpi_12, lag.max = 24, na.action = na.pass)
pacf(e_cpi_12, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_cpi_13 <- residuals(armacpi_13)
acf(e_cpi_13, lag.max = 24, na.action = na.pass)
pacf(e_cpi_13, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_cpi_21 <- residuals(armacpi_21)
acf(e_cpi_21, lag.max = 24, na.action = na.pass)
pacf(e_cpi_21, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_cpi_22 <- residuals(armacpi_22)
acf(e_cpi_22, lag.max = 24, na.action = na.pass)
pacf(e_cpi_22, lag.max = 24, na.action = na.pass)

par(mfrow = c(2,1))
e_cpi_31 <- residuals(armacpi_31)
acf(e_cpi_31, lag.max = 24, na.action = na.pass)
pacf(e_cpi_31, lag.max = 24, na.action = na.pass)

# Provedení Q-testu
Box.test(e_cpi_11, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 2
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_11, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_cpi_12, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 3
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_12, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_cpi_13, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 4
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_13, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_cpi_21, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 3
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_21, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_cpi_22, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 4
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_22, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

Box.test(e_cpi_31, lag = 8, type = c("Ljung-Box"), fitdf = 2)
max_test = 20
p = 4
for (my_lag in seq(p+1,max_test,by=1)){
    pom_test <- Box.test(e_cpi_31, lag = my_lag, type = c("Ljung-Box"),
                         fitdf = p)
    sprintf("Zpozdeni %2.0f, statistika %6.3f , p-hodnota %5.4f\n ",
            my_lag,pom_test$statistic,pom_test$p.value) %>% cat()
}

# Akaikeho identifikacni kriterium
AIC(armacpi_11)
AIC(armacpi_12)
AIC(armacpi_13)
AIC(armacpi_21)
AIC(armacpi_22)
AIC(armacpi_31)
