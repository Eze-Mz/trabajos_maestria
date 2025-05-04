# MAESTRÍA EN ESTADÍSTICA APLICADA. UNC
# DISEÑO DE ESTUDIOS CUANTITATIVOS. TRABAJO FINAL 2024

# --- Libraries --- #
# https://tidyverse.tidyverse.org/
library(tidyverse)
library(ggplot2)
library(skimr)
library(dlookr)
library(janitor)
library(purrr)
library(car)

# --- Data --- #

# Set working directory (if needed)
#setwd("C:/Users/Admin/Desktop/Trabajo final DEC/")

# Read CSV file
data <- read.csv("DATOS/fb.csv", sep = ",")

# --- Pre - processing --- #

# View first few rows of the data
head(data)

# Optionally, view the entire data in a spreadsheet-like viewer
View(data)

# Create a new column condVEF
data <- data %>%
  mutate(
    CondVEF = case_when(
      VEF1 <= 79 & VEF1 >= 65 ~ "Leve",
      VEF1 < 65 & VEF1 >= 50 ~ "Moderado",
      VEF1 < 50 & VEF1 >= 35 ~ "Grave",
      VEF1 < 35 ~ " Muy grave"
    )
  ) %>%
  relocate(CondVEF, .after = VEF1)  

# Convert string to factors

data$Sexo <- as.factor(data$Sexo)

data$CondPeso <- as.factor(data$CondPeso)

data$CondVEF <- as.factor(data$CondVEF)


# --- Exploratory data analysis --- #

## VEF1 - sexo ##

# exploración
table(data$Sexo)
aggregate(VEF1 ~ Sexo, data = data, FUN = mean)
aggregate(VEF1 ~ Sexo, data = data, FUN = sd)
ggplot(data = data, aes(x = Sexo, y = VEF1, color = Sexo)) +
  geom_boxplot() +
  theme_bw()

# supuesto de normalidad
par(mfrow = c(1,2))
qqnorm(data[data$Sexo == "varon","VEF1"], main = "varon")
qqline(data[data$Sexo == "varon","VEF1"])
qqnorm(data[data$Sexo == "mujer","VEF1"], main = "mujer")
qqline(data[data$Sexo == "mujer","VEF1"])
#test


#supuesto de homocedasticidad


#Análisis de varianza ANOVA
anova <- aov(data$VEF1 ~ data$Sexo)
summary(anova)
plot(anova)

# calcular el eta
eta_cuadrado <- 839.7/(839.7 + 2169.4)
eta_cuadrado

## VEF1 - CondPeso ##
table(data$CondPeso)
aggregate(VEF1 ~ CondPeso, data = data, FUN = mean)
aggregate(VEF1 ~ CondPeso, data = data, FUN = sd)

ggplot(data = data, aes(x = CondPeso, y = CRF, color = Sexo)) +
  geom_boxplot() +
  theme_bw()

anava <- aov(data$VEF1 ~ data$CondPeso)
summary(anava)
plot(anava)

# supuesto de normalidad
par(mfrow = c(1,2))
qqnorm(data[data$Sexo == "varon","VEF1"], main = "varon")
qqline(data[data$Sexo == "varon","VEF1"])
qqnorm(data[data$Sexo == "mujer","VEF1"], main = "mujer")
qqline(data[data$Sexo == "mujer","VEF1"])
#test


#supuesto de homocedasticidad


#Análisis de varianza ANOVA
anova <- aov(data$VEF1 ~ data$Sexo)
summary(anova)
plot(anova)

# calcular el eta
eta_cuadrado <- 839.7/(839.7 + 2169.4)
eta_cuadrado






