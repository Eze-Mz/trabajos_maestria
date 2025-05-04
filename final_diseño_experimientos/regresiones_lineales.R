##### LOAD DATA #####
library(tidyverse)
library(ggplot2)
library(skimr)
library(ggcorrplot)
library(performance)

setwd("~/PROYECTOS R")
data <- read.csv("DATOS/fb.csv", sep = ",")
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

view(data)

### RL VEF1 ###
correlation <- cor(data[,c("VEF1", "Edad", "Altura", "Peso", "IMC", "BMP")])
ggcorrplot(correlation, hc.order = TRUE, type = "lower", lab = TRUE)

cowplot::plot_grid(
  ggplot(data, aes(x = Edad, y = VEF1)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = Altura, y = VEF1)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = Peso, y = VEF1)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = IMC, y = VEF1)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = BMP, y = VEF1)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  nrow = 3
)


lm_VEF1_edad <- lm(VEF1 ~ Edad, data = data)
summary(lm_VEF1_edad)
plot(lm_VEF1_edad)
cor.test(data$VEF1, data$Edad)
check_model(lm_VEF1_edad)


lm_VEF1_altura <- lm(VEF1 ~ Altura, data = data)
summary(lm_VEF1_altura)
plot(lm_VEF1_altura)
cor.test(data$VEF1, data$Altura)
check_model(lm_VEF1_altura)

lm_VEF1_peso <- lm(VEF1 ~ Peso, data = data)
summary(lm_VEF1_peso)
plot(lm_VEF1_peso)
cor.test(data$VEF1, data$Peso)
check_model(lm_VEF1_peso)

lm_VEF1_imc <- lm(VEF1 ~ IMC, data = data)
summary(lm_VEF1_imc)
plot(lm_VEF1_imc)
check_model(lm_VEF1_imc)

lm_VEF1_BMP <- lm(VEF1 ~ BMP, data = data)
summary(lm_VEF1_BMP)
plot(lm_VEF1_BMP)
check_model(lm_VEF1_BMP)

### RL VR ###
correlation <- cor(data[,c("VR", "Edad", "Altura", "Peso", "IMC", "BMP")])
ggcorrplot(correlation, hc.order = TRUE, type = "lower", lab = TRUE)

cowplot::plot_grid(
  ggplot(data, aes(x = Edad, y = VR)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = Altura, y = VR)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = Peso, y = VR)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = IMC, y = VR)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = BMP, y = VR)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  nrow = 3
)

lm_VR_edad <- lm(VR ~ Edad, data = data)
summary(lm_VR_edad)
plot(lm_VR_edad)
cor.test(data$VR, data$Edad)
check_model(lm_VR_edad)

lm_VR_altura <- lm(VR ~ Altura, data = data)
summary(lm_VR_altura)
plot(lm_VR_altura)
cor.test(data$VR, data$Altura)

lm_VR_peso <- lm(VR ~ Peso, data = data)
summary(lm_VR_peso)
plot(lm_VR_peso)
cor.test(data$VR, data$Peso)

lm_VR_imc <- lm(VR ~ IMC, data = data)
summary(lm_VR_imc)
plot(lm_VR_imc)
cor.test(data$VR, data$IMC)

lm_VR_BMP <- lm(VR ~ BMP, data = data)
summary(lm_VR_BMP)
plot(lm_VR_BMP)
cor.test(data$VR, data$BMP)

### RL CRF ###
correlation <- cor(data[,c("CRF", "Edad", "Altura", "Peso", "IMC", "BMP")])
ggcorrplot(correlation, hc.order = TRUE, type = "lower", lab = TRUE)

cowplot::plot_grid(
  ggplot(data, aes(x = Edad, y = CRF)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = Altura, y = CRF)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = Peso, y = CRF)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = IMC, y = CRF)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = BMP, y = CRF)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  nrow = 3
)

lm_CRF_edad <- lm(CRF ~ Edad, data = data)
summary(lm_CRF_edad)
plot(lm_CRF_edad)
cor.test(data$CRF, data$Edad)

lm_CRF_altura <- lm(CRF ~ Altura, data = data)
summary(lm_CRF_altura)
plot(lm_CRF_altura)
cor.test(data$CRF, data$Altura)

lm_CRF_peso <- lm(CRF ~ Peso, data = data)
summary(lm_CRF_peso)
plot(lm_CRF_peso)
cor.test(data$CRF, data$Peso)

lm_CRF_imc <- lm(CRF ~ IMC, data = data)
summary(lm_CRF_imc)
plot(lm_CRF_imc)
cor.test(data$CRF, data$IMC)

lm_CRF_BMP <- lm(CRF ~ BMP, data = data)
summary(lm_CRF_BMP)
plot(lm_CRF_BMP)
cor.test(data$CRF, data$BMP)

### RL CPT ###
correlation <- cor(data[,c("CPT", "Edad", "Altura", "Peso", "IMC", "BMP")])
ggcorrplot(correlation, hc.order = TRUE, type = "lower", lab = TRUE)

cowplot::plot_grid(
  ggplot(data, aes(x = Edad, y = CPT)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = Altura, y = CPT)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = Peso, y = CPT)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = IMC, y = CPT)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = BMP, y = CPT)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  nrow = 3
)

lm_CPT_edad <- lm(CPT ~ Edad, data = data)
summary(lm_CPT_edad)
plot(lm_CPT_edad)
cor.test(data$CPT, data$Edad)

lm_CPT_altura <- lm(CPT ~ Altura, data = data)
summary(lm_CPT_altura)
plot(lm_CPT_altura)
cor.test(data$CPT, data$Altura)

lm_CPT_peso <- lm(CPT ~ Peso, data = data)
summary(lm_CPT_peso)
plot(lm_CPT_peso)
cor.test(data$CPT, data$Peso)

lm_CPT_imc <- lm(CPT ~ IMC, data = data)
summary(lm_CPT_imc)
plot(lm_CPT_imc)
cor.test(data$CPT, data$IMC)

lm_CPT_BMP <- lm(CPT ~ BMP, data = data)
summary(lm_CPT_BMP)
plot(lm_CPT_BMP)
cor.test(data$CPT, data$BMP)

### RL PeMAX ###
correlation <- cor(data[,c("PeMAX", "Edad", "Altura", "Peso", "IMC", "BMP")])
ggcorrplot(correlation, hc.order = TRUE, type = "lower", lab = TRUE)

cowplot::plot_grid(
  ggplot(data, aes(x = Edad, y = PeMAX)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = Altura, y = PeMAX)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = Peso, y = PeMAX)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = IMC, y = PeMAX)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  ggplot(data, aes(x = BMP, y = PeMAX)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal(),
  nrow = 3
)

lm_PeMAX_edad <- lm(PeMAX ~ Edad, data = data)
summary(lm_PeMAX_edad)
plot(lm_PeMAX_edad)
cor.test(data$PeMAX, data$Edad)

lm_PeMAX_altura <- lm(PeMAX ~ Altura, data = data)
summary(lm_PeMAX_altura)
plot(lm_PeMAX_altura)
cor.test(data$PeMAX, data$Altura)

lm_PeMAX_peso <- lm(PeMAX ~ Peso, data = data)
summary(lm_PeMAX_peso)
plot(lm_PeMAX_peso)
cor.test(data$PeMAX, data$Peso)

lm_PeMAX_imc <- lm(PeMAX ~ IMC, data = data)
summary(lm_PeMAX_imc)
plot(lm_PeMAX_imc)
cor.test(data$PeMAX, data$IMC)

lm_PeMAX_BMP <- lm(PeMAX ~ BMP, data = data)
summary(lm_PeMAX_BMP)
plot(lm_PeMAX_BMP)
cor.test(data$PeMAX, data$BMP)


