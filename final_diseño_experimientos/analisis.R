##### LOAD DATA #####
library(tidyverse)
library(ggplot2)
library(skimr)

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


### PeMAX ###

stepwise_model <- step(lm(PeMAX ~ VEF1 + CRF + CPT + BMP + IMC + Altura + Peso + VR + Edad + Sexo+CondPeso, data = data), direction = "both")
summary(stepwise_model)


lm(PeMAX ~ VEF1 + IMC + Peso, data = data) %>% plot()
lm(PeMAX ~ VEF1 + IMC + Peso, data = data) %>% summary()
lm(formula = PeMAX ~ VEF1 + BMP + Peso + VR, data = data) %>% summary()

# Modelo saturado - prueba 1
modelo_saturado1 <- lm(PeMAX ~ . -CondVEF -CondPeso, data = data)  
modelo_step1 <- step(modelo_saturado1, direction = "both") # el argumento direction se puede omitir
summary(modelo_step1)

# Modelo saturado - prueba 2
modelo_saturado2 <- lm(PeMAX ~ VEF1 + CRF + CPT  + VR , data = data)
modelo_step2 <- step(modelo_saturado2, direction = "both")
summary(modelo_step2)

library(performance) 
compare_performance(modelo_saturado1, 
                    modelo_saturado2,
                    metrics = "common")

# lo anterior es consistente con:
library(ggcorrplot)
correlation <- cor(data[,c("VEF1", "PeMAX", "CRF", "CPT", "BMP", "IMC", "Altura", "Peso", "VR", "Edad")])
ggcorrplot(correlation, hc.order = TRUE, type = "lower", lab = TRUE)

library(dlookr)
data |>       
  select(PeMAX, VEF1, VR) |>       
  correlate() |>       
  arrange(desc(abs(coef_corr)))  

data |>       
  select(PeMAX, VEF1, VR, CRF, CPT, Peso) |>        
  correlate() |>      
  plot()


### EDAD ###
aov(PeMAX ~ Edad, data = data) %>% summary()
lm(PeMAX ~ Edad, data = data) %>% summary()
plot(PeMAX ~ Edad, data = data)

lm(VEF1 ~ Edad, data = data) %>% summary()
plot(VEF1 ~ Edad, data = data)

lm(CRF ~ Edad, data = data) %>% summary()
plot(CRF ~ Edad, data = data)

lm(CPT ~ Edad, data = data) %>% summary()
plot(CPT ~ Edad, data = data)

lm(VR ~ Edad, data = data) %>% summary()
plot(VR ~ Edad, data = data)


### COLINEALIDAD ###
library(performance)
modelo <- lm(PeMAX ~ VEF1 + CRF + CPT + VR + Sexo, data = data)
summary(modelo)
check_collinearity(modelo)
vif(modelo)
plot(CRF)
