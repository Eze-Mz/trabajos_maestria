'

# MAESTRÍA EN ESTADÍSTICA APLICADA. UNC
# DISEÑO DE ESTUDIOS CUANTITATIVOS. TRABAJO FINAL 2024

# Alumnos

# Santa Cruz, Leandro Ezequiel
# Córdoba, Ezequiel Muñoz
# Alumno 3

'

#### Desarrollo ####
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("dlookr")
install.packages("car")
install.packages("purrr")

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
setwd("~/PROYECTOS R")
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

summary(data)

# Initial exploration
glimpse(data)

# Describe numerical variables
describe(data, -N)

# Describe categorical variables
data %>%  
  tabyl(CondPeso, CondVEF, Sexo) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title()

# Data summary
skim(data)

# Grouped summary
data %>% 
  group_by(Sexo) %>%
  select(- N) %>% 
  skim()

data %>% 
  group_by(CondPeso) %>%
  select(- N) %>% 
  skim()

data %>% 
  group_by(CondVEF) %>%
  select(- N) %>%
  skim()

# --- Graphical exploratory data analysis --- #

# Efectos de las condiciones antropométricas sobre las mediciones de las funciones pulmonares

# Variables respiratorias en función de la condición del peso y sexo

data %>% 
  ggplot(aes(CondPeso, VEF1, color = Sexo)) + 
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.4))

data %>% 
  ggplot(aes(CondPeso, VR, color = Sexo)) + 
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25))

data %>% 
  ggplot(aes(CondPeso, CRF, color = Sexo)) + 
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25))

data %>% 
  ggplot(aes(CondPeso, CPT, color = Sexo)) + 
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.25))
#

# BMP en función de la función pulmonar y sexo

data %>% 
  ggplot(aes(CondVEF, PeMAX, color = Sexo)) + 
  geom_boxplot() + 
  geom_point(position = position_jitterdodge(jitter.width = 0.25)) + 
  facet_grid(Sexo ~ CondPeso)

data %>% 
  ggplot(aes(Sexo, y = VEF1, color = Sexo)) + 
  geom_boxplot() + 
  geom_jitter(size=1, alpha=0.9)

data %>% 
  ggplot(aes(CondVEF, IMC, color = Sexo)) + 
  geom_boxplot()

data %>% 
  ggplot(aes(Sexo, VEF1)) + 
  geom_boxplot() 

data %>% 
  ggplot(aes(Sexo, VEF1, color = CondPeso)) + 
  geom_boxplot()

data %>% 
  ggplot(aes(CondPeso, VEF1, color = Sexo)) + 
  geom_boxplot() 

data %>% 
  ggplot(aes(CondPeso, PeMAX, color = Sexo)) + 
  geom_boxplot() 

data %>% 
  ggplot(aes(CondVEF, PeMAX, color = Sexo)) + 
  geom_boxplot() 

# --- Modelos --- #

attach(data)
model_1 <- aov(VEF1 ~ CondPeso)

summary(model_1)

model_2 <- aov(VEF1 ~ Sexo)

summary(model_2)

model_3 <- aov(VEF1 ~ Sexo*CondPeso)

summary(model_3)

model_4 <- lm(VEF1 ~ IMC + Sexo + Edad)

summary(model_4)
plot(model_4)
vif(model_4)

model_4.1 <- lm(VEF1 ~ IMC + Sexo + VR)

summary(model_4.1)
plot(model_4.1)
vif(model_4.1)

model_5 <- lm(VR ~ IMC + Sexo + Edad)
summary(model_5)
plot(model_5)
vif(model_5)

model_6 <- lm(CRF ~ IMC + Sexo + Edad)
summary(model_6)
plot(model_6)
vif(model_6)

model_7 <- lm(PeMAX ~ IMC + Sexo + Edad)
summary(model_7)
plot(model_7)
vif(model_7)



# Test de homocedasticidad Barlet
bartlett.test(VEF1 ~ Sexo, data = data)

#

require(car)
leveneTest(VEF1 ~ IMC+Sexo,data,center = "median")



Version sin medias


ggplot(data, aes(x = CondPeso, y = VEF1, color = Sexo, group = Sexo)) +
  geom_line() +
  labs(title = "Interaction Plot", 
       x = "Factor A", 
       y = "Response Variable (Y)", 
       color = "Factor B") +
  theme_minimal()


# Versión con medias

agg_data <- aggregate(VEF1 ~ CondPeso * Sexo, data = data, FUN = mean)

ggplot(agg_data, aes(x = CondPeso, y = VEF1, color = Sexo, group = Sexo)) +
  geom_line() +
  geom_point() +
  labs(title = "Gráficos de interacción", 
       x = "Condición de peso", 
       y = "VEF1 (Y)", 
       color = "Sexo") +
  theme_minimal()
  
  
model_5 <- aov(VEF1 ~ CondPeso * Sexo)

summary(model_5)
  
  
  
  
  
  
  
  
  

plot1 <- ggplot(data, aes(x = factor(CondPeso), y = Pemax, fill = factor(Sexo))) + 
  geom_boxplot(color = "black", alpha = 0.7) + 
  labs(title = "One-Way ANOVA", 
       x = "Condición de peso", 
       y = "PeMAX") + 
  theme_minimal() + 
  theme(legend.position = "top") 

plot1

# Analysis of variance

model_1 <- aov(Pemax ~ factor(Sexo)*factor(CondPeso))

summary(model_1)

'
data_summary <- data %>%
  summarize(
    mean_age = mean(age, na.rm = TRUE),
    median_salary = median(salary, na.rm = TRUE),
    sd_salary = sd(salary, na.rm = TRUE)
  )
print(data_summary)
'

group_summary <- data %>%
  group_by(Sexo) %>%
  summarize(
    avg_age = mean(Edad, na.rm = TRUE),
    avg_weight = mean(Peso, na.rm = TRUE),
    avg_height = mean(Altura, na.rm = TRUE),
    .groups = 'drop'
  )
print(group_summary)

help(vif)

# correlación entre variables

# calculate correlation between variables
cor(data[,c("VEF1", "PeMAX", "CRF", "CPT", "BMP", "IMC", "Altura", "Peso", "VR", "Edad")])  
# heatmap graph for correlation
install.packages("ggcorrplot")
library(ggcorrplot)
correlation <- cor(data[,c("VEF1", "PeMAX", "CRF", "CPT", "BMP", "IMC", "Altura", "Peso", "VR")])
ggcorrplot(correlation, hc.order = TRUE, type = "lower", lab = TRUE)

cor.test(data$CRF, data$CPT)
cor.test(data$VR, data$CPT)
cor.test(data$VR, data$CRF)

# PeMAx vs all variables
lm(PeMAX ~ VEF1 + CRF + CPT + BMP + IMC + Altura + Peso + VR + Edad + Sexo+CondPeso, data = data) %>% summary()


stepwise_model <- step(lm(PeMAX ~ VEF1 + CRF + CPT + BMP + IMC + Altura + Peso + VR + Edad + Sexo+CondPeso, data = data), direction = "both")
summary(stepwise_model)


lm(PeMAX ~ VEF1 + IMC + Peso, data = data) %>% plot()


