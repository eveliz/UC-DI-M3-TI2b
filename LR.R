# @author : Edgardo Pedro Veliz Limas
# @date   : 2020-03-12  

install.packages("tidyverse")
install.packages("scales")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("gridExtra")
install.packages("GGally")
install.packages("corrplot")
install.packages("lmtest")
install.packages("latticeExtra")
install.packages("Hmisc")
install.packages("gdata")
install.packages("corrr")
install.packages("nortest")

library(tidyverse)
library(scales)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(GGally)
library(corrplot)
library(lmtest)
library(car)
library(Hmisc)
library(gdata)
library(corrr)
library(nortest)

# LEYENDO LOS DATOS
# Leyendo los datos
df_data <- read.csv("./data/climate_change.csv", header = TRUE)
df_data <- na.omit(df_data) # Eliminando valores perdidos
str(df_data)
summary(df_data)

# Dividiendo los datos en dos grupos (train y test)
df_train = subset(df_data, Year <= 2006)
df_test = subset(df_data, Year > 2006)
write.csv(df_train, "./data/climate_change_train.csv",row.names = FALSE)
write.csv(df_test, "./data/climate_change_test.csv", row.names = FALSE)

nameY <- "Temp"
nameX <- c("CO2", "CH4", "N2O", "CFC.11", "CFC.12", "MEI", "TSI", "Aerosols")
nameVars <- c(nameX, nameY)

# Explorando los datos
# Se creerá un nuevo data frame para visualización, primero se va a reescalar todas las columnas a [0, 100], excepto la
# variable dependiente, luego se cambiará su estructura (reshape) para utilizar el tipo de gráfico violin plot
df_vis <- data.frame(lapply(df_data[, nameX], rescale, c(0, 10)), df_data[, nameY])
colnames(df_vis) <- colnames(df_data[, nameVars])

df_vis <- 
  df_vis%>% 
  gather(key = "var_name", value = "var_value", -nameY) %>%
  slice(rep(row_number(), var_value)) %>%
  select(-var_value)

df_vis$var_name <- reorder.factor(df_vis$var_name, new.order=nameX)
df_vis <- df_vis %>% arrange(var_name)

p <- 
  ggplot(data = df_vis) +
  aes(var_name, df_vis[, nameY]) +
  geom_violin(adjust = .5) + 
  stat_summary(fun.data = mean_sdl, geom = "pointrange", color="red") + 
  labs(x = "Variables", y = nameY)
p

# Graficando las correlaciones
ggpairs(df_data[, nameVars], lower = list(continuous = "smooth"), diag = list(continuous = "bar"), axisLabels = "none")
corrplot.mixed(round(cor(df_data[, nameVars]), digits = 2), tl.pos = "lt")

# Otro tipo de gráfico
#nameVarsc <- c("CO2", "CH4", "N2O", "CFC.11", "CFC.12")
df_data[, nameX] %>% correlate() %>% fashion()
df_data[, nameX] %>% correlate() %>% network_plot(curved = TRUE, min_cor = .01)
                                            
# EL MODELO
# Entrenando el modelo
m1 <- lm(formula = as.formula(paste(nameY, " ~ ", ".")), 
         data = df_train[, nameVars])
summary(m1)

# Selección de predictores
step(m1, direction = "both", trace = 1)

temp_nameX <- c("CO2", "N2O", "CFC.11", "CFC.12", "MEI", "TSI", "Aerosols")
m2 <- lm(as.formula(paste(nameY, " ~ ", paste(temp_nameX, collapse = " + "), sep = "")), 
         data = df_train[nameVars])
summary(m2)

temp_nameX <- c("CO2", "MEI", "TSI", "Aerosols")
m3a <- lm(as.formula(paste(nameY, " ~ ", paste(temp_nameX, collapse = " + "), sep = "")), 
         data = df_train[nameVars])
summary(m3a)

temp_nameX <- c("N2O", "MEI", "TSI", "Aerosols")
m3b <- lm(as.formula(paste(nameY, " ~ ", paste(temp_nameX, collapse = " + "), sep = "")), 
         data = df_train[nameVars])
summary(m3b)

temp_nameX <- c("CFC.11", "MEI", "TSI", "Aerosols")
m3c <- lm(as.formula(paste(nameY, " ~ ", paste(temp_nameX, collapse = " + "), sep = "")), 
          data = df_train[nameVars])
summary(m3c)

temp_nameX <- c("CFC.12", "MEI", "TSI", "Aerosols")
m3d <- lm(as.formula(paste(nameY, " ~ ", paste(temp_nameX, collapse = " + "), sep = "")), 
          data = df_train[nameVars])
summary(m3d)

step(m3a, direction = "both", trace = 1)

# ANÁLISIS DE SUPUESTOS
# Linealidad de las variables respecto a los residuos
final_nameX <- c("CO2", "MEI", "TSI", "Aerosols")
final_model <- m3a
summary(final_model)

var_res_plot <- function(var_name, df, linear_model){
  p <- 
    ggplot(data = df) +
    aes(df[, var_name], linear_model$residuals) +
    geom_point() + geom_smooth(color = "firebrick") +   geom_hline(yintercept = 0) + 
    labs(x = var_name) +
    theme_bw()
  return(p)
}

plots <- lapply(X = final_nameX, FUN = var_res_plot, df = df_train, linear_model = final_model)
marrangeGrob(plots, nrow = round(length(plots) / 2, 0), ncol = floor(length(plots) / 2))

# Media cero de los residuos
mean(final_model$residuals)

# Homocedasticidad (varianza constante de los residuos)
ggplot(data = df_train, aes(final_model$fitted.values, final_model$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()
bptest(final_model)

# No multicolinialidad
# a) Matriz de correlación entre predictores
corrplot(cor(df_train[, final_nameX]), method = "number", tl.col = "black")

# b) Análisis de Inflación de Varianza (VIF)
vif(final_model)


# Distribución normal de los residuos
qqnorm(final_model$residuals)
qqline(final_model$residuals)

lillie.test(final_model$residuals)