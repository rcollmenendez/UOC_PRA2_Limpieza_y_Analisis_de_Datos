# ----------------------------------
# Tipología de datos - PRA2
# ----------------------------------

library(tidyverse)

# ----------------------------------
# 2. Integración de los datos
# ----------------------------------

# Set working directory
setwd("C:/Users/RCOLL/Downloads/wine_data")

# Lectura de los datos
df <- read.csv("winequality-red.csv", header=TRUE, 
                       sep=",", na.strings="NA", dec=".", strip.white=TRUE)

class(df)

# Nombres de columnas
colnames(df)

# Primer vistazo
str(df)
summary(df)

# ----------------------------------
# 3. Limpieza de los datos
# ----------------------------------

summary(df)
nrow(df)

# Duplicados
duplicates <- df[duplicated(df),]
nrow(duplicates)
duplicates

df <- unique(df)
nrow(df)

# Número de ceros
colSums(df == 0)
# Número de nulos
colSums(is.na(df))

# Replace 0's
# df[df == 0] <- NA
# colSums(is.na(df))

# Imputar 0 
# df$citric.acid[is.na(df$citric.acid)] <- mean(df$citric.acid, na.rm = T)
# colSums(df == 0)

# summary(df)


# Outliers

summary(df)

# Variable - Fixed Acidity
fixed_acidity <- df$fixed.acidity
variable <- 'fixed_acidity'
fixed_acidity_norm <- rnorm(200,mean=mean(fixed_acidity, na.rm=TRUE), sd=sd(fixed_acidity, na.rm=TRUE))

# Boxplot
b <- boxplot(fixed_acidity, fixed_acidity_norm,
        main = "Boxplots",
        at = c(1,2),
        names = c(variable, "normal_dist"),
        col = rgb(0.1,0.1,0.7,0.5),
        horizontal = TRUE
)

# Num de outliers
length(b$out)

# Outliers
b$out

# Rango intercuartílico
b$stats

# Histograma
hist(fixed_acidity,
     main="Histograma",
     freq=FALSE
)
lines(density(fixed_acidity))
lines(seq(min(fixed_acidity), max(fixed_acidity), by=.5),
      dnorm(seq(min(fixed_acidity), max(fixed_acidity),
                by=.5),mean(fixed_acidity), sd(fixed_acidity)), col="blue")


# Variable - Volatile acidity
volatile_acidity <- df$volatile.acidity
variable <- 'volatile_acidity'
volatile_acidity_norm <- rnorm(200,mean=mean(volatile_acidity, na.rm=TRUE), sd=sd(volatile_acidity, na.rm=TRUE))

# Boxplot
b <- boxplot(volatile_acidity, volatile_acidity_norm,
             main = "Boxplots",
             at = c(1,2),
             names = c(variable, "normal_dist"),
             col = rgb(0.1,0.1,0.7,0.5),
             horizontal = TRUE
)

# Num de outliers
length(b$out)

# Outliers
outliers <- b$out

# Rango intercuartílico
b$stats

# Histograma
hist(volatile_acidity,
     main="Histograma",
     freq=FALSE
)
lines(density(volatile_acidity))
lines(seq(min(volatile_acidity), max(volatile_acidity), by=0.05),
      dnorm(seq(min(volatile_acidity), max(volatile_acidity),
                by=0.05),mean(volatile_acidity), sd(volatile_acidity)), col="blue")

# Exclusion outliers - Volatile acidity

df <- df[-which(df$volatile.acidity %in% outliers),]

nrow(df)
ncol(df)

summary(df)

# -------------- # Variable - Volatile acidity
volatile_acidity <- df$volatile.acidity
variable <- 'volatile_acidity'
volatile_acidity_norm <- rnorm(200,mean=mean(volatile_acidity, na.rm=TRUE), sd=sd(volatile_acidity, na.rm=TRUE))

# Boxplot
b <- boxplot(volatile_acidity, volatile_acidity_norm,
             main = "Boxplots",
             at = c(1,2),
             names = c(variable, "normal_dist"),
             col = rgb(0.1,0.1,0.7,0.5),
             horizontal = TRUE
)

# Num de outliers
length(b$out)

# Outliers
b$out

# Rango intercuartílico
b$stats

# Histograma
hist(volatile_acidity,
     main="Histograma",
     freq=FALSE
)
lines(density(volatile_acidity))
lines(seq(min(volatile_acidity), max(volatile_acidity), by=0.05),
      dnorm(seq(min(volatile_acidity), max(volatile_acidity),
                by=0.05),mean(volatile_acidity), sd(volatile_acidity)), col="blue")



# Variable - residual.sugar
residual_sugar <- df$residual.sugar
variable <- 'residual_sugar'
residual_sugar_norm <- rnorm(200,mean=mean(residual_sugar, na.rm=TRUE), sd=sd(residual_sugar, na.rm=TRUE))

# Boxplot
b <- boxplot(residual_sugar, residual_sugar_norm,
             main = "Boxplots",
             at = c(1,2),
             names = c(variable, "normal_dist"),
             col = rgb(0.1,0.1,0.7,0.5),
             horizontal = TRUE
)

# Num de outliers
length(b$out)

# Outliers
b$out

# Rango intercuartílico
b$stats

# Histograma
hist(residual_sugar,
     main="Histograma",
     freq=FALSE
)
lines(density(residual_sugar))
lines(seq(min(residual_sugar), max(residual_sugar), by=.5),
      dnorm(seq(min(residual_sugar), max(residual_sugar),
                by=.5),mean(residual_sugar), sd(residual_sugar)), col="blue")


# Variable - density
density <- df$density
variable <- 'density'
density_norm <- rnorm(200,mean=mean(density, na.rm=TRUE), sd=sd(density, na.rm=TRUE))

# Boxplot
b <- boxplot(density, density_norm,
             main = "Boxplots",
             at = c(1,2),
             names = c(variable, "normal_dist"),
             col = rgb(0.1,0.1,0.7,0.5),
             horizontal = TRUE
)

# Num de outliers
length(b$out)

# Outliers
b$out

# Rango intercuartílico
b$stats

# Histograma
hist(density,
     main="Histograma",
     freq=FALSE
)
lines(density(density))
lines(seq(min(density), max(density), by=.05),
      dnorm(seq(min(density), max(density),
                by=.05),mean(density), sd(density)), col="blue")


# Variable - alcohol
alcohol <- df$alcohol
variable <- 'alcohol'
alcohol_norm <- rnorm(200,mean=mean(alcohol, na.rm=TRUE), sd=sd(alcohol, na.rm=TRUE))

# Boxplot
b <- boxplot(alcohol, alcohol_norm,
             main = "Boxplots",
             at = c(1,2),
             names = c(variable, "normal_dist"),
             col = rgb(0.1,0.1,0.7,0.5),
             horizontal = TRUE
)

# Num de outliers
length(b$out)

# Outliers
b$out

# Rango intercuartílico
b$stats

# Histograma
hist(alcohol,
     main="Histograma",
     freq=FALSE
)
lines(density(alcohol))
lines(seq(min(alcohol), max(alcohol), by=.05),
      dnorm(seq(min(alcohol), max(alcohol),
                by=.05),mean(alcohol), sd(alcohol)), col="blue")


# Variable - quality
quality <- df$quality
variable <- 'quality'
quality_norm <- rnorm(200,mean=mean(quality, na.rm=TRUE), sd=sd(quality, na.rm=TRUE))

# Boxplot
b <- boxplot(quality, quality_norm,
             main = "Boxplots",
             at = c(1,2),
             names = c(variable, "normal_dist"),
             col = rgb(0.1,0.1,0.7,0.5),
             horizontal = TRUE
)

# Num de outliers
length(b$out)

# Outliers
b$out

# Rango intercuartílico
b$stats

# Histograma
hist(quality,
     main="Histograma",
     breaks = 5,
     freq=FALSE
)
lines(density(quality))
lines(seq(min(quality), max(quality), by=.05),
      dnorm(seq(min(quality), max(quality),
                by=.05),mean(quality), sd(quality)), col="blue")




#Subset data
# df <- subset(data,select=c("generosity"))
# head(df, n = 10)

# ----------------------------------
# 4. Análisis
# ----------------------------------

cor(df)

v_color <- viridis::viridis(n = nrow(df))
df$color <- v_color[Matrix::invPerm(p = order(x = df$quality))]

pairs(
  formula = quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
    free.sulfur.dioxide + density + pH + sulphates + alcohol,
  data = df,
  col = df$color,
  pch = 19
)

# Correlación
library(GGally)
ggpairs(data.frame(df), lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

round(cor(df),3)

# Escalamos los datos
df <- scale(df)
summary(df)


# MODELO REGRESIÓN LINEAL MÚLTIPLE

# Modelo sin variables
rlm_0 <- lm(quality ~ 1,data = data.frame(df) )
summary(rlm_0)

# Modelo con todas las variables
rlm_1 <- lm(quality ~ .,data = data.frame(df) )
summary(rlm_1)

# Regresión stepwise - Selección mejores predictores
stw <- step(rlm_0, scope = list(lower=rlm_0, upper=rlm_1), direction = "both")
summary(stw)

# Mejor modelo
modelo <- lm(quality ~ alcohol + volatile.acidity + sulphates + chlorides + 
               total.sulfur.dioxide + pH + free.sulfur.dioxide, data = data.frame(df))
summary(modelo)

confint(modelo)

round(coef(modelo),3)

# Distribución normal de los residuos - Normalidad de la variable dependiente
qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(my_data$BirthWeightGm)
hist(my_data$BirthWeightGm)

# Homocedasticidad - Variabilidad de los residuos
ggplot(data = df, aes(modelo$fitted.values, modelo$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()

library(lmtest)
bptest(modelo)
