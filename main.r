library(readxl)
LATAM19 <- read_excel("Documents/UNIVERSIDAD/5 SEMESTRE/Análisis Estadístico de Datos/Proyecto/AED BD/LATAM19.xlsx")
LATAM20 <- read_excel("Documents/UNIVERSIDAD/5 SEMESTRE/Análisis Estadístico de Datos/Proyecto/AED BD/LATAM20.xlsx")
LATAM21 <- read_excel("Documents/UNIVERSIDAD/5 SEMESTRE/Análisis Estadístico de Datos/Proyecto/AED BD/LATAM21.xlsx")
NORTHAM19 <- read_excel("Documents/UNIVERSIDAD/5 SEMESTRE/Análisis Estadístico de Datos/Proyecto/AED BD/NA19.xlsx")
NORTHAM20 <- read_excel("Documents/UNIVERSIDAD/5 SEMESTRE/Análisis Estadístico de Datos/Proyecto/AED BD/NA20.xlsx")
NORTHAM21 <- read_excel("Documents/UNIVERSIDAD/5 SEMESTRE/Análisis Estadístico de Datos/Proyecto/AED BD/NA21.xlsx")
ASIA19 <- read_excel("Documents/UNIVERSIDAD/5 SEMESTRE/Análisis Estadístico de Datos/Proyecto/AED BD/ASIA19.xlsx")
ASIA20 <- read_excel("Documents/UNIVERSIDAD/5 SEMESTRE/Análisis Estadístico de Datos/Proyecto/AED BD/ASIA20.xlsx")
EU19 <-read_excel("Documents/UNIVERSIDAD/5 SEMESTRE/Análisis Estadístico de Datos/Proyecto/AED BD/EUROPE19.xlsx") 
EU20 <-read_excel("Documents/UNIVERSIDAD/5 SEMESTRE/Análisis Estadístico de Datos/Proyecto/AED BD/EUROPE20.xlsx") 

LATAM19 <- LATAM19[,-c(6)] 
LATAM20 <- LATAM20[,-c(6)] 
NORTHAM19 <- NORTHAM19[,-c(6)] 
NORTHAM20 <- NORTHAM20[,-c(6)] 
ASIA19 <- ASIA19[,-c(6)] 
ASIA20 <- ASIA20[,-c(6)] 
EU19 <- EU19[,-c(6)] 
EU20 <- EU20[,-c(6)] 

join <- merge(LATAM19,LATAM20, all=TRUE)
join <- merge(join,NORTHAM19, all=TRUE)
join <- merge(join,NORTHAM20, all=TRUE)
join <- merge(join,ASIA19, all=TRUE)
join <- merge(join,ASIA20, all=TRUE)
join <- merge(join,EU19, all=TRUE)
todo <- merge(join,EU20, all=TRUE)

todo[todo=="-"] <- NA
todo[todo=="..."] <- NA
todo[todo=="#N/A"] <- NA
todo[todo=="#ERROR"] <- NA
todo[todo==" "] <- NA

todo <- transform(todo, GDP = as.numeric(GDP),Unemployment = as.numeric(Unemployment),Interest.Rate = as.numeric(Interest.Rate))

#----- Regresón lineal
linear_reg1 <- lm(Mid.Price ~ Local+GDP+Unemployment+Interest.Rate+CPI+Calification, data=todo)
summary(linear_reg1)

# --- Plot
library(ggplot2)
library(MASS)
library(gplots)
library("ggpubr")
todo <- todo[todo$Mid.Price<1000,]
ggplot(todo, aes(x=GDP, y=Mid.Price)) +
  geom_point() + geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  theme_light()
hist(todo$Mid.Price)
ggboxplot(todo, x = "Mid.Price", y = "GDP", 
          color = "Local",
          ylab = "Precio", xlab = "Treatment")

# ---- cancor
library(CCA)
sinnan <- na.omit(todo)
macro <- sinnan[,c(8,9,10,11,13)]
bono <- sinnan[,c(2,4)]
cancor(bono,macro)
cor(bono,macro)
cor(todo)
cca <- cc(macro,bono)
cca$cor
ggplot(sinnan,aes(Unemployment,Cpn)) + geom_point()

############################
library(MASS)
library(gplots)

BD20 <- read.csv(file='/Users/usuario/Desktop/BD.csv',sep=";",check.names = F)
mod1 <- manova(cbind(BD20$BBG_Composite, BD20$Local)~ BD20$Continent)
mod2 <- aov(BD20$Mid_Price~BD20$BBG_Composite + BD20$Local)
mod3 <- aov(BD19$Mid_Price~BD19$BBG_Composite)
mod4 <- aov(BD19$Mid_Price~BD19$Local)
summary(mod2)
summary(mod3)
mod4 <- manova(cbind(BD20$Cpn ,BD20$Mid_Price) ~ BD20$Continent)
post_hoc1 <- lda(BD20$Continent ~ cbind(BD20$BBG_Composite , BD20$Local))
post_hoc1 <- lda(BD20$Continent ~ BD20$BBG_Composite * BD20$Local)
post_hoc1 <- lda(BD20$Continent ~ BD20$BBG_Composite )

BD19 <- read.csv(file='/Users/usuario/Desktop/BD19.csv',sep=";",check.names = F)

plotmeans(BD20$Mid_Price~BD20$BBG_Composite)
plotmeans(BD19$Mid_Price~BD19$BBG_Composite)
plotmeans(BD20$Mid_Price~BD20$Local)
plotmeans(BD19$Mid_Price~BD19$Local)

ggboxplot(BD20, x = "Local", y = "Mid_Price", 
          color = "Local",
          ylab = "Precio", xlab = "Treatment")

ggboxplot(BD20, x = "BBG_Composite", y = "Mid_Price", 
          color = "BBG_Composite",
          ylab = "Precio", xlab = "Treatment")



ggboxplot(BD19, x = "Local", y = "Mid_Price", 
          color = "Local",
          ylab = "Precio", xlab = "Treatment")

ggboxplot(BD19, x = "BBG_Composite", y = "Mid_Price", 
          color = "BBG_Composite",
          ylab = "Precio", xlab = "Treatment")
