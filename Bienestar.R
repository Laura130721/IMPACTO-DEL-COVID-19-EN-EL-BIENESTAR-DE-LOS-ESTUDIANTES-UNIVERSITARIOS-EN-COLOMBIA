#### Instalar paquetes ####

install.packages("readx1") 
install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("stargazer")
install.packages("pscl")
install.packages("mfx")
installed.packages("betareg")
install.packages("carData")
install.packages("car")
install.packages("stats")
install.packages("tidyverse")
install.packages("data.table")
install.packages("caret")
install.packages("zoo")
install.packages("lmtest")
install.packages("htest")
install.packages("MASS")
install.packages("aod")
install.packages("nortest")
install.packages("foreign")
install.packages("sandwich")
install.packages("multiwayvcov")
install.packages("margins")
install.packages("ggplot2")

#### Lectura de los paquetes ####

library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(stargazer)
library(pscl)
library(mfx, betareg)
library(carData)
library(car)
library(stats)
library(tidyverse)
library(data.table)
library(caret)
library(zoo)
library(lmtest)
#library(htest)
library(MASS)
library(aod)
library(nortest)
library(foreign)
library(sandwich)
library(multiwayvcov)
library(margins)
library(ggplot2)

set.seed(123)
#### Cargamos las bases de datos ####

encuesta_1 <- read_excel("Documents/1era encuesta COVID.xlsx")
view(encuesta_1)
                                       
encuesta_2 <- read_excel("Documents/2da encuesta COVID.xlsx")
view(encuesta_2)

encuesta_1 <- `colnames<-`(encuesta_1, c("Fecha", "Edad", "Genero", "Universidad",
                                         "Departamento", "Vive_solo", "Peso",
                                         "Estatura","Masa_Corporal", "Estrato", "Numero_comidas",
                                         "Horas_sueño", "Domicilios", "Alcohol_dia",
                                         "Ejercicio", "Bienestar", "Comidas_antes",
                                         "Sueño_antes", "Domicilios_antes", "Alcohol_antes",
                                         "Ejercicio_antes", "Bienestar_antes"))
view(encuesta_1)

encuesta_2 <- `colnames<-`(encuesta_2, c("Fecha","Edad", "Genero","Universidad",
                                         "Departamento", "Vive_solo", "Peso",
                                         "Estatura","Masa_Corporal", "Estrato", "Numero_comidas",
                                         "Horas_sueño", "Domicilios", "Alcohol_dia",
                                         "Ejercicio", "Bienestar"))
view(encuesta_2)


#### Correción de datos ####

e_1 <- encuesta_1
e_1 <- as.data.frame(e_1)
e_1 <- e_1[,-18:-21]
e_1 <- e_1[,-17:-18]
e_2 <- encuesta_2
e_2 <- e_2[,]
e_2 <- as.data.frame(e_2)


#### Para unir las bases de datos ####

encuesta_total <- rbind(e_1,e_2)
View(encuesta_total)
summary(encuesta_total)

encuesta_total$etapa_2 <- encuesta_total$Fecha < "2020-03-28" #2020-03-28 TRUE
encuesta_total$Hombres <- encuesta_total$Genero > "Hombre" ##Verdadero Hombre
encuesta_total$Estrato <- encuesta_total$Estrato == 3
encuesta_total$Bogota <- encuesta_total$Departamento == "Bogotá D.C."
encuesta_total$Vive_solo <- encuesta_total$Vive_solo == "Sí"

str(encuesta_total)
encuesta_total$Peso <- as.numeric(encuesta_total$Peso)
encuesta_total$Estatura <- as.numeric(encuesta_total$Estatura)

names(encuesta_total)
#### Regresiones ####

comidas <- lm(Numero_comidas~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
              data =subset(encuesta_total,c(etapa_2==TRUE, Genero==TRUE,  Vive_solo==TRUE)))
comidas_total <- lm(Numero_comidas~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
              data = encuesta_total)
qplot(data = encuesta_total,
      x= Numero_comidas, 
      geom = "histogram",
      fill= etapa_2, 
      main= "Numero de comidas promedio por dia")


sueño <- lm(Horas_sueño~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
              data =subset(encuesta_total,c(etapa_2==TRUE, Genero==TRUE, Vive_solo==TRUE)))
sueño_total <- lm(Horas_sueño~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
            data =encuesta_total)
qplot(data = encuesta_total,
      x= Horas_sueño, 
      geom = "histogram",
      fill= etapa_2, 
      main= "Numero de horas de sueño en promedio por dia")


Domicilios <- lm(Domicilios~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
              data =subset(encuesta_total,c(etapa_2==TRUE, Genero==TRUE,  Vive_solo==TRUE)))
Domicilios_total <- lm(Domicilios~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
                 data =encuesta_total)


Alcohol_dia <- lm(Alcohol_dia~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
              data =subset(encuesta_total,c(etapa_2==TRUE, Genero==TRUE, Vive_solo==TRUE)))
Alcohol_dia_total <- lm(Alcohol_dia~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
                  data =encuesta_total)


Ejercicio <- lm(Ejercicio~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
              data =subset(encuesta_total,c(etapa_2==TRUE, Genero==TRUE, Vive_solo==TRUE)))
Ejercicio_total <- lm(Ejercicio~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
                data =encuesta_total)


Bienestar <- lm(Bienestar~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
                data =subset(encuesta_total,c(etapa_2==TRUE, Genero==TRUE, Vive_solo==TRUE)))

Bienestar_total <- lm(Bienestar~Hombres+ Bogota+ Vive_solo+ Masa_Corporal+ Estrato+ etapa_2,
                      data =encuesta_total)

stargazer(comidas,comidas_total,type="text")
stargazer(sueño, sueño_total, type= "text")
stargazer(Domicilios, Domicilios_total, type= "text")
stargazer(Alcohol_dia, Alcohol_dia_total, type= "text")
stargazer(Ejercicio, Ejercicio_total, type= "text")
stargazer(Bienestar, Bienestar_total, type= "text")

stargazer(comidas_total,sueño_total,Domicilios_total,
          Alcohol_dia_total,Ejercicio_total,Bienestar_total, type= "text")

# Correcion de errores ----------------------------------------------------
resettest(comidas_total) #Es significativo
resettest(sueño_total) #Es siginificativo
resettest(Domicilios_total) #Es significativo
resettest(Alcohol_dia_total) #Es significativo
resettest(Ejercicio_total) #Es significativo
resettest(Bienestar_total) #No es significativo


shapiro.test(comidas_total$residuals) #no es normal
shapiro.test(sueño_total$residuals) #no es normal
shapiro.test(Domicilios_total$residuals) #no es normal
shapiro.test(Alcohol_dia_total$residuals)#no es normal
shapiro.test(Ejercicio_total$residuals)#no es normal
shapiro.test(Bienestar_total$residuals)#no es normal

hist(comidas_total$residuals) #grafico

bptest(comidas_total) #se rechaza homoscedasticidad
bptest(sueño_total)#se rechaza homoscedasticidad
bptest(Domicilios_total) #no se rechaza 
bptest(Alcohol_dia_total)#se rechaza homoscedasticidad
bptest(Ejercicio_total)# no se rechaza
bptest(Bienestar_total) #no se rechaza


#robustecer por white los que no son homoscedasticos


rob_comidas =coeftest(comidas_total, vcov = vcovHC(comidas_total, "HC1")) # Robusteciendo_errores

rob_sueño =coeftest(sueño_total, vcov = vcovHC(sueño_total, "HC1")) # Robusteciendo_errores

rob_alcohol =coeftest(Alcohol_dia_total, vcov = vcovHC(Alcohol_dia_total, "HC1")) # Robusteciendo_errores

stargazer(rob_comidas,rob_sueño, Domicilios_total, rob_alcohol, Ejercicio_total, Bienestar_total,
          column.labels= c("Comida", "sueño", "", "Alcohol","",""))
