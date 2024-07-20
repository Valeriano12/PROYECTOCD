
if(!require(haven)) {install.packages("haven")
  library(haven)}

if(!require(tidyverse)) {install.packages("tidyverse")
  library(tidyverse)}

if(!require(dplyr)) {install.packages("dplyr")
  library(dplyr)}

#Importamos la base original.

ESS10SC_subset <- read_csv("ESS10SC-subset.csv")
View(ESS10SC_subset)

datos <- ESS10SC_subset

#Transformamos las variables texto en factoriales.

datos <- datos %>%
  mutate(across(where(is.character), as.factor))
View(datos)

#Codificamos variable "Género".

datos$gndr<-factor(datos$gndr, levels = c("1", "2"), labels = c("Hombre", "Mujer"))

#Codificamos variable "Nivel de estudio".

datos$eisced<-factor(datos$eisced, levels = c("1", "2", "3", "4", "5", "6", "7"), 
                     labels = c("Primaria", "Secundaria Inferior", "Secundaria Superior", 
                                "Bachiller", "Formacion Profesional", "Grado Universitario", "Master/Doctorado"))

#Clasificamos como NAs todos los valores superiores a 100 de la variable "Edad".

datos$agea[datos$agea>100]<-NA

#Codificamos la variable "País".
datos$cntry<-factor(datos$cntry, levels = c("AT", "CY", "DE", "ES", "IL", "LV", "PL", "RS","SE" ), 
                     labels = c("Austria", "Chipre", "Alemania", 
                                "España", "Israel", "Letonia", "Polonia","Serbia","Suecia"))


# Cambiamos la unidad de minutos a horas de la variable "Tiempo de Uso de Internet". 
# Y la guardamos en una nueva varible.

datos$horas_internet <- datos$netustm/60
datos$horas_internet <- round(datos$horas_internet,2)

#Establecemos como NAs aquellos valores superiores a 24 horas

datos$horas_internet[datos$horas_internet>24]<-NA

# Repetimos el proceso para la variable "Tiempo en minutos del Uso de Internet para Consultar Noticias".

datos$noticias_internet <- datos$nwspol/60
datos$noticias_internet <- round(datos$noticias_internet,2)
datos$noticias_internet[datos$noticias_internet>24]<-NA

#Establecemos los NAs para las dos VV sobre el tiempo de uso de Internet en minutos. 
#(11440 min = 24 horas).

datos$netustm[datos$netustm>1440]<-NA #"Tiempo de uso de Internet"
datos$nwspol[datos$nwspol>1440]<-NA   #"Tiempo de uso de Internet para ver Noticias"

# Para estas VV establecemos como NAs aquellos valores superiores a 10.

datos$trstprt[datos$trstprt > 10] <- NA #Confianza en los partidos políticos
datos$trstplt[datos$trstplt > 10] <- NA #Confianza en los políticos
datos$trstsci[datos$trstsci > 10] <- NA #Confianza en los científicos
datos$happy[datos$happy > 10] <- NA     # Felicidad percibida

#Estipulamos los NAs, e invertimos las puntuaciones de la variable "salud".

datos$health[datos$health > 5] <- NA

datos$health <- ifelse(datos$health == 1, 5,
                       ifelse(datos$health == 2, 4,
                              ifelse(datos$health == 3, 3,
                                     ifelse(datos$health == 4, 2, 1))))

#Creamos un subset con las variables que utilizaremos.
# Omitimos la variable $trstprt ("Coinfianza en los partidos políticos") por ser muy similar a la variable "Confianza en los políticos".

BD_depurada<-dplyr::select(datos,all_of(c("gndr","eisced", "cntry", "agea","nwspol","netustm",
                                        "horas_internet","noticias_internet", 
                                        "trstplt","trstsci","happy", "health")))
View(BD_depurada)

#Eliminamos aquellas filas que tengan más de 3 NAs

BD_depurada <- BD_depurada[rowSums(is.na(BD_depurada)) <= 3, ]

View(BD_depurada)

#Creamos la base de datos depurada en formato .csv

write.csv(BD_depurada, "BD_depurada.csv", row.names = TRUE)
