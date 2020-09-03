#Se lee las librerías que voy a utilizar acá
library(data.table)
library(ggmap)
library(rjson)
library(purrr)
library(rgdal)
library(maptools)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(ggthemes)
library(jsonlite)
library(viridis)
library(scales)
library(geoR)
library(tmap)
library(lubridate)
library(geoRglm)
library(dplyr)
library(sf)
library(foreign)
rm(list = ls())
#Lectura de los datos 
setwd("/home/ze/Área de Trabalho/Examen/")
datos <- fread("carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico.csv")

mapa <- readOGR(dsn = "09a.shp")
plot(mapa)

personas <- data.table(read.dbf("RESAGEBURB_09DBF10.dbf"))
pob <- personas[,sum(as.numeric(POBTOT)), by = "AGEB"] %>% rename(pob = V1)

str(datos)


datos <- datos[ao_hechos >= 2016]

datos[,.N, "ao_hechos"][order(ao_hechos)]
datos[,.N, "mes_hechos"]

datos[,dia := ifelse(is.na(as.numeric(substr(datos$fecha_hechos, 1, 4))), substr(datos$fecha_hechos, 1, 2), substr(datos$fecha_hechos, 9, 10))]

datos[, fecha := ymd(paste0(ao_hechos, mes_hechos, dia))]
#1)
datos[,.N, "fecha"][order(fecha)] 

#2)
datos[,.N, "delito"][order(N, decreasing = T)]

#3)
datos <- datos[alcaldia_hechos %in% datos[,.N, "alcaldia_hechos"][order(N, decreasing = T)][N > 2500]$alcaldia_hechos]
#Dejando sólo las alcaldías de la CdMx

datos[,.N, "alcaldia_hechos"][order(N, decreasing = T)]

datos[,.N, by = c("delito", "ao_hechos")]
tabla <- datos[,.N, by = c("delito", "ao_hechos")] %>% dcast(delito ~ ao_hechos, value.var = "N")
tabla

#4)
delitos_anno <- data.table(tabla[,1], dif_rel_2018 = tabla[,`2019` - `2018`]/tabla[,`2018`], dif_rel_2017 = tabla[,`2019` - `2017`]/tabla[,`2017`], delitos_2017 = tabla[,`2017`], delitos_2018 = tabla[,`2018`], delitos_2019 = tabla[,`2019`])
delitos_anno[order(dif_rel_2017)]
delitos_anno[order(dif_rel_2018)]
delitos_anno[delitos_2018 >= 500 | delitos_2019 >= 500][order(dif_rel_2017)]
delitos_anno[delitos_2018 >= 500 | delitos_2019 >= 500][order(dif_rel_2017, decreasing = T)]

#5)
datos[,.N, by = "alcaldia_hechos"][order(N)]

#6)
#Las 3 colonias con más delitos desde 2016 
datos %>% group_by(alcaldia_hechos, colonia_hechos) %>% summarise(n = n()) %>% top_n(n = 3)

#7)
#Se puede ver el poder de la pandemia en los delitos en la CdMx
datos[,.N, "fecha"][order(fecha)] %>% ggplot(aes(x = fecha, y = N)) + geom_line()

datos[,semana := weekdays(datos$fecha)]
datos[, .N, by = "dia"][order(dia)]
datos[ao_hechos != "2020", .N, "semana"]

serie <- datos[,.N, "fecha"][order(fecha)]
serie <- ts(serie$N)

acf(serie)
pacf(serie)

datos[ao_hechos != "2020", .N, by = "dia"][order(dia)]

datos[ao_hechos != "2020",.N, "mes_hechos"][order(mes_hechos)] 


#8)
tabla <- datos[,.N, by = c("delito", "alcaldia_hechos")] %>% dcast(delito ~ alcaldia_hechos, value.var = "N", margins = T)

apply(tabla[,-1], 2, which.max)

#Benito Juarez, Robo a Negocio - Cuahutemóc, Fraude - MH, Robo de Objetos 



#9)
dados_crimenes <- datos[(delito %like% "HOMI") & !(delito %like% "CULPOSO"), .N, by = c("geopoint", "longitud", "latitud", "delito")]
dados_crimenes <- dados_crimenes[!is.na(latitud) | !is.na(longitud)]


crimes <- SpatialPointsDataFrame(coords = data.frame(Long = dados_crimenes$longitud, Lat = dados_crimenes$latitud), data = data.frame(dados_crimenes$delito), proj4string = CRS(proj4string(mapa)))

mapa <- SpatialPolygonsDataFrame(mapa, data = data.frame(mapa@data))
mapa <- spTransform(mapa, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
crimes <- spTransform(crimes, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

res <- over(crimes, mapa)

res <- data.table(res)[,.N, by = "CVE_AGEB"]

mapa@data <- merge(mapa@data, res, by = "CVE_AGEB", all.x = T, all.y = F)
mapa@data <- merge(mapa@data, pob, by.x = "CVE_AGEB", by.y = "AGEB", all.x = T, all.y = F)
mapa@data$N <- ifelse(is.na(mapa@data$N) , 0, mapa@data$N)
mapa@data$pob <- ifelse(is.na(mapa@data$pob) , 0, mapa@data$pob)
mapa@data$tasa <- 100000*mapa@data$N/mapa@data$pob


#11)

#Son outliers que estaban causando ruído. En términos de los mapas, esté tipo de cambio no afecta
mapa@data$tasa[782] <-130
mapa@data$tasa[2318] <- 130
mapa@data$tasa[388] <- 130
mapa@data$tasa[167] <- 130
mapa@data$tasa[184] <- 130
mapa@data$tasa[160] <- 130
mapa@data$tasa[1261] <- 130
mapa@data$tasa[263] <- 130
mapa@data$tasa[709] <- 130
tm_shape(mapa) + tm_fill(col = "tasa") + tm_borders()
mapa@data$tasa <- 100000*mapa@data$N/mapa@data$pob

dados_crimenes <- datos[, .N, by = c("geopoint", "longitud", "latitud", "categoria_delito", "alcaldia_hechos", "colonia_hechos")]
dados_crimenes <- dados_crimenes[!is.na(latitud) | !is.na(longitud)]
crimes <- SpatialPointsDataFrame(coords = data.frame(Long = dados_crimenes$longitud, Lat = dados_crimenes$latitud), data = data.frame(dados_crimenes$categoria_delito), proj4string = CRS(proj4string(mapa)))
mapa <- SpatialPolygonsDataFrame(mapa, data = data.frame(mapa@data))
mapa <- spTransform(mapa, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
crimes <- spTransform(crimes, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

res <- over(crimes, mapa)
res <- data.table(res, delito = dados_crimenes$categoria_delito, colonia = dados_crimenes$colonia_hechos, alcaldia = dados_crimenes$alcaldia_hechos)
res[,.N, by = "delito"]
res[delito == "ROBO DE VEHÃ\u008dCULO CON Y SIN VIOLENCIA",]$delito <- "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA"
res[delito == "ROBO A TRANSEUNTE EN VÃ\u008dA PÃšBLICA CON Y SIN VIOLENCIA",]$delito <- "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA"
res[delito == "VIOLACIÃ“N",]$delito <- "VIOLACIÓN"
res[delito == "ROBO A CASA HABITACIÃ“N CON VIOLENCIA",]$delito <- "ROBO A CASA HABITACIÓN CON VIOLENCIA"

res1 <- data.table(res)[,.N, by = c("CVE_AGEB", "delito")]

tabla <- res1 %>% dcast(CVE_AGEB ~ delito, value.var = "N")
tabla1 <- apply(tabla[,-1], 2, f <- function(x){ifelse(is.na(x), 0, x)})
tabla <- data.table(ageb = tabla[,1], tabla1)
tabla <- unique(merge(tabla, res[,c(1,6,7)], by.x = "ageb.CVE_AGEB", by.y = "CVE_AGEB", all.x = F, all.y = F))



for(i in 1:10){
  analise <- kmeans(tabla[-1,-c(1, 18,19)], centers = i)
  tabela[i] <- analise$betweenss/analise$totss
}
plot(ts(tabela))
rm(tabela)

analise <- kmeans(tabla[-1,-c(1,18)], centers = 5)
tabla <- data.table(tabla, cluster = analise$cluster)



#12)
dados_crimenes <- datos[(delito %like% "ROBO A PASAJERO A BORDO"), ]
dados_crimenes[,.N, c("calle_hechos")][order(N, decreasing = T)]
dados_crimenes[,.N, c("calle_hechos", "colonia_hechos")][order(N, decreasing = T)]

