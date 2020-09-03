rm(list = ls())
setwd("/home/ze/Área de Trabalho/Examen/BOPS_case/BOPS_case/")
#Cargando las librerías 
library(data.table)
library(dtplyr)


#Leyendo las bases de datos 
datos1 <- fread("bops_bm.csv", )
datos2 <- fread("bops_online.csv")

#Analisando la estructura de las dos bases de datos
str(datos1)
str(datos2)

#Quitando variables que sólo tienen NA's aunque algunas tenían 1 cero
datos1[,.N, by = "V8"]
datos1[,.N, by = "V9"]
datos1[,.N, by = "V10"]
datos1[,.N, by = "V11"]
datos1

datos1 <- datos1[-c(4537, 4538), -c(8:11)]

datos1[, sales := as.numeric(gsub("([0-9]+)\\,([0-9])", "\\1\\2", sales))]



datos2[,.N, by = "V8"]
datos2[,.N, by = "V9"]
datos2[,.N, by = "V10"]
datos2[,.N, by = "V11"]
datos2[,.N, by = "V12"]
datos2[,.N, by = "V13"]
datos2[,.N, by = "V14"]
datos2[,.N, by = "V15"]
datos2[,.N, by = "V16"]
datos2[,.N, by = "V17"]

datos2 <- datos2[, -c(8:17)]
datos2[, sales := as.numeric(gsub("([0-9]+)\\,([0-9])", "\\1\\2", sales))]

datos1[, sum(sales), by = "after"]
datos2[, sum(sales), by = "after"]

datos1[, mean(sales), by = "after"]
datos2[, mean(sales), by = "after"]

datos1[,after1 := ifelse(week >= 43, 1, 0)]
datos2[,after1 := ifelse(week >= 43, 1, 0)]

datos1[, mean(sales), by = "after1"]
datos1[, mean(sales), by = "after1"][2,] - datos1[, mean(sales), by = "after1"][1,]

datos2[, mean(sales), by = "after1"]
datos2[, mean(sales), by = "after1"][2,] - datos2[, mean(sales), by = "after1"][1,]


datos1[usa == 0]
datos2[, usa := ifelse(`id (DMA)` <= 165, 0, 1)]
datos1[,.N, by= "usa"] 
datos2[, .N, by = c("close", "usa")]

datos1[, sum(sales), by = c("week", "id (store)")]
datos1[,.N, "id (store)"]

datos1[, mean(sales), by = c("after", "usa")]
datos2[, mean(sales), by = c("after","usa")]

datos1 <- as_tibble(datos1) %>% rename(id = "id (store)")
datos2 <- as_tibble(datos2) %>% rename(id = "id (DMA)")

lista <- list(datos1, datos2) 
datos <- rbindlist(lista, fill = T)

datos[usa == 0 & !is.na(close), mean(sales), by = "id"][order(V1)]
datos[usa == 0 & is.na(close), mean(sales), by = "id"][order(V1)]

datos[usa == 0 & close == 0, mean(sales), by = "id"][order(V1)]
datos[usa == 0 & close == 1, mean(sales), by = "id"][order(V1)]




#####################################################################

datos[, mean(sales), by = c("week", "after")][order(week)]

datos[, mean(sales), by = c("week", "after", "usa")][order(week)] %>% ggplot(aes(x = week, y = V1)) + geom_line() + geom_vline(xintercept = 43) 
datos[, mean(sales), by = c("week", "after", "usa")][order(week)] %>% ggplot(aes(x = week, y = V1, col = factor(usa))) + geom_line() + geom_vline(xintercept = 43)
datos[, mean(sales), by = c("week", "after",  "close")][order(week)] %>% ggplot(aes(x = week, y = V1, col = factor(is.na(close)))) + geom_line() + geom_vline(xintercept = 43)

datos[, mean(sales), by = c("week", "after", "usa")][order(week)]

datos[is.na(close), sum(sales), by = c("week", "after")][order(week)]



serie <- datos[, sum(sales), by = c("week", "after")][order(week)] 
serie <- ts(serie$V1)


acf(serie)
pacf(serie)
x <- c(rep(0, 43), rep(1, 11))

serie1 <- datos[is.na(close), mean(sales), by = c("week")][order(week)] 
serie2 <- datos[!is.na(close), mean(sales), by = c("week")][order(week)]
serie <- merge(serie1, serie2, all.x = T, all.y = F, by.x = "week", by.y = "week")

cor(serie[,-1], use = "complete.obs")

