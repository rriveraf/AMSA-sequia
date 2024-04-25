


directorio_base = getwd()

source(paste0(directorio_base, "/2.Depurado_y_Relleno/funciones.R"))

tolerancia_km = 50

#carga de archivos
##ESTO LUEGO QUE PASE COMO PARAMETROS DE FUNCIÓN	
temp_DGA_min = read.csv(paste0(directorio_base, "/BBDD/temp/DGA/min/temp_DGA_min_2023_2024_3.csv"))
temp_DGA_max = read.csv(paste0(directorio_base, "/BBDD/temp/DGA/max/temp_DGA_max_2023_2024_3.csv"))
temp_DGA_mean = read.csv(paste0(directorio_base, "/BBDD/temp/DGA/mean/temp_DGA_mean_2023_2024_3.csv"))
metadatos_temp = read.csv(paste0(directorio_base, "/BBDD/metadatos/DGA/pp_y_temp/estaciones_DGA_pp_y_temp.csv"))

#juntar todo en un archivo
temp_DGA <- temp_DGA_min
temp_DGA$temp_mean <- temp_DGA_mean$temp_mean
temp_DGA$temp_max <- temp_DGA_max$temp_max

summary(temp_DGA)
#    temp_min         temp_mean         temp_max
# Min.   :-50.000   Min.   :-49.99   Min.   :-49.90
# 1st Qu.:  3.310   1st Qu.:  8.30   1st Qu.: 13.32
# Median :  7.520   Median : 12.99   Median : 19.40
# Mean   :  6.809   Mean   : 12.54   Mean   : 19.28  
# 3rd Qu.: 11.130   3rd Qu.: 17.74   3rd Qu.: 26.16
# Max.   : 48.000   Max.   : 48.00   Max.   : 50.00

#marcar outliers
temp_DGA$temp_min[temp_DGA$temp_min <= -20 | temp_DGA$temp_min >= 45] <- NA
temp_DGA$temp_max[temp_DGA$temp_max <= -20 | temp_DGA$temp_max >= 45] <- NA
temp_DGA$temp_mean[temp_DGA$temp_mean <= -20 | temp_DGA$temp_mean >= 45] <- NA

summary(temp_DGA)

#    temp_min         temp_mean          temp_max     
# Min.   :-19.900   Min.   :-19.945   Min.   :-19.90
# 1st Qu.:  3.400   1st Qu.:  8.361   1st Qu.: 13.40
# Median :  7.590   Median : 13.021   Median : 19.46
# Mean   :  7.002   Mean   : 12.703   Mean   : 19.42
# 3rd Qu.: 11.170   3rd Qu.: 17.760   3rd Qu.: 26.15
# Max.   : 44.800   Max.   : 44.857   Max.   : 44.99
# NA's   :1093      NA's   :877       NA's   :1011
#identificar estaciones que tienen mas 

#eliminamos valores na, para asi quitar estaciones invalidas
temp_DGA <- drop_na(temp_DGA)

#eliminar duplicados
temp_DGA <- unique(temp_DGA)

#contamos la cantidad de mediciciones para cada estacion en el periodo de tiempo
count_estaciones <- temp_DGA  %>% group_by(Codigo_nacional) %>% summarise (n = n())

#OJO AL FUTURO PARA AUTOMATIZAR ESTO
#obtenemos fechas del archivo
fechas_archivo <- obtener_fechas_archivo(paste0(directorio_base, "/BBDD/temp/DGA/min/temp_DGA_min_2023_2024_3.csv"))

#calculamos el mínimo número de datos que debería tener una estación según las fechas del archivo. se fija una completitud del 70%
n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin*30) * 0.7

#quitamos estaciones que tienen menos del 70% de mediciones
estaciones_validas = count_estaciones %>% filter(n >=n_minimo_datos)
temp_DGA_validas <- temp_DGA %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)
head(temp_DGA_validas)
summary(temp_DGA_validas)

#      Year          Month             Day        Codigo_nacional   
 #Min.   :2023   Min.   : 1.0   Min.   : 1.00   Length:244636
 #1st Qu.:2023   1st Qu.: 2.0   1st Qu.: 8.00   Class :character
 #Median :2023   Median : 5.0   Median :16.00   Mode  :character
 #Mean   :2023   Mean   : 5.7   Mean   :15.74
 #3rd Qu.:2023   3rd Qu.: 9.0   3rd Qu.:23.00
 #Max.   :2024   Max.   :12.0   Max.   :31.00
 #   temp_min        temp_mean          temp_max
 #Min.   :-19.36   Min.   :-18.521   Min.   :-18.50
 #1st Qu.:  3.50   1st Qu.:  8.442   1st Qu.: 13.50
 #Median :  7.60   Median : 13.065   Median : 19.50
 #Mean   :  7.04   Mean   : 12.761   Mean   : 19.52  
 #3rd Qu.: 11.12   3rd Qu.: 17.753   3rd Qu.: 26.20
 #Max.   : 31.30   Max.   : 35.831   Max.   : 44.99

#rellenamos las fechas faltantes para cada estación, dejando todos los valores de T° como NA
temp_DGA_relleno <- rellenar_fechas(temp_DGA_validas)

#ordenar columnas
temp_DGA_relleno <- temp_DGA_relleno %>%
  select(Year, Month, Day, Codigo_nacional, temp_min, temp_mean, temp_max)



summary(temp_DGA_relleno)

# NA's   :9812   
# 251712 registros

############ COMIENZA RELLENO DE DATOS FALTANTES #########

#PASO 1: Imputacion de vecinos cercanos
#creamos antes del bucle un dataframe que indica para cada estacion cual es la mas cercana segun la tolerancia definida
#asi evitamos ejecutar dicho calculo en cada iteracion del bucle
tuplas_estaciones <- data.frame(Codigo_nacional = unique(temp_DGA_relleno$Codigo_nacional), mas_cercana = NA)
tuplas_estaciones$mas_cercana <- sapply(tuplas_estaciones$Codigo_nacional, function(x) estacion_mas_cercana_DGA(x, metadatos_temp, tolerancia_km))

#start_time = Sys.time()
#Imputacion de vecinos cercanos
for(i in 1:nrow(temp_DGA_relleno)){
  if(is.na(temp_DGA_relleno$temp_min[i])){
    registro <- temp_DGA_relleno[i,]
    estacion_mas_cercana <- tuplas_estaciones %>% filter(Codigo_nacional == registro$Codigo_nacional)
    Year <- registro$Year
    Month <- registro$Month
    Day <- registro$Day
    imputacion_temp_min <- obtener_valor(temp_DGA_relleno, estacion_mas_cercana$mas_cercana, Year, Month, Day, "temp_min")
    imputacion_temp_mean <- obtener_valor(temp_DGA_relleno, estacion_mas_cercana$mas_cercana, Year, Month, Day, "temp_mean")
    imputacion_temp_max <- obtener_valor(temp_DGA_relleno, estacion_mas_cercana$mas_cercana, Year, Month, Day, "temp_max")
    temp_DGA_relleno$temp_min[i] <- imputacion_temp_min
    temp_DGA_relleno$temp_mean[i] <- imputacion_temp_mean
    temp_DGA_relleno$temp_max[i] <- imputacion_temp_max
  }
}
#end_time = Sys.time()
#time_running = end_time - start_time
#print("TIEMPO EJECUCION:")  
#print(time_running)

summary(temp_DGA_relleno)
# NA's   :4925


#PASO 2: Rellenar con el promedio mensual de la estación
temp_DGA_relleno <- temp_DGA_relleno %>%
  group_by(Codigo_nacional, Year, Month) %>%
  mutate(
    temp_min = ifelse(is.na(temp_min), ave(temp_min, FUN = function(x) mean(x, na.rm = TRUE)), temp_min),
    temp_mean = ifelse(is.na(temp_mean), ave(temp_mean, FUN = function(x) mean(x, na.rm = TRUE)), temp_mean),
    temp_max = ifelse(is.na(temp_max), ave(temp_max, FUN = function(x) mean(x, na.rm = TRUE)), temp_max)
  ) %>%
  ungroup() 

summary(temp_DGA_relleno)
# 2620 NA's

# PASO 3: Rellenar con el promedio anual de la estación
# Rellenar NA con el promedio anual para cada estación, esto es para los casos en que la estación no tiene datos en un mes completo
# en general esto ocurre con fechas mas antiguas, y no es el caso de los ultimos meses ya que en un principio se seleccionaron estaciones con mas de 20 datos en el ultimo mes  
temp_DGA_relleno <- temp_DGA_relleno %>%
  group_by(Codigo_nacional, Year) %>%
  mutate(
    temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
    temp_mean = ifelse(is.na(temp_mean), mean(temp_mean, na.rm = TRUE), temp_mean),
    temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)
  ) %>%
  ungroup()
summary(temp_DGA_relleno)
# 0 NA's

# Paso 4: Rellenar con el promedio global del año, por seguridad si algun existiese algun valor NA
temp_DGA_relleno <- temp_DGA_relleno %>%
  group_by(Year) %>%
  mutate(
    temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
    temp_mean = ifelse(is.na(temp_mean), mean(temp_mean, na.rm = TRUE), temp_mean),
    temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)
  ) %>%
  ungroup()

#corregir temp mean 
temp_DGA_relleno$temp_mean = (temp_DGA_relleno$temp_min + temp_DGA_relleno$temp_max) / 2 


summary(temp_DGA_relleno)

write.csv(temp_DGA_relleno, paste0(directorio_base, "/BBDD/temp/DGA/depurado/temp_DGA_2023_2024_3.csv"), row.names = FALSE)






