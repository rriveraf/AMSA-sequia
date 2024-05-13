
directorio_base = getwd()

source(paste0(directorio_base, "/2.Depurado_y_Relleno/funciones.R"))

tolerancia_km = 50

##carga de archivos
###ESTO LUEGO QUE PASE COMO PARAMETROS DE FUNCIÓN
temp_DMC_min = read.csv(paste0(directorio_base, "/BBDD/temp/DMC/min/temp_min_DMC_2020_2024_4.csv"))
temp_DMC_max = read.csv(paste0(directorio_base, "/BBDD/temp/DMC/max/temp_max_DMC_2020_2024_4.csv"))
metadatos_temp = read_excel(paste0(directorio_base, "/BBDD/metadatos/DMC/estaciones_DMC.xlsx"))

#juntar todo en un archivo
temp_DMC <- merge(temp_DMC_min, temp_DMC_max, by = c("Year", "Month", "Day", "Codigo_nacional"))
write.csv(temp_DMC, paste0(directorio_base, "/BBDD/temp/DMC/bruto/temp_DMC_2020_2024_4.csv"))

summary(temp_DMC)

#marcar outliers
temp_DMC$temp_min[temp_DMC$temp_min <= -20 | temp_DMC$temp_min >= 42] <- NA
temp_DMC$temp_max[temp_DMC$temp_max <= -20 | temp_DMC$temp_max >= 42] <- NA

#eliminar valores NA
temp_DMC <- drop_na(temp_DMC)
temp_DMC <- unique(temp_DMC)

#contamos la cantidad de mediciones para cada estacion en el periodo de tiempo
count_estaciones <- temp_DMC  %>% group_by(Codigo_nacional) %>% summarise (n = n())

#OJO AL FUTURO PARA AUTOMATIZAR ESTO
#obtenemos fechas del archivo
fechas_archivo <- obtener_fechas_archivo(paste0(directorio_base, "/BBDD/temp/DMC/bruto/temp_DMC_2020_2024_4.csv"))


#calculamos el mínimo número de datos que debería tener una estación según las fechas del archivo. se fija una completitud del 70%
n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin*30) * 0.7

#quitamos estaciones que tienen menos del 70% de mediciones
estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
temp_DMC_validas <- temp_DMC %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)


#rellenamos las fechas faltantes para cada estación, dejando los valores de pp como NA
temp_DMC_relleno <- rellenar_fechas(temp_DMC_validas)
temp_DMC_relleno <- temp_DMC_relleno %>%
  select(Year, Month, Day, Codigo_nacional, temp_min, temp_max)

summary(temp_DMC_relleno)

######COMIENZA RELLENADO DE DATOS FALTANTES######
#PASO 1: Imputacion de vecinos cercanos
#creamos antes del bucle un dataframe que indica para cada estacion cual es la mas cercana segun la tolerancia definida
#asi evitamos ejecutar dicho calculo en cada iteracion del bucle
tuplas_estaciones <- data.frame(Codigo_nacional = unique(temp_DMC_relleno$Codigo_nacional), mas_cercana = NA)
tuplas_estaciones$mas_cercana <- sapply(tuplas_estaciones$Codigo_nacional, function(x) estacion_mas_cercana_DGA(x, metadatos_temp, tolerancia_km))

#Imputacion de vecinos cercanos
for(i in 1:nrow(temp_DMC_relleno)){
    if(is.na(temp_DMC_relleno$temp_min[i])){
        registro <- temp_DMC_relleno[i,]
        estacion_mas_cercana <- tuplas_estaciones$mas_cercana[tuplas_estaciones$Codigo_nacional == registro$Codigo_nacional]
        temp_DMC_relleno$temp_min[i] <- obtener_valor(temp_DMC_relleno, estacion_mas_cercana, registro$Year, registro$Month, registro$Day, "temp_min")
    }
    if(is.na(temp_DMC_relleno$temp_max[i])){
        registro <- temp_DMC_relleno[i,]
        estacion_mas_cercana <- tuplas_estaciones$mas_cercana[tuplas_estaciones$Codigo_nacional == registro$Codigo_nacional]
        temp_DMC_relleno$temp_max[i] <- obtener_valor(temp_DMC_relleno, estacion_mas_cercana, registro$Year, registro$Month, registro$Day, "temp_max")
    }
}

summary(temp_DMC_relleno)

#PASO 2: Imputacion de media mensual de la estación
temp_DMC_relleno <- temp_DMC_relleno %>%
  group_by(Codigo_nacional, Year, Month) %>%
  mutate(temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
         temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)) %>%
  ungroup()

summary(temp_DMC_relleno)

#PASO 3: rellenar con promedio anual de la estacion
temp_DMC_relleno <- temp_DMC_relleno %>%
  group_by(Codigo_nacional, Year) %>%
  mutate(temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
         temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)) %>%
  ungroup()

summary(temp_DMC_relleno)

#paso 4: rellenar con promedio general, por seguridad si exisgte algun valor NA

temp_DMC_relleno <- temp_DMC_relleno %>% 
    group_by(Year) %>%
    mutate(temp_min = ifelse(is.na(temp_min), mean(temp_min, na.rm = TRUE), temp_min),
           temp_max = ifelse(is.na(temp_max), mean(temp_max, na.rm = TRUE), temp_max)) %>%
    ungroup()

summary(temp_DMC_relleno)

write.csv(temp_DMC_relleno, paste0(directorio_base, "/BBDD/temp/DMC/depurado/temp_DMC_2020_2024_4.csv") )
