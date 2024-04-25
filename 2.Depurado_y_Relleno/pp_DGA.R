
directorio_base = getwd()

source(paste0(directorio_base, "/2.Depurado_y_Relleno/funciones.R"))

#AGREGAR LUEGO COMO PARÁMETROS DE FUNCIÓN
tolerancia_km = 50

pp_DGA = read.csv(paste0(directorio_base, "/BBDD/pp/DGA/bruto/pp_DGA_2023_2024_3.csv"))
metadatos_pp = read.csv(paste0(directorio_base, "/BBDD/metadatos/DGA/pp_y_temp/estaciones_DGA_pp_y_temp.csv"))
head(metadatos_pp)

#marcar outliers
#pp_DGA$pp_day[pp_DGA$pp_day < 0 | pp_DGA$pp_day >= 800] <- NA

#borrar entradas invalidas, asi quitar estaciones inactivas
pp_DGA <- drop_na(pp_DGA)

#eliminar duplicados
pp_DGA <- unique(pp_DGA)


#contamos la cantidad de mediciciones para cada estacion en el periodo de tiempo
count_estaciones <- pp_DGA %>% group_by(Codigo_nacional) %>% summarise(n = n()) %>% arrange(desc(n))

#OJO AL FUTURO PARA AUTOMATIZAR ESTO
#obtenemos fechas del archivo
fechas_archivo <- obtener_fechas_archivo(paste0(directorio_base, "/BBDD/pp/DGA/bruto/pp_DGA_2023_2024_3.csv"))

#calculamos el mínimo número de datos que debería tener una estación según las fechas del archivo. se fija una completitud del 70%
n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin*30) * 0.7
#quitamos estaciones que tienen menos del 70% de mediciones
estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
pp_DGA_validas <- pp_DGA %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)

summary(pp_DGA_validas)


#rellenamos las fechas faltantes para cada estación, dejando todos los valores de pp como NA
pp_DGA_relleno <- rellenar_fechas(pp_DGA_validas)
pp_DGA_relleno <- pp_DGA_relleno %>%
  select(Year, Month, Day, Codigo_nacional, pp_day)

summary(pp_DGA_relleno)

 #   pp_day
 #Min.   :  0.000
 #1st Qu.:  0.000
 #Median :  0.000
 #Mean   :  2.185
 #3rd Qu.:  0.100
 #Max.   :856.000
 #NA's   :9539
#


######COMIENZA RELLENADO DE DATOS FALTANTES######

#PASO 1: Imputacion de vecinos cercanos
#creamos antes del bucle un dataframe que indica para cada estacion cual es la mas cercana segun la tolerancia definida
#asi evitamos ejecutar dicho calculo en cada iteracion del bucle
tuplas_estaciones <- data.frame(Codigo_nacional = unique(pp_DGA_relleno$Codigo_nacional), mas_cercana = NA)
tuplas_estaciones$mas_cercana <- sapply(tuplas_estaciones$Codigo_nacional, function(x) estacion_mas_cercana_DGA(x, metadatos_pp, tolerancia_km))


#Imputacion de vecinos cercanos
for(i in 1:nrow(pp_DGA_relleno)){
    if(is.na(pp_DGA_relleno$pp_day[i])){
        registro <- pp_DGA_relleno[i,]
        estacion_mas_cercana <- tuplas_estaciones %>% filter (Codigo_nacional == registro$Codigo_nacional)
        Year <- registro$Year
        Month <- registro$Month
        Day <- registro$Day
        imputacion_pp <- obtener_valor(pp_DGA_relleno, estacion_mas_cercana$mas_cercana, Year, Month, Day, "pp_day")
        pp_DGA_relleno$pp_day[i] <- imputacion_pp
    }
}
#end_time = Sys.time()
#time_running = end_time - start_time
#print("TIEMPO EJECUCION:")
#print(time_running)

summary(pp_DGA_relleno)
# NA's   :2991  
#PASO 2: rellenamos los valores faltantes con la mediana mensual de la estacion
pp_DGA_relleno <- pp_DGA_relleno %>%
  group_by(Codigo_nacional, Year, Month) %>%
  mutate(
    pp_day = ifelse(is.na(pp_day), ave(pp_day, FUN = function(x) median(x, na.rm = TRUE)), pp_day)
  ) %>%
  ungroup()

#1483 NA's
# PASO 3: ahora rellenamos los valores faltantes con la mediana anual de la estacion
pp_DGA_relleno <- pp_DGA_relleno %>%
  group_by(Codigo_nacional, Year) %>%
  mutate(
    pp_day = ifelse(is.na(pp_day), median(pp_day, na.rm = TRUE), pp_day)
  ) %>%
  ungroup()

# 0 NA's
#PASO 4: Rellenar la mediana global del año, por seguridad si algun existiese algun valor NA
pp_DGA_relleno <- pp_DGA_relleno %>%
  group_by(Year) %>%
  mutate(
    pp_day = ifelse(is.na(pp_day), median(pp_day, na.rm = TRUE), pp_day)
  ) %>%
  ungroup()





write.csv(pp_DGA_relleno, paste0(directorio_base, "/BBDD/pp/DGA/depurado/pp_DGA_2023_2024_3.csv"), row.names = FALSE)
