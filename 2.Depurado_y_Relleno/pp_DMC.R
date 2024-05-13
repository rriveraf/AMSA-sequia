
directorio_base = getwd()

source(paste0(directorio_base, "/2.Depurado_y_Relleno/funciones.R"))

#AGREGAR LUEGO COMO PARÁMETROS DE FUNCIÓN
tolerancia_km = 50

pp_DMC = read.csv(paste0(directorio_base, "/BBDD/pp/DMC/bruto/pp_DMC_2020_2024_4.csv"))
metadatos_pp_dmc = read_excel(paste0(directorio_base, "/BBDD/metadatos/DMC/estaciones_DMC.xlsx"))

#marcar outliers
pp_DMC$pp_day[pp_DMC$pp_day < 0 | pp_DMC$pp_day > 700] <- NA

head(pp_DMC)
#borrar entradas invalidas, asi quitar estaciones inactivas
pp_DMC <- drop_na(pp_DMC)


#eliminar duplicados
pp_DMC <- unique(pp_DMC)

#contamos la cantidad de mediciciones para cada estacion en el periodo de tiempo
count_estaciones <- pp_DMC %>% group_by(Codigo_nacional) %>% summarise(n = n()) %>% arrange(desc(n))

#OJO AL FUTURO PARA AUTOMATIZAR ESTO
#obtenemos fechas del archivo
fechas_archivo <- obtener_fechas_archivo(paste0(directorio_base, "/BBDD/pp/DMC/bruto/pp_DMC_2020_2024_4.csv"))

#calculamos el mínimo número de datos que debería tener una estación según las fechas del archivo. se fija una completitud del 70%
n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin*30) * 0.7

#quitamos estaciones que tienen menos del 70% de mediciones
estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
pp_DMC_validas <- pp_DMC %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)


#rellenamos las fechas faltantes para cada estación, dejando los valores de pp como NA
pp_DMC_relleno <- rellenar_fechas(pp_DMC_validas)
pp_DMC_relleno <- pp_DMC_relleno %>%
  select(Year, Month, Day, Codigo_nacional, pp_day)

summary(pp_DMC_relleno)
#159600 registros
# 11270 NA's

######COMIENZA RELLENADO DE DATOS FALTANTES######
#PASO 1: Imputacion de vecinos cercanos
#creamos antes del bucle un dataframe que indica para cada estacion cual es la mas cercana segun la tolerancia definida
#asi evitamos ejecutar dicho calculo en cada iteracion del bucle
tuplas_estaciones <- data.frame(Codigo_nacional = unique(pp_DMC_relleno$Codigo_nacional), mas_cercana = NA)
tuplas_estaciones$mas_cercana <- sapply(tuplas_estaciones$Codigo_nacional, function(x) estacion_mas_cercana_DGA(x, metadatos_pp_dmc, tolerancia_km))

#Imputacion de vecinos cercanos
for(i in 1:nrow(pp_DMC_relleno)){
    if(is.na(pp_DMC_relleno$pp_day[i])){
        registro <- pp_DMC_relleno[i,]
        estacion_mas_cercana <- tuplas_estaciones$mas_cercana[tuplas_estaciones$Codigo_nacional == registro$Codigo_nacional]
        pp_DMC_relleno$pp_day[i] <- obtener_valor(pp_DMC_relleno, estacion_mas_cercana, registro$Year, registro$Month, registro$Day, "pp_day")
    }
}

summary(pp_DMC_relleno)
# 7575 NA's

#PASO 2: Imputacion de mediana mensual de la estación
pp_DMC_relleno <- pp_DMC_relleno %>%
  group_by(Codigo_nacional, Year, Month) %>%
  mutate(
    pp_day = ifelse(is.na(pp_day), ave(pp_day, FUN = function(x) median(x, na.rm = TRUE)), pp_day)
  ) %>%
  ungroup()

summary(pp_DMC_relleno)
# 5111 NA's

#PASO 3: Imputacion de mediana anual de la estación
pp_DMC_relleno <- pp_DMC_relleno %>%
  group_by(Codigo_nacional, Year) %>%
  mutate(
    pp_day = ifelse(is.na(pp_day), ave(pp_day, FUN = function(x) median(x, na.rm = TRUE)), pp_day)
  ) %>%
  ungroup()

summary(pp_DMC_relleno)
# 3276 NA's

#PASO 4: Imputacion de mediana global, por seguridad si aún existe algún valor NA
pp_DMC_relleno <- pp_DMC_relleno %>% 
  group_by(Year) %>%
  mutate(
    pp_day = ifelse(is.na(pp_day), median(pp_day, na.rm = TRUE), pp_day)
  )

summary(pp_DMC_relleno)

# 0 NA's

write.csv(pp_DMC_relleno, paste0(directorio_base, "/BBDD/pp/DMC/depurado/pp_DMC_2020_2024_4.csv"), row.names = FALSE)
