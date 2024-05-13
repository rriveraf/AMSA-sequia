

directorio_base = getwd()

source(paste0(directorio_base, "/2.Depurado_y_Relleno/funciones.R"))


caudal_DGA = read.csv(paste0(directorio_base, "/BBDD/q/DGA/bruto/q_mean_DGA_2023_2024_4.csv"))
metadatos_caudal = read.csv(paste0(directorio_base, "/BBDD/metadatos/DGA/caudal/estaciones_DGA_caudal.csv"))

summary(caudal_DGA)
#224087 registros

#marcar outliers
caudal_DGA$caudal_mean[caudal_DGA$caudal_mean < 0] <- NA

#borrar entradas invalidas, asi quitar estaciones inactivas
caudal_DGA <- drop_na(caudal_DGA)

#eliminar duplicados
caudal_DGA <- unique(caudal_DGA)

#contamos la cantidad de mediciciones para cada estacion en el periodo de tiempo
count_estaciones <- caudal_DGA %>% group_by(Codigo_nacional) %>% summarise(n = n()) %>% arrange(desc(n))

#OJO AL FUTURO PARA AUTOMATIZAR ESTO
#obtenemos fechas del archivo
fechas_archivo <- obtener_fechas_archivo(paste0(directorio_base, "/BBDD/q/DGA/bruto/q_mean_DGA_2023_2024_4.csv"))

#calculamos el mínimo número de datos que debería tener una estación según las fechas del archivo. se fija una completitud del 70%
n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin*30) * 0.7


#quitamos estaciones que tienen menos del 70% de mediciones
estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
caudal_DGA_validas <- caudal_DGA %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)

summary(caudal_DGA_validas)
#214045 registros


#rellenamos las fechas faltantes para cada estación, dejando los valores de caudal como NA
caudal_DGA_relleno <- rellenar_fechas(caudal_DGA_validas)
caudal_DGA_relleno <- caudal_DGA_relleno %>%
  select(Year, Month, Day, Codigo_nacional, caudal_mean)
summary(caudal_DGA_relleno)
#6203 NA's
#217512 registros
###########COMIENZA RELLENADO DE DATOS FALTANTES###########
#PASO 1: rellenamos los valores faltantes con la mediana mensual de la estacion
caudal_DGA_relleno <- caudal_DGA_relleno %>%
  group_by(Codigo_nacional, Year, Month) %>%
  mutate(
    caudal_mean = ifelse(is.na(caudal_mean), ave(caudal_mean, FUN = function(x) median(x, na.rm = TRUE)), caudal_mean)
  ) %>%
  ungroup()

summary(caudal_DGA_relleno)
#2327 NA's

#PASO 2: rellenamos con mediana anual de la estacion

caudal_DGA_relleno <- caudal_DGA_relleno %>%
  group_by(Codigo_nacional, Year) %>%
  mutate(
    caudal_mean = ifelse(is.na(caudal_mean), ave(caudal_mean, FUN = function(x) median(x, na.rm = TRUE)), caudal_mean)
  ) %>%
  ungroup()

summary(caudal_DGA_relleno)
#91 NA's

#PASO 3: rellenamos con la mediana global, por seguridad si existe algún valor NA
caudal_DGA_relleno <- caudal_DGA_relleno %>% 
  group_by(Year) %>%
  mutate(
    caudal_mean = ifelse(is.na(caudal_mean), median(caudal_mean, na.rm = TRUE), caudal_mean)
  )

summary(caudal_DGA_relleno)
#0 NA's

write.csv(caudal_DGA_relleno, paste0(directorio_base, "/BBDD/q/DGA/depurado/q_mean_DGA_2023_2024_3.csv"), row.names = FALSE)
