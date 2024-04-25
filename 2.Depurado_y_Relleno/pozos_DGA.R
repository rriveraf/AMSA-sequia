
directorio_base = getwd()

source(paste0(directorio_base, "/2.Depurado_y_Relleno/funciones.R"))


pozos_DGA = read.csv(paste0(directorio_base, "/BBDD/niveles_pozos/bruto/niveles_pozos_DGA_2023_2024_3.csv"))

#marcar outliers
pozos_DGA$nivel_mean[pozos_DGA$nivel_mean < 0] <- NA

#borrar entradas invalidas, asi quitar estaciones inactivas
pozos_DGA <- drop_na(pozos_DGA)

#eliminar duplicados
pozos_DGA <- unique(pozos_DGA)

#identificar estaciones que tienen mas datos en el ultimo mes
count_estaciones <- pozos_DGA %>% group_by(Codigo_nacional) %>% summarise(n = n()) %>% arrange(desc(n))

#OJO AL FUTURO PARA AUTOMATIZAR ESTO
#obtenemos fechas del archivo
fechas_archivo <- obtener_fechas_archivo(paste0(directorio_base, "/BBDD/niveles_pozos/bruto/niveles_pozos_DGA_2023_2024_3.csv"))

#calculamos el mínimo número de datos que debería tener una estación según las fechas del archivo. se fija una completitud del 70%
n_minimo_datos = ((fechas_archivo$ano_fin - fechas_archivo$ano_ini) * 365 + fechas_archivo$mes_fin*30) * 0.7


#quitamos estaciones que tienen menos de 20 datos en el ultimo mes
estaciones_validas <- count_estaciones %>% filter(n >= n_minimo_datos)
pozos_DGA_validas <- pozos_DGA %>% filter(Codigo_nacional %in% estaciones_validas$Codigo_nacional)

#rellenamos las fechas faltantes para cada estación, dejando los valores de nivel como NA
pozos_DGA_relleno <- rellenar_fechas(pozos_DGA_validas)
pozos_DGA_relleno <- pozos_DGA_relleno %>%
  select(Year, Month, Day, Codigo_nacional, nivel_mean)

summary(pozos_DGA_relleno)
#19608 registros
#1654 NA's

######COMIENZA RELLENADO DE DATOS FALTANTES######
#PASO 1: relleno con mediana mensual de la estacion
pozos_DGA_relleno <- pozos_DGA_relleno %>%
  group_by(Codigo_nacional, Year, Month) %>%
  mutate(
    nivel_mean = ifelse(is.na(nivel_mean), ave(nivel_mean, FUN = function(x) median(x, na.rm = TRUE)), nivel_mean)
  ) %>%
  ungroup()

summary(pozos_DGA_relleno)
# 1043 NA's

#PASO 2: relleno con mediana anual de la estacion
pozos_DGA_relleno <- pozos_DGA_relleno %>%
  group_by(Codigo_nacional, Year) %>%
  mutate(
    nivel_mean = ifelse(is.na(nivel_mean), ave(nivel_mean, FUN = function(x) median(x, na.rm = TRUE)), nivel_mean)
  ) %>%
  ungroup()

summary(pozos_DGA_relleno)
# 0 NA's

#PASO 3: relleno con mediana global de la estacion
pozos_DGA_relleno <- pozos_DGA_relleno %>%
    group_by(Codigo_nacional) %>%
  mutate(
    nivel_mean = ifelse(is.na(nivel_mean), ave(nivel_mean, FUN = function(x) median(x, na.rm = TRUE)), nivel_mean)
  )
summary(pozos_DGA_relleno)

write.csv(pozos_DGA_relleno, paste0(directorio_base, "/BBDD/niveles_pozos/depurado/niveles_pozos_DGA_2023_2024_3.csv"), row.names = FALSE)
