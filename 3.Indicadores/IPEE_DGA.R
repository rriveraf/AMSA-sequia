
directorio_base = getwd()

#cargar precipitación y temperaturas históricas
pp_mensual_dga_1979_2019 <- read.csv(paste0(directorio_base, "/BBDD/pp/DGA/periodo_historico/pp_DGA_monthly_1979_2019_cr2met.csv"))
temp_mensual_dga_1979_2019 <- read.csv(paste0(directorio_base, "/BBDD/temp/DGA/periodo_historico/temp_DGA_monthly_1979_2019_cr2met.csv"))


head(pp_mensual_dga_1979_2019)
names(pp_mensual_dga_1979_2019)[names(pp_mensual_dga_1979_2019) == "precipitacion_mensual"] <- "pp_month"
summary(pp_mensual_dga_1979_2019)

#cargar precipitacion y temperaturas descargadas
pp_day_dga_2020_actual <- read.csv(paste0(directorio_base, "/BBDD/pp/DGA/depurado/pp_DGA_2020_2024_4.csv"))
temp_day_dga_2020_actual <- read.csv(paste0(directorio_base, "/BBDD/temp/DGA/depurado/temp_DGA_2020_2024_3.csv"))


head(pp_day_dga_2020_actual)

#calcular precipitacion mensual
pp_mensual_2020_actual <- pp_day_dga_2020_actual %>%
  group_by(Year, Month, Codigo_nacional) %>%
  summarise(pp_month = sum(pp_day, na.rm = TRUE)) %>%
  ungroup()

summary(pp_mensual_2020_actual)
#calcular temperaturas extremas mensuales
temp_mensual_2020_actual <- temp_day_dga_2020_actual %>%
  group_by(Year, Month, Codigo_nacional) %>%
  summarise(
    temp_min = mean(temp_min, na.rm = TRUE),
    temp_max = mean(temp_max, na.rm = TRUE)
  ) %>%
  ungroup()

#generamos las series completas de precipitación y temperatura
pp_mensual_completo <- rbind(pp_mensual_dga_1979_2019, pp_mensual_2020_actual)
temp_mensual_completo <- rbind(temp_mensual_dga_1979_2019, temp_mensual_2020_actual)
head(temp_mensual_completo)

#juntamos todos los archivos en un solo dataframe
pp_temp_mensual_completo <- merge(pp_mensual_completo, temp_mensual_completo, by = c("Year", "Month", "Codigo_nacional"))

tail(pp_temp_mensual_completo, 10)
#ordenar por codigo nacional, año y mes: CLAVE PARA CALCULAR SPEI
pp_temp_mensual_completo <- pp_temp_mensual_completo %>%
  arrange(Codigo_nacional, Year, Month)


#obtenemos latitud y longitud de cada estación para calcular la evapotranspiracion
csv_metadata_pp_y_temp_path = paste0(directorio_base, "/BBDD/metadatos/DGA/pp_y_temp/estaciones_DGA_pp_y_temp.csv")
#Definir columnas de los archivos de metadatos, con su tipo de dato específico
tipos <- cols(
  Nombre = col_character(),
  LAT = col_double(),
  LONG = col_double(),
  Altura = col_integer(),
  COD_CUEN = col_integer(),
  COD_SUBC = col_integer(),
  COD_SSUBC = col_integer(),
  NOM_CUEN = col_character(),
  NOM_SUBC = col_character(),
  NOM_SSUBC = col_character(),
  COD_REG = col_integer(),
  COD_PROV = col_integer(),
  COD_COM = col_integer(),
  NOM_REG = col_character(),
  NOM_PROV = col_character(),
  NOM_COM = col_character()
)
metadata_DGA_pp_y_temp <- read_csv(csv_metadata_pp_y_temp_path, col_types = tipos)

pp_temp_mensual_completo$lat <- metadata_DGA_pp_y_temp$LAT[match(pp_temp_mensual_completo$Codigo_nacional, metadata_DGA_pp_y_temp$Codigo_nacional)]

tail(pp_temp_mensual_completo)
summary(pp_temp_mensual_completo)

#omitir NA, son muy poquitos
pp_temp_mensual_completo <- na.omit(pp_temp_mensual_completo)

#calcular evapotranspiracion agrupando primero por estacion
pp_temp_mensual_completo <- pp_temp_mensual_completo %>%
  group_by(Codigo_nacional) %>%
  rowwise() %>%  # Asegura que cada operación se aplique a una sola fila
  mutate(
    ETP = hargreaves(Tmin = temp_min, Tmax = temp_max, lat = lat, Pre = pp_month, verbose = FALSE, na.rm = TRUE)
  ) %>%
  ungroup()

summary(pp_temp_mensual_completo)
#ordenar por ETP decreciente

#########test
#pp_temp_2019 <- pp_temp_mensual_completo %>%
#  filter(Year <= 2019) %>%
#  select(Codigo_nacional, Year, Month, pp_month, temp_min, temp_max, ETP)
#summary(pp_temp_2019)
#
#
#pp_temp_2019 <- pp_temp_2019[order(pp_temp_2019$temp_max, decreasing = TRUE), ]
#head(pp_temp_2019,20)
#
#pp_temp_2020_2024 <- pp_temp_mensual_completo %>%
#  filter(Year >= 2019) %>%
#  select(Codigo_nacional, Year, Month, pp_month, temp_min, temp_max, ETP)
#
#pp_temp_2020_2024 <- pp_temp_2020_2024[order(pp_temp_2020_2024$ETP, decreasing = TRUE), ]
#head(pp_temp_2020_2024,20)
###########test



pp_temp_mensual_completo$BH <- pp_temp_mensual_completo$pp_month - pp_temp_mensual_completo$ETP

head(pp_temp_mensual_completo)

#calculamos IPEE
spei_dataframe <- pp_temp_mensual_completo %>%
  group_by(Codigo_nacional) %>%
  mutate(
    spei_1 = NA,
    spei_3 = NA,
    spei_6 = NA,
    spei_12 = NA,
    spei_24 = NA
  ) %>%
  ungroup()

#calculamos spei_1, spei_3, spei_6, spei_12, spei_24
spei_dataframe <- spei_dataframe %>%
  group_by(Codigo_nacional) %>%
  mutate(
    spei_1 = spei(BH, scale = 1, distribution = "log-Logistic", na.rm = TRUE)$fitted,
    spei_3 = spei(BH, scale = 3, distribution = "log-Logistic", na.rm = TRUE)$fitted,
    spei_6 = spei(BH, scale = 6, distribution = "log-Logistic", na.rm = TRUE)$fitted,
    spei_12 = spei(BH, scale = 12, distribution = "log-Logistic", na.rm = TRUE)$fitted,
    spei_24 = spei(BH, scale = 24, distribution = "log-Logistic", na.rm = TRUE)$fitted
  ) %>%
  ungroup()

head(spei_dataframe)


write.csv(spei_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPEE/DGA/IPEE_DGA_1979_2024_3.csv"))


spei_2023 <- spei_dataframe %>% filter(Year >= 2023)
write.csv(spei_2023, "spei_2023_2024_3.csv")
