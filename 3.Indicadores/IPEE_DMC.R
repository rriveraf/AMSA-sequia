
directorio_base = getwd()

#cargar precipitacion y temperaturas historicas
pp_mensual_dmc_1979_2019 <- read.csv(paste0(directorio_base, "/BBDD/pp/DMC/periodo_historico/pp_DMC_monthly_1979_2019_cr2met.csv"))
temp_mensual_dmc_1979_2019 <- read.csv(paste0(directorio_base, "/BBDD/temp/DMC/periodo_historico/temp_DMC_monthly_1979_2019_cr2met.csv"))

head(pp_mensual_dmc_1979_2019)
names(pp_mensual_dmc_1979_2019)[names(pp_mensual_dmc_1979_2019) == "precipitacion_mensual"] <- "pp_month"
head(temp_mensual_dmc_1979_2019)

#cargar precipitacion y temperaturas descargadas
pp_day_dmc_2020_actual <- read.csv(paste0(directorio_base, "/BBDD/pp/DMC/depurado/pp_DMC_2020_2024_4.csv"))
temp_day_dmc_2020_actual <- read.csv(paste0(directorio_base, "/BBDD/temp/DMC/depurado/temp_DMC_2020_2024_4.csv"))

head(pp_day_dmc_2020_actual)

#calcular precipitacion mensual
pp_mensual_2020_actual <- pp_day_dmc_2020_actual %>%
  group_by(Year, Month, Codigo_nacional) %>%
  summarise(pp_month = sum(pp_day, na.rm = TRUE)) %>%

  ungroup()

summary(pp_mensual_2020_actual)

#calcular temperaturas extremas mensuales
temp_mensual_2020_actual <- temp_day_dmc_2020_actual %>%
  group_by(Year, Month, Codigo_nacional) %>%
  summarise(
    temp_min = mean(temp_min, na.rm = TRUE),
    temp_max = mean(temp_max, na.rm = TRUE)
  ) %>%
  ungroup()

#generamos las series completas de precipitacion y temperatura
pp_mensual_completo <- rbind(pp_mensual_dmc_1979_2019, pp_mensual_2020_actual)
temp_mensual_completo <- rbind(temp_mensual_dmc_1979_2019, temp_mensual_2020_actual)

head(temp_mensual_completo)

#juntamos todos los archivos en un solo dataframe
pp_temp_mensual_completo <- merge(pp_mensual_completo, temp_mensual_completo, by = c("Year", "Month", "Codigo_nacional"))

tail(pp_temp_mensual_completo, 10)

#ordenar por codigo nacional, año y mes: CLAVE PARA CALCULAR SPEI
pp_temp_mensual_completo <- pp_temp_mensual_completo %>%
  arrange(Codigo_nacional, Year, Month)

#obtenemos latitud y longitud de cada estacion para calcular la evapotranspiracion

metadata_dmc = read_excel(paste0(directorio_base,"/BBDD/metadatos/DMC/estaciones_DMC.xlsx"))

#agregar latitud al dataframe completo
pp_temp_mensual_completo$lat <- metadata_dmc$LAT[match(pp_temp_mensual_completo$Codigo_nacional, metadata_dmc$Codigo_nacional)]

#calcular evapotranspiracion agrupaando por estacion
pp_temp_mensual_completo <- pp_temp_mensual_completo %>%
  group_by(Codigo_nacional) %>%
  rowwise() %>%  # Asegura que cada operación se aplique a una sola fila
  mutate(
    ETP = hargreaves(Tmin = temp_min, Tmax = temp_max, lat = lat, Pre = pp_month, verbose = FALSE, na.rm = TRUE)
  ) %>%
  ungroup()


pp_temp_mensual_completo <- pp_temp_mensual_completo %>%
  rowwise() %>%  # Asegura que cada operación se aplique a una sola fila
  mutate(
    BH = pp_month - ETP
  )


pp_temp_mensual_completo <- na.omit(pp_temp_mensual_completo)

#calculamos SPEI

#generamos columnas para spei_1, spei_3, spei_6, spei_12, spei_24
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
    spei_1 = spei(BH, scale = 1, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
    spei_3 = spei(BH, scale = 3, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
    spei_6 = spei(BH, scale = 6, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
    spei_12 = spei(BH, scale = 12, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted,
    spei_24 = spei(BH, scale = 24, distribution = "log-Logistic", na.rm = TRUE, verbose = FALSE)$fitted
  ) %>%
  ungroup()

head(spei_dataframe)

write.csv(spei_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPEE/DMC/IPEE_DMC_1979_2024_4.csv"))

spei_2020 <- spei_dataframe %>% filter(Year >= 2020)

head(spei_2020)

write.csv(spei_2020, "DMC_spei_2020_2024_4.csv")



pp_dga <- read.csv(paste0(directorio_base, "/BBDD/indicadores/IPE/DMC/IPE_DMC_1979_2024_4.csv"))

head(pp_dga)

#seleccionar columnas Codigo_nacional pp_month Year Month
pp_dga <- pp_dga %>% select(Codigo_nacional, pp_month, Year, Month)

head(pp_dga)

write.csv(pp_dga, "pp_dmc_monthly_1979_2024_4.csv")
