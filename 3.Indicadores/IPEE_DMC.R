
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
