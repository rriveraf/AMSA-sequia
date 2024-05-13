

directorio_base = getwd()

#cargar periodo historico
pp_mensual_dmc_1979_2019 <- read.csv(paste0(directorio_base, "/BBDD/pp/DMC/periodo_historico/pp_DMC_monthly_1979_2019_cr2met.csv"))
head(pp_mensual_dmc_1979_2019)
names(pp_mensual_dmc_1979_2019)[names(pp_mensual_dmc_1979_2019) == "precipitacion_mensual"] <- "pp_month"
summary(pp_mensual_dmc_1979_2019)

#cargar datos descargados
pp_day_dmc_2020_actual <- read.csv(paste0(directorio_base, "/BBDD/pp/DMC/depurado/pp_DMC_2020_2024_4.csv"))

head(pp_day_dmc_2020_actual)

#calcular precipitacion mensual
pp_mensual_2020_actual <- pp_day_dmc_2020_actual %>%
  group_by(Year, Month, Codigo_nacional) %>%
  summarise(pp_month = sum(pp_day, na.rm = TRUE)) %>%
  ungroup()

#generar dataframe completo de datos de precipitacion
pp_mensual_completo <- rbind(pp_mensual_dmc_1979_2019, pp_mensual_2020_actual)


#si PP == 0, entonces PP = 0.0001, Así evitar errores en el cálculo de SPI.
pp_mensual_completo$pp_month[pp_mensual_completo$pp_month == 0] <- 0.0001
#ordenar por codigo nacional, año y mes: CLAVE PARA CALCULAR SPI
pp_mensual_completo <- pp_mensual_completo %>%
  arrange(Codigo_nacional, Year, Month)

#generamos columnas para spi_1, spi_3, spi_6, spi_12, spi_24
spi_dataframe <- pp_mensual_completo %>%
  group_by(Codigo_nacional) %>%
  mutate(
    spi_1 = NA,
    spi_3 = NA,
    spi_6 = NA,
    spi_12 = NA,
    spi_24 = NA
  ) %>%
  ungroup()

#calculamos spi_1, spi_3, spi_6, spi_12, spi_24

spi_dataframe <- spi_dataframe %>%
  group_by(Codigo_nacional) %>%
  mutate(
    spi_1 = spi(pp_month, scale = 1, distribution = "Gamma", na.rm = TRUE)$fitted,
    spi_3 = spi(pp_month, scale = 3, distribution = "Gamma", na.rm = TRUE)$fitted,
    spi_6 = spi(pp_month, scale = 6, distribution = "Gamma", na.rm = TRUE)$fitted,
    spi_12 = spi(pp_month, scale = 12, distribution = "Gamma", na.rm = TRUE)$fitted,
    spi_24 = spi(pp_month, scale = 24, distribution = "Gamma", na.rm = TRUE)$fitted
  ) %>%
  ungroup()

head(spi_dataframe)

write.csv(spi_dataframe, paste0(directorio_base, "/BBDD/indicadores/IPE/DMC/IPE_DMC_1979_2024_4.csv"))

spi <- spi_dataframe %>% filter(Codigo_nacional == "10414001-7")

head(spi_2023)

write.csv(spi_2023, "DMC_spi_2023_2024_4.csv")
