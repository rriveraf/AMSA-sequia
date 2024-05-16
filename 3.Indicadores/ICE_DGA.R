

directorio_base = getwd()

# cargar periodo historico
caudal_mensual_1989_2019 <- read.csv(paste0(directorio_base, "/BBDD/q/DGA/periodo_historico/q_DGA_monthly_1989_2019.csv"))
head(caudal_mensual_1989_2019)

#cargar datos descargados
caudal_day_2020_actual <- read.csv(paste0(directorio_base, "/BBDD/q/DGA/depurado/q_mean_DGA_2020_2024_4.csv"))
head(caudal_day_2020_actual)

#calcular caudal mensual
caudal_mensual_2020_actual <- caudal_day_2020_actual %>%
  group_by(Year, Month, Codigo_nacional) %>%
  summarise(q_month = mean(caudal_mean, na.rm = TRUE)) %>%
  ungroup()

#generar dataframe completo de datos de caudal
#primero solo seleccionamos (en la data nueva) aquellas estaciones que están en la data histórica
estaciones_historicas = unique(caudal_mensual_1989_2019$Codigo_nacional)
caudal_mensual_2020_actual <- caudal_mensual_2020_actual[caudal_mensual_2020_actual$Codigo_nacional %in% estaciones_historicas,]

head(caudal_mensual_2020_actual)
head(caudal_mensual_1989_2019)
caudal_mensual_1989_2019 <- select(caudal_mensual_1989_2019, -X)


caudal_mensual_completo <- rbind(caudal_mensual_1989_2019, caudal_mensual_2020_actual)

#ordenar por codigo nacional, año y mes: CLAVE PARA CALCULAR ICE
caudal_mensual_completo <- caudal_mensual_completo %>%
  arrange(Codigo_nacional, Year, Month)

#si q = 0, entonces q = 0.0001, Así evitar errores en el cálculo de ICE.
caudal_mensual_completo$q_month[caudal_mensual_completo$q_month == 0] <- 0.0001

#computamos los periodos de acumulación de caudal (promedios moviles)
head(caudal_mensual_completo)


caudal_mensual_completo <- caudal_mensual_completo %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep="-"))) %>%
  arrange(Codigo_nacional, Date)

caudal_mensual_completo <- caudal_mensual_completo %>%
  group_by(Codigo_nacional) %>%
  mutate(q_3month = rollapply(q_month, 3, mean, partial = FALSE, fill = NA, align = "right"),
         q_6month = rollapply(q_month, 6, mean, partial = FALSE, fill = NA, align = "right"),
         q_12month = rollapply(q_month, 12, mean, partial = FALSE, fill = NA, align = "right"),
         q_24month = rollapply(q_month, 24, mean, partial = FALSE, fill = NA, align = "right"))

head(caudal_mensual_completo)

caudal_mensual_completo <- select(caudal_mensual_completo, -Date)

#generamos columnas para ice_1, ice_3, ice_6, ice_12, ice_24
ice_dataframe <- caudal_mensual_completo %>%
  group_by(Codigo_nacional) %>%
  mutate(
    ice_1 = NA,
    ice_3 = NA,
    ice_6 = NA,
    ice_12 = NA,
    ice_24 = NA
  ) %>%
  ungroup()

#calculamos ice_1, ice_3, ice_6, ice_12, ice_24

ice_dataframe <- ice_dataframe %>%
  group_by(Codigo_nacional) %>%
  mutate(
    ice_1 = spi(q_month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
    ice_3 = spi(q_3month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
    ice_6 = spi(q_6month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
    ice_12 = spi(q_12month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted,
    ice_24 = spi(q_24month, scale = 1, distribution = "Gamma", na.rm = TRUE, verbose = FALSE)$fitted
  ) %>%
  ungroup()

head(ice_dataframe)

#ice_dataframe <- select(ice_dataframe, -q_3month, -q_6month, -q_12month, -q_24month)

head(ice_dataframe, 20)
write.csv(ice_dataframe, paste0(directorio_base, "/BBDD/indicadores/ICE/DGA/ICE_DGA_1989_2024_4.csv"))


ice_dataframe <- ice_dataframe %>% filter(Codigo_nacional == "0573008-6")

head(ice_dataframe)

write.csv(ice_dataframe, "test.csv")
