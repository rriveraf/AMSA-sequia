
graficar_series_ipe <- function(ipe_dataframe, Codigo_estacion, ano_inicio, mes_inicio, ano_fin, mes_fin, metadata) {
  library(tidyverse)
  library(ggplot2)

  # Nombre de la estación
  metadata <- metadata %>% filter(Codigo_nacional == Codigo_estacion)
  nombre_estacion = metadata$Nombre

  # Filtrar por estación y limpiar datos
  ipe_estacion <- ipe_dataframe %>%
    filter(Codigo_nacional == Codigo_estacion) %>%
    mutate(Date = as.Date(paste(Year, Month, "1", sep="-"), "%Y-%m-%d"),
           across(everything(), ~replace(., . == -Inf, NA)))  # Reemplazar -Inf con NA para todas las columnas

  # Filtrar por periodo indicado
  ipe_estacion <- ipe_estacion %>%
    filter(Date >= as.Date(paste(ano_inicio, mes_inicio, "1", sep="-"), "%Y-%m-%d") &
           Date <= as.Date(paste(ano_fin, mes_fin, "1", sep="-"), "%Y-%m-%d"))

  # Transformar datos SPI a formato largo
  spi_estacion_long <- ipe_estacion %>%
    pivot_longer(cols = starts_with("spi"), names_to = "SPI_Type", values_to = "Value") %>%
    mutate(SPI_Type = factor(SPI_Type, levels = c("spi_1", "spi_3", "spi_6", "spi_12", "spi_24"),
                             labels = c("SPI 1", "SPI 3", "SPI 6", "SPI 12", "SPI 24")))

  # Gráfico de SPI
  spi_plot <- ggplot(data = spi_estacion_long, aes(x = Date, y = Value, color = SPI_Type)) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
    labs(title = paste0("Índices SPI entre ", ano_inicio, "-", mes_inicio, " y ", ano_fin, "-", mes_fin),
         x = "Fecha", y = "Valor SPI", color = "Valores SPI") +
    scale_color_brewer(palette = "Dark2") +
    theme(plot.background = element_rect(fill = "white", color = "black"),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey", linetype = "dotted"),
          panel.grid.minor = element_line(color = "grey88", linetype = "dotted"),
          legend.position = "bottom")

  # Gráfico de precipitaciones
  pp_plot <- ggplot(data = ipe_estacion, aes(x = Date, y = pp_month)) +
    geom_line(color = "blue", size = 1.5) +
    geom_point(color = "blue", size = 3) +
    labs(title = paste0("Precipitaciones entre ", ano_inicio, "-", mes_inicio, " y ", ano_fin, "-", mes_fin),
         x = "Fecha", y = "Precipitaciones (mm)") +
    theme(plot.background = element_rect(fill = "white", color = "black"),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey", linetype = "dotted"),
          panel.grid.minor = element_line(color = "grey88", linetype = "dotted"))

  # Combinar los gráficos
  combined_plot <- spi_plot + pp_plot +
    plot_layout(ncol = 1) +
    plot_annotation(title = paste0("Estación ", nombre_estacion), theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))

  # Mostrar el gráfico combinado
  print(combined_plot)
}

graficar_series_ipee <- function(ipee_dataframe, Codigo_estacion, ano_inicio, mes_inicio, ano_fin, mes_fin, metadata) {
  library(tidyverse)
  library(ggplot2)

  # Nombre de la estación
  metadata <- metadata %>% filter(Codigo_nacional == Codigo_estacion)
  nombre_estacion = metadata$Nombre

  # Filtrar por estación y limpiar datos
  ipee_estacion <- ipee_dataframe %>%
    filter(Codigo_nacional == Codigo_estacion) %>%
    mutate(Date = as.Date(paste(Year, Month, "1", sep="-"), "%Y-%m-%d"),
           across(starts_with(c("spei", "pp_", "ETP")), ~replace(., . == -Inf, NA)))  # Reemplazar -Inf con NA para todas las columnas relevantes

  # Filtrar por periodo indicado
  ipee_estacion <- ipee_estacion %>% filter(Date >= as.Date(paste(ano_inicio, mes_inicio, "1", sep="-"), "%Y-%m-%d") & Date <= as.Date(paste(ano_fin, mes_fin, "1", sep="-"), "%Y-%m-%d"))

  # Transformar datos SPEI a formato largo
  spei_estacion_long <- ipee_estacion %>%
    pivot_longer(cols = starts_with("spei"), names_to = "SPEI_Type", values_to = "Value") %>%
    mutate(SPEI_Type = factor(SPEI_Type, levels = c("spei_1", "spei_3", "spei_6", "spei_12", "spei_24"),
                             labels = c("SPEI 1", "SPEI 3", "SPEI 6", "SPEI 12", "SPEI 24")))

  # Gráfico de SPEI
  spei_plot <- ggplot(data = spei_estacion_long, aes(x = Date, y = Value, color = SPEI_Type)) +
    geom_line(size = 1.5) + 
    geom_point(size = 3) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) + 
    labs(title = paste0("Índices SPEI entre ", ano_inicio, "-", mes_inicio, " y ", ano_fin, "-", mes_fin),
         x = "Fecha", y = "Valor SPEI", color = "Valores SPEI") +
    scale_color_brewer(palette = "Dark2") +
    theme(plot.background = element_rect(fill = "white", color = "black"),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey", linetype = "dotted"),
          panel.grid.minor = element_line(color = "grey88", linetype = "dotted"),
          legend.position = "bottom")

  # Gráfico de precipitaciones y evapotranspiración
  pe_plot <- ggplot(data = ipee_estacion, aes(x = Date)) +
    geom_line(aes(y = pp_month, color = "Precipitaciones (mm)"), size = 1.5) +
    geom_line(aes(y = ETP, color = "Evapotranspiración Potencial (mm)"), size = 1.5) +
    geom_point(aes(y = pp_month), color = "blue", size = 3) +
    geom_point(aes(y = ETP), color = "red", size = 3) +
    labs(title = paste0("Precipitaciones y Evapotranspiración Potencial entre ", ano_inicio, "-", mes_inicio, " y ", ano_fin, "-", mes_fin),
         x = "Fecha", y = "PP y ETP (mm)", color = "") +
    scale_color_manual(values = c("Precipitaciones (mm)" = "blue", "Evapotranspiración Potencial (mm)" = "red")) +
    theme(plot.background = element_rect(fill = "white", color = "black"),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey", linetype = "dotted"),
          panel.grid.minor = element_line(color = "grey88", linetype = "dotted"),
          legend.position = "bottom")

  # Combinar los gráficos
  combined_plot <- spei_plot + pe_plot +
    plot_layout(ncol = 1) +
    plot_annotation(title = paste0("Estación ", nombre_estacion), theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))

  # Mostrar el gráfico combinado
  print(combined_plot)
}

graficar_series_ice <- function(ice_dataframe, Codigo_estacion, ano_inicio, mes_inicio, ano_fin, mes_fin, metadata){
  library(tidyverse)
  library(ggplot2)
  
  #nombre de la estacion
  metadata <- metadata %>% filter(Codigo_nacional == Codigo_estacion)
  nombre_estacion = metadata$Nombre

  # Filtrar por estación y limpiar datos
  ice_estacion <- ice_dataframe %>%
    filter(Codigo_nacional == Codigo_estacion) %>%
    mutate(Date = as.Date(paste(Year, Month, "1", sep="-"), "%Y-%m-%d"),
           across(starts_with(c("ice", "q_")), ~replace(., . == -Inf, NA)))  # Reemplazar -Inf con NA para todas las columnas relevantes
  
  #filtrar por periodo indicado
  ice_estacion <- ice_estacion %>% filter(Date >= as.Date(paste(ano_inicio, mes_inicio, "1", sep="-"), "%Y-%m-%d") & Date <= as.Date(paste(ano_fin, mes_fin, "1", sep="-"), "%Y-%m-%d"))


  # Transformar datos ICE a formato largo
  ice_estacion_long <- ice_estacion %>%
    pivot_longer(cols = starts_with("ice"), names_to = "ICE_Type", values_to = "Value") %>%
    mutate(ICE_Type = factor(ICE_Type, levels = c("ice_1", "ice_3", "ice_6", "ice_12", "ice_24"),
                             labels = c("ICE 1", "ICE 3", "ICE 6", "ICE 12", "ICE 24")))

  # Transformar datos de caudales a formato largo
  q_estacion_long <- ice_estacion %>%
    pivot_longer(cols = starts_with("q_"), names_to = "q_Type", values_to = "Value") %>%
    mutate(q_Type = factor(q_Type, levels = c("q_month", "q_3month", "q_6month", "q_12month", "q_24month"),
                           labels = c("Caudal 1", "Caudal 3", "Caudal 6", "Caudal 12", "Caudal 24")))

  # Gráfico de ICE
  ice_plot <- ggplot(data = ice_estacion_long, aes(x = Date, y = Value, color = ICE_Type)) +
    geom_line(size = 1.5) + 
    geom_point(size = 3) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) + 
    labs(title = paste0("Índices de Caudal estandarizado (ICE) entre ", ano_inicio, "-", mes_inicio, " y ", ano_fin, "-", mes_fin),
         x = "Fecha", y = "Valor ICE", color = "Valores ICE") +
    scale_color_brewer(palette = "Dark2") +
    theme(plot.background = element_rect(fill = "white", color = "black"),  # Fondo blanco con marco negro
        panel.background = element_rect(fill = "white", color = "black"),  # Igual para el panel
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),  # Añade cuadrícula mayor
        panel.grid.minor = element_line(color = "grey88", linetype = "dotted"),  # Añade cuadrícula menor
        legend.position = "bottom")

  # Gráfico de caudales
  q_plot <- ggplot(data = q_estacion_long, aes(x = Date, y = Value, color = q_Type)) +
    geom_line(size = 1.5) + 
    geom_point(size = 3) + 
    labs(title = paste0("Caudales promedio mensuales según acumulación entre ", ano_inicio, "-", mes_inicio, " y ", ano_fin, "-", mes_fin),
         x = "Fecha", y = "Valor Caudal", color = "Valores Caudal") +
    scale_color_brewer(palette = "Dark2") +
    theme(plot.background = element_rect(fill = "white", color = "black"),  # Fondo blanco con marco negro
        panel.background = element_rect(fill = "white", color = "black"),  # Igual para el panel
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),  # Añade cuadrícula mayor
        panel.grid.minor = element_line(color = "grey88", linetype = "dotted"),  # Añade cuadrícula menor
        legend.position = "bottom")

  # Combinar los gráficos
  combined_plot <- ice_plot + q_plot +
    plot_layout(ncol = 1) +
    plot_annotation(title = paste0("Estación ", nombre_estacion), theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))

  # Mostrar el gráfico combinado
  print(combined_plot)
}
