interpolar_y_actualizar_shapefiles <- function(mes_ultimo, ano_fin){
#Esta funcion engloba todas las funciones presentes en este sript
#Inicia con la interpolación de los indicadores IPEE e IPE y finaliza guardando archivos.shp de los indicadores en sus respectivas carpetas
#Además, se encarga de convertir los puntos de ICE en shapefiles

  #mes_ultimo<-as.numeric(month(as.Date(Sys.time()))-1)
  #mes_ultimo <- as.numeric(substr(Sys.time() %m-% months(1), 6, 7))
  #ano_fin <- as.numeric(substr(Sys.time() %m-% months(1), 1, 4))

  if (mes_ultimo == 12) {
    ano_siguiente <- ano_fin + 1
    mes_siguiente <- 1
  } else {
      ano_siguiente <- ano_fin
      mes_siguiente <- mes_ultimo + 1
  }

  fecha_ini_interpolacion <- as.Date(paste0(paste(ano_fin, mes_ultimo, sep = "-"), "-01"))
  fecha_fin_interpolacion <- as.Date(paste0(paste(ano_siguiente, mes_siguiente, sep = "-"), "-01"))
  
  #Definimos los elmentos en el dataset, cada uno con un "path" y "variable"
  datasets <- list(

   IPE = list(path=paste0("/BBDD/indicadores/IPE/consolidado/IPE_consolidado_1979_",
                   ano_fin,"_",mes_ultimo,".csv"),
                   variable= c("spi_1", "spi_3", "spi_6", "spi_12", "spi_24")),

    IPEE = list(path=paste0("/BBDD/indicadores/IPEE/consolidado/IPEE_consolidado_1979_",
                    ano_fin,"_",mes_ultimo,".csv"),
                    variable= c("spei_1", "spei_3", "spei_6", "spei_12", "spei_24"))
  )

  #Cargamos la metadata de las estaciones de monitoreo del dataset
  metadata<-read.csv(paste0(getwd(),"/BBDD/metadatos/consolidado/estaciones_DGA_DMC.csv"))
  head(metadata)
  #Definimos la resolucion para interpolar
  resolution=0.05

  ################       EJECUTANDO INTERPOLADO     ######################
  #Finalmente interpolamnos las variables dentro de los datasets (IPE e IPEE)
  interpolated_data<-interpolate_dataset(datasets, 
                                         fecha_ini = fecha_ini_interpolacion, 
                                        fecha_fin = fecha_fin_interpolacion, 
                                        metadata , resolution ) 


  #Transformamos el raster a shapefile
  comunas_shapefile_path<-paste0(getwd(), "/BBDD/landcover/Comunas/comunas.shp")
  cuencas_shapefile_path<-paste0(getwd(), "/BBDD/landcover/Cuencas/cuencas.shp")

  comunas_clipped_data<-interpolated_data_to_combined_polygons(interpolated_data, 
                                                             datasets, 
                                                             comunas_shapefile_path)
  cuencas_clipped_data<-interpolated_data_to_combined_polygons(interpolated_data,
                                                              datasets, 
                                                              cuencas_shapefile_path)
  
  #Extraer los valores de los raster y agregar el valor medio del indicador al shapefile

  comunas_clipped_data_values <- mapply(add_mean_raster_value, 
                             comunas_clipped_data, 
                             names(comunas_clipped_data), 
                             MoreArgs = list(raster_list = interpolated_data), 
                             SIMPLIFY = FALSE)


  cuencas_clipped_data_values <- mapply(add_mean_raster_value,
                              cuencas_clipped_data, 
                              names(cuencas_clipped_data), 
                              MoreArgs = list(raster_list = interpolated_data), 
                              SIMPLIFY = FALSE)

  #agregar areas a los shapefiles y 
  #agregar atributo para poder calcular promedios st_area*variable
  comunas_clipped_data_values <- lapply(names(comunas_clipped_data_values), function(name) {
    shapefile <- comunas_clipped_data_values[[name]]
    variable_name <- str_remove(name, "X202404_")
    shapefile <- shapefile %>%
      mutate(st_area = st_area(.),
             !!paste0(variable_name, "_w") := !!sym(paste0(variable_name, "_m")) * st_area)
    return(shapefile)
  })
  comunas_clipped_data_values <- setNames(comunas_clipped_data_values, names(comunas_clipped_data)) #importante para no perder los nombres

  cuencas_clipped_data_values <- lapply(names(cuencas_clipped_data_values), function(name) {
    shapefile <- cuencas_clipped_data_values[[name]]
    variable_name <- str_remove(name, "X202404_")
    shapefile <- shapefile %>%
      mutate(st_area = st_area(.),
             !!paste0(variable_name, "_w") := !!sym(paste0(variable_name, "_m")) * st_area)
    return(shapefile)
  })
  cuencas_clipped_data_values <- setNames(cuencas_clipped_data_values, names(cuencas_clipped_data)) #importante para no perder los nombres
  
  
  #comunas_clipped_data_values[[1]]
  # Guardar los polígonos como shapefile
  # Guardar shapes de IPEE e IPE intersectados por comunas y luego cuencas
  write_shapefile_list(comunas_clipped_data_values, division = "comunas")

  write_shapefile_list(cuencas_clipped_data_values, division = "cuencas")

  ################ INDICADOR ICE ########################################
  ################       Convertimos ICE A shapefile     ######################
  #ICE input
  ICE_DGA <- list(path=paste0(getwd(),"/BBDD/indicadores/ICE/DGA/ICE_DGA_",
                               1989, "_",
                               ano_fin, "_", mes_ultimo, ".csv"),
                   variable= c("ice_1", "ice_3", "ice_6", "ice_12", "ice_24"))

  #ICE output inicializar
  ICE_DGA_shapefile<-list()

  #Cargamos la metadata de caudal
  metadata_DGA_caudal<-read.csv(paste0(getwd(),"/BBDD/metadatos/DGA/caudal/estaciones_DGA_caudal.csv"))
  #Pasamos a shapefile las mediciones puntuales del indicador ICE
  for(i in seq_along(ICE_DGA$variable)){
    select_var<-ICE_DGA$variable[i]
    ICE_DGA_shapefile[[i]]<-points_to_shapefile(read.csv(ICE_DGA$path), 
                                                metadata_DGA_caudal, 
                                                fecha_ini_interpolacion, 
                                                fecha_fin_interpolacion,
                                                select_var)
  }
  #nombrar las variables en el shapefile, clave para guardar correctamente
  names(ICE_DGA_shapefile) <- ICE_DGA$variable   #asignar nombres a los shapefiles

  ICE_DGA_shapefile <- mapply(add_classification, 
                              ICE_DGA_shapefile, 
                              names(ICE_DGA_shapefile), 
                              SIMPLIFY = FALSE)

  # Guardar los puntos como shapefile
  write_shapefile_list(ICE_DGA_shapefile, division = "points")
}





####################### FUNCIONES definidas ###################################

#Funcion de utilidad: Esta funcion sirve para arreglar un shapefile con errores
fix_shape<-function(shapefile_path){
  shape <- st_read(shapefile_path)
  shape <- st_transform(shape, crs = 4326)
  invalid_geoms <- which(!st_is_valid(shape))
  shape$geometry[invalid_geoms] <- st_make_valid(shape$geometry[invalid_geoms])
  return(shape)
}

#Funciones para Interpolar cada una de las variables en el dataset:


#1 NIVEL) La primera funcion utiliza lapply para ejecutar en 
#         cada uno de los elementos del dataset
interpolate_dataset<-function(datasets, metadata, fecha_ini, fecha_fin, resolution){
  
  
  #Aprox 10 minutos por año interpolado
  a<-Sys.time()
  # Process and interpolate each dataset
  raster_list <- lapply(names(datasets), function(name) {
    dataset_info <- datasets[[name]]
    process_and_interpolate_data(paste0(getwd(), 
                                        dataset_info$path), 
                                 dataset_info$variable, 
                                 fecha_ini, 
                                 fecha_fin, 
                                 estaciones = metadata, 
                                 resolution)
  })
  #Calculate total execution time
  b <- Sys.time()
  b-a
  cat("Total execution time:", b - a, "\n")
  return (raster_list)
}

#2 NIVEL) Esta función se encarga del manejo de datos iterando por fechas y variables
process_and_interpolate_data <- function(file_path, select_var, fecha_ini, fecha_fin, estaciones, resolution) {
  print(paste0("Proccessing ",select_var))
  # Read and preprocess the data
  df <- read_csv(file_path) %>%
    na.omit() %>%
    mutate(
      Codigo_nacional = as.character(Codigo_nacional),
      Year = as.integer(Year),
      Month = as.integer(Month)
    ) %>%
    left_join(estaciones, by="Codigo_nacional") %>%
    mutate(Fecha = as.Date(paste0(paste(Year, Month, sep = "-"), "-01"))) %>%  # Create Fecha column
    filter(Fecha >= fecha_ini, Fecha < fecha_fin) %>%  # Filter based on date range
    select(-Fecha) %>%   ##FILTERING DATA
    select(Year, Month, LONG, LAT, all_of(select_var))
  
  
  # Initialize an empty list to store each daily raster
  raster_list <- list()
  
  # Ensure there is data to process
  if (nrow(df) == 0) {
    return(NULL)  # Return NULL if no data after initial filtering
  }
  
  # Create unique identifiers for each date
  unique_days <- unique(paste(df$Year, df$Month, sep = "_"))
  unique_days <- sort(unique_days)
  
  raster_list<-NULL
  # Loop through each unique day identifier
  for (day_id in unique_days) {
    # Split the identifier to get year, month, and day
    parts <- unlist(strsplit(day_id, "_"))
    year <- as.numeric(parts[1])
    month <- as.numeric(parts[2])

    
    # Manually filter data for the current day
    current_data <- df[df$Year == year & df$Month == month, ]
    
    for(selected_var in select_var){
    # Perform interpolation only if there's data
    if (nrow(current_data) > 0) {
      
      ###llamado a INTERPOLACION:
      interpolated_raster <- interpolate_to_raster_brick(current_data, selected_var, resolution)
     
       # Store the raster with a proper name
      layer_name <- paste0("X",year, sprintf("%02d", month),"_",selected_var)
      names(interpolated_raster) <- layer_name
      raster_list[[layer_name]] <- interpolated_raster
    }
   }
  }
  
  # Combine all raster layers into a single RasterBrick
  if (length(raster_list) > 0) {
    result_raster_brick <- stack(raster_list)
    # Then convert the RasterStack to a RasterBrick
    result_raster_brick <- brick(result_raster_brick)
    return(result_raster_brick)
  } else {
    return(NULL)  # Return NULL if no valid raster layers were created
  }
}


#3 NIVEL) Esta funcion se encarga de generar una grilla para realizar la interpolacion
interpolate_to_raster_brick <- function(df, var_name, resolution) {
  # Convert dataframe to SpatialPointsDataFrame
  coordinates(df) <- ~LONG+LAT  # Defines the columns to use as coordinates
  proj4string(df) <- CRS("+proj=longlat +datum=WGS84")  # Set projection
  
  
  raster_template <- raster()
  extent(raster_template) <- c(-76, -66, -56, -17)
  res(raster_template) <- resolution
  projection(raster_template) <- CRS("+proj=longlat +datum=WGS84")
  
  ### llamado a Algoritmo de interpolacion IDW
  idw_raster <- idw_manual(df, raster_template, var_name)
  
  # Convert the single raster layer to a RasterBrick
  raster_brick <- brick(idw_raster)
  
  return(raster_brick)
}


#4 NIVEL) Se define el algoritmo IDW (Inverse distance weighted) 
idw_manual <- function(points, grid, var_name, power=2) {
  # Make sure that points are a SpatialPointsDataFrame
  if (!inherits(points, "SpatialPointsDataFrame")) {
    stop("The points argument must be a SpatialPointsDataFrame.")
  }
  
  # Extract coordinates and variable values from points
  coords <- coordinates(points)
  values <- points[[var_name]]
  
  # Prepare a matrix to store interpolated values
  interpolated_values <- rep(NA, ncell(grid))
  
  # Calculate interpolated values for each cell in the grid
  cells <- seq_len(ncell(grid))
  cell_coords <- xyFromCell(grid, cells)
  
  for (cell in cells) {
    distances <- sqrt((coords[,1] - cell_coords[cell,1])^2 + (coords[,2] - cell_coords[cell,2])^2)
    if (any(distances == 0)) {
      # Handle the case where a point falls exactly on a raster cell
      interpolated_values[cell] <- values[which.min(distances)]
    } else {
      weights <- 1 / (distances^power)
      weighted_values <- weights * values
      interpolated_values[cell] <- sum(weighted_values, na.rm = TRUE) / sum(weights, na.rm = TRUE)
    }
  }
  
  # Assign values to the raster grid
  values(grid) <- interpolated_values
  return(grid)
}


#Visualizacion de resultados:

#Funciones para graficar los resultados en formato raster
raster_plot <- function(raster_stack_indicador, shape, fecha_plot, variable, resolution, comuna_seleccionada) {
  # Custom color ramp based on breaks and colors
  color.palette <- colorRampPalette(c("red","orange","yellow","lightblue","lightgreen", "darkgreen"))
  
  fecha_plot_formated <- paste0("X", gsub("-", "", fecha_plot), "_",variable)
  # Convert shapefile to Spatial format (required for mask function)
  shape_sp <- as(shape, "Spatial")
  raster_stack_indicador <- mask(raster_stack_indicador,  shape_sp)
  
  if (all(comuna_seleccionada != "todas")){
    # Filter the shapefile to include only the selected Comuna
    shape_filtered <- shape %>% 
      filter(Comuna %in% comuna_seleccionada)
    
    # Set the extent of the plot to the bounding box of the selected Comuna
    extent_comuna <- st_bbox(shape_filtered)
    extent_raster <- raster::extent(extent_comuna["xmin"], extent_comuna["xmax"], extent_comuna["ymin"], extent_comuna["ymax"])
    
    # Plot the raster with the defined color ramp
    plot(raster_stack_indicador[[fecha_plot_formated]], 
         col = color.palette(100), 
         main = paste0("Mapa interpolado ", variable," ", fecha_plot, " res =", resolution), 
         zlim = c(-2.6, 2.6),
         xlim = c(extent_comuna["xmin"], extent_comuna["xmax"]),
         ylim = c(extent_comuna["ymin"], extent_comuna["ymax"]))
    
    # Plot filtered comunas with a thinner line
    plot(st_geometry(shape), 
         add = TRUE, 
         col = NA, 
         border = "black", 
         lwd = 1)
    
  } else {
    
    plot(raster_stack_indicador[[fecha_plot_formated]], 
         col = color.palette(100), 
         main = paste0("Mapa interpolado ", variable," ", fecha_plot, " res =", resolution), 
         zlim = c(-2.6, 2.6))
    
    # Plot filtered comunas with a thinner line
    plot(st_geometry(shape), 
         add = TRUE, 
         col = NA, 
         border = "black", 
         lwd = 1)
  }
}


#Cloropleth plot comunal
choropleth_plot<-function(raster_layer, shape, fecha_seleccionada, comuna_seleccionada){
  
  
  shape_comunal<-add_raster_means_to_shape(shape, raster_layer)
  
  # Convert PM2_5_Predicted to categorized factor
  shape_comunal<- shape_comunal %>%
    mutate(Indicador_clasificado = cut(Mean_raster_value,    
                                   breaks = c(-Inf, -2.6, -0.8, 0.8, 2.6, Inf),
                                   labels = c("Muy seco (-Inf, -2.6)",
                                              "Seco (-2.6, -0.8)",
                                              "Normal (-0.8, 0.8)",
                                              "Húmedo (0.8, 2.6)",
                                              "Muy Húmedo (2.6, Inf)"),
                                   right = FALSE)) %>%
    na.omit()   #Se elimina isla de pascua y juan fernández
  
  if (all(comuna_seleccionada != "todas")){
    shape_comunal<- shape_comunal %>% filter(Comuna %in% comuna_seleccionada)}
  # Plot the choropleth map with custom colors and adjusted transparency for the Bueno category
  p<-ggplot(data = shape_comunal) +
    geom_sf(aes(fill = Indicador_clasificado), color = "black", size = 0.1) +
    scale_fill_manual(values = c("Muy seco (-Inf, -2.6)"= "red",  # 80% transparent green
                                 "Seco (-2.6, -0.8)" = "yellow",
                                 "Normal (-0.8, 0.8)" = "lightblue",
                                 "Húmedo (0.8, 2.6)" = "blue",
                                 "Muy Húmedo (2.6, Inf)"= "darkgreen"),
                      guide = guide_legend(title = "Indicador de sequía estandarizado")) +
    labs(
      title = paste0("Nivel de sequía para ",comuna_seleccionada," en ",fecha_seleccionada),
      fill = "Indicador de sequía estandarizado"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(colour = "transparent")
    )
  print(p)
  return()
  # Save the plot as a .jpg file
  #ggsave(filename = paste0(getwd(),
  #                         "/BBDD/output/model/plots/choropleth/",comuna_seleccionada,"_",
  #                         fecha_seleccionada,".jpg"), plot = p, width = 10, height = 6, units = "in", dpi = 300)
}

add_raster_means_to_shape <- function(shape, raster_layer) {
  # Read the shapefile
  # Ensure the shape is an sf object
  if (!inherits(shape, "sf")) {
    shape <- st_as_sf(shape)
  }
  
  # Ensure the raster is a RasterLayer
  if (!inherits(raster_layer, "RasterLayer")) {
    stop("The raster input must be a single RasterLayer")
  }
  
  # Calculate the mean of the raster values within each polygon
  means <- raster::extract(raster_layer, shape, fun = mean, na.rm = TRUE, df = TRUE)
  
  # Rename the column containing the means
  colnames(means)[2] <- "Mean_raster_value"
  
  # Combine the means with the original shape data
  result_shape <- cbind(shape, Mean_raster_value = means$Mean_raster_value)
  
  return(result_shape)
  
}


raster_to_shapefile<-function(raster_data, multiple_dates = "FALSE"){
 
  layer_name<-names(raster_data)
  if(multiple_dates == "TRUE"){
  variable_name<-layer_name
  } 
  
  # Define los intervalos de reclassificación
  reclass_matrix <- matrix(c(
    -Inf, -2.05, 1,   # Extremadamente seco
    -2.05, -1.64, 2,  # Severamente seco
    -1.64, -1.28, 3,  # Muy seco
    -1.28, -1.04, 4,  # Seco
    -1.04, -0.84, 5,  # Ligeramente seco
    -0.84, 0.84, 6,   # Normal
    0.84, 1.28, 7,    # Húmedo
    1.28, Inf, 8      # Muy húmedo / Extremadamente húmedo
  ), ncol=3, byrow=TRUE)
  
  # Aplicar la reclassificación
  raster_reclass<- reclassify(raster_data, reclass_matrix)
  
  # Convertir el raster a polígonos
  polygon_data <- st_as_sf(rasterToPolygons(raster_reclass, dissolve = TRUE))
  
  # Nombre de la nueva columna basado en `layer_name`
  #classification_column <- paste0(layer_name,"_cat")
  # Actualizar el shapefile para tener categorías como cadenas de texto
  polygon_data <- polygon_data %>%
    mutate(!!layer_name := case_when(
      !!sym(layer_name) == 1 ~ "Extremadamente seco",
      !!sym(layer_name) == 2 ~ "Severamente seco",
      !!sym(layer_name) == 3 ~ "Muy seco",
      !!sym(layer_name) == 4 ~ "Seco",
      !!sym(layer_name) == 5 ~ "Ligeramente seco",
      !!sym(layer_name) == 6 ~ "Normal",
      !!sym(layer_name) == 7 ~ "Húmedo",
      !!sym(layer_name) == 8 ~ "Muy húmedo / Extremadamente húmedo",
      TRUE ~ "Desconocido"
    ))
  
  return(polygon_data)
}

interpolated_data_to_combined_polygons<-function(interpolated_data, datasets, input_shapefile_path){
  a<-Sys.time()
  # Loop through the datasets list and the interpolated_data list simultaneously
  combined_polygons <- list()
  
  # Loop through the datasets list and the interpolated_data list simultaneously
  for (i in seq_along(datasets)) {
    for (j in names(interpolated_data[[i]])) {
      dataset_info <- datasets[[i]]
      variable_name <- gsub("^[^_]*_", "",j)
      
      combined_polygons[[j]] <- raster_to_shapefile(interpolated_data[[i]][[j]])
      print(paste0("Transforme el dataset ", 
                   names(datasets)[i], 
                   " variable ", 
                   variable_name, 
                   " en poligonos"))
    }
  }
  
  
  # Obtener el shapefile de entrada y asegurarse de que ambos tengan el mismo sistema de coordenadas
  shape <- fix_shape(input_shapefile_path)
  
  # Aplicar la intersección a cada shapefile en combined_polygons
  for (j in names(combined_polygons)) {
    if (st_crs(combined_polygons[[j]]) != st_crs(shape)) {
    shape <- st_transform(shape, st_crs(combined_polygons[[1]]))
    }
    combined_polygons[[j]] <- st_intersection(combined_polygons[[j]], shape)
    print(paste0("Intersecte con el shapefile la variable ", j))
  }
  
  # Modificar los nombres
  # Extraer el prefijo repetitivo
  prefix <- sub("_.*$", "", colnames(combined_polygons[[1]])[1])
  prefix_date<-paste0(substr(gsub("X","",prefix),1,4),"-",substr(gsub("X","",prefix),5,6))
  for(i in seq_along(names(combined_polygons))) {
    combined_polygons[[i]]<- combined_polygons[[i]] %>% 
      mutate(Fecha = prefix_date)
    for(j in seq_along(colnames(combined_polygons[[i]])))
      # Verifica si el nombre contiene el prefijo
      if (startsWith(colnames(combined_polygons[[i]])[j], prefix)) {
        # Dividir por "." y obtener la segunda parte
        colnames(combined_polygons[[i]])[j]<-gsub(paste0(prefix,"_"), 
                                                   "", 
                                                   colnames(combined_polygons[[i]])[j])
      }
  }
  
   # Function to rename geometry column
  rename_geometry_column <- function(sf_object) {
    new_geom_name <- paste0(names(sf_object)[1], ".geometry")
    names(sf_object)[ncol(names(sf_object))] <- new_geom_name
    st_geometry(sf_object) <- new_geom_name  # Set the new geometry column
    sf_object
  }
  
  # Apply the renaming function to each sf object in the list
  combined_polygons <- combined_polygons %>%
    purrr::map(rename_geometry_column)
  
  # Combine all shapefiles into a single sf object
  # Esta parte no se incluye ya que ArcgisOnline solo permite subir shapefiles
  # Con UNA geometría activa. Por lo tanto debemos subir cada geometria como
  # un archivo separado.
  # hubiese ideal subir solo un archivo por indicador pero no se puede)
  # merged_shapefile <- combined_polygons %>%
  # purrr::map_dfr(~ as_tibble(.))  %>%  # Convert sf objects to tibbles and bind rows
  # st_as_sf()  # Convert back to sf object
  
  b<-Sys.time()
  print(paste0("Raster to polygons demora ", b-a))
  return(combined_polygons)
}

# Guardar los polígonos como shapefile
write_shapefile_list <- function(final_clipped_data, division) {
  for (i in seq_along(names(final_clipped_data))) {
    variable_name <- names(final_clipped_data)[i]
    
    # Determinar el indicador (SPI, SPEI, o ICE) basado en el nombre de la variable
    if (grepl("_spi_", variable_name, ignore.case = TRUE)) {
      indicador <- "SPI"
    } else if (grepl("_spei_", variable_name, ignore.case = TRUE)) {
      indicador <- "SPEI"
    } 
      else if (grepl("ice_", variable_name, ignore.case = TRUE)) {
      indicador <- "ICE"
    }
      else {
      stop(paste("El nombre de la variable", variable_name, "no corresponde a un indicador válido (SPI, SPEI o ICE)"))
    }

    if((division == "points")){
      base_name <- variable_name
      final_name <- variable_name
    }
    else{
    # Remover el prefijo de fecha y agregar "_comunas"
      base_name <- sub("^[^_]*_", "", variable_name)
      final_name <- paste0(base_name, "_", division)
    }
    
    # Construir la nueva ruta de salida
    output_dir <- file.path(getwd(),"BBDD", "mapas", indicador, division, base_name)
    output_shapefile_path <- file.path(output_dir, paste0(final_name, ".shp"))
    
    # Verificar si el directorio existe y crearlo si no existe
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Guardar el shapefile combinado
    st_write(final_clipped_data[[i]], output_shapefile_path)
    
    # Crear un archivo ZIP con todos los archivos correspondientes al shapefile
    zipfile_path <- file.path(output_dir, paste0(final_name, ".zip"))
    files_to_zip <- c(output_shapefile_path, 
                      file.path(output_dir, paste0(final_name, ".prj")), 
                      file.path(output_dir, paste0(final_name, ".dbf")),
                      file.path(output_dir, paste0(final_name, ".shx")))
    
    # Cambiar el directorio de trabajo al directorio de los archivos a comprimir
    old_wd <- setwd(output_dir)
    
    # Comprimir los archivos usando zip() asegurándose de que solo se incluyan nombres de archivo relativos
    zip(basename(zipfile_path), files = basename(files_to_zip))
    
    # Volver al directorio de trabajo original
    setwd(old_wd)
    
    # Eliminar los archivos originales después de comprimir
    file.remove(files_to_zip)
  }
}

# Guardar los polígonos como shapefile
plot_shapefile<-function(shapefile, variable){
  
  geometria<-paste0(variable,".geometry")
  # Cambia la geometría activa
  shapefile <- st_set_geometry(shapefile, geometria)
  # Definir una paleta de colores para las categorías
  colors <- c(
    "Extremadamente seco" = "red",
    "Severamente seco" = "brown",
    "Muy seco" = "orange",
    "Seco" = "yellow",
    "Ligeramente seco" = "lightyellow",
    "Normal" = "lightblue",
    "Húmedo" = "lightgreen",
    "Muy húmedo / Extremadamente húmedo" = "darkgreen",
    "Desconocido" = "grey"
  )
  
  # Crear el gráfico
  ggplot(data = shapefile) +
    geom_sf(aes_string(fill =gsub(".geometry","",variable)), color = "black") +
    scale_fill_manual(name = gsub(".geometry","",variable), values = colors) +
    ggtitle(paste0("Interpolado del indicador ", gsub(".geometry","",variable))) +
    theme_minimal()
  
}

points_to_shapefile<-function(df, metadata, fecha_ini, fecha_fin, select_var){
  df <- df%>%
    na.omit() %>%
    mutate(
      Codigo_nacional = as.character(Codigo_nacional),
      Year = as.integer(Year),
      Month = as.integer(Month)
    ) %>%
    left_join(metadata, by="Codigo_nacional") %>%
    mutate(Fecha = as.Date(paste0(paste(Year, Month, sep = "-"), "-01"))) %>%  # Create Fecha column
    filter(Fecha >= fecha_ini, Fecha < fecha_fin) %>%  # Filter based on date range
    select(-Fecha) %>%   ##FILTERING DATA
    select(Year, Month, LONG, LAT, Nombre, Altura, NOM_REG, NOM_COM,NOM_PROV, NOM_CUEN, NOM_SUBC, all_of(select_var)) %>%
    mutate(LONG = as.numeric(LONG),
           LAT = as.numeric(LAT))

  # Convert dataframe to SpatialPointsDataFrame
  coordinates(df) <- ~LONG+LAT  # Defines the columns to use as coordinates
  proj4string(df) <- CRS("+proj=longlat +datum=WGS84")  # Set projection
  shape<-st_as_sf(df)
  print(paste0("Variable ",select_var," convertida en shapefile"))
  return(shape)
}


add_mean_raster_value <- function(shapefile, shapefile_name, raster_list) {
  # Remove the date prefix to get the new attribute name
  new_attribute_name <- gsub("X[0-9]+_", "", shapefile_name)
  new_attribute_name <- paste0(new_attribute_name, "_m")
  for (raster_brick in raster_list) {
    if (shapefile_name %in% names(raster_brick)) {
      raster_layer <- raster_brick[[shapefile_name]]
      mean_value <- raster::extract(raster_layer, shapefile, fun = mean, na.rm = TRUE)
      paste0(print("Se agregaron los valores de la variable "), shapefile_name)
      shapefile[[new_attribute_name]] <- mean_value
      return(shapefile)
    }
  }
  return(shapefile)
}

add_area_to_shape <- function(shape) {
  # Leer el shapefile
  # Asegurarse de que el shape sea un objeto sf
  if (!inherits(shape, "sf")) {
    shape <- st_as_sf(shape)
  }
  # Calcular el área de cada polígono
  shape$Area <- st_area(shape)
  return(shape)
}

add_classification <- function(shapefile, shapefile_name) {
  # Define reclassification matrix with category labels
  reclass_matrix <- matrix(c(
    -Inf, -2.05, "Extremadamente seco",   # Extremadamente seco
    -2.05, -1.64, "Severamente seco",  # Severamente seco
    -1.64, -1.28, "Muy seco",  # Muy seco
    -1.28, -1.04, "Seco",  # Seco
    -1.04, -0.84, "Ligeramente seco",  # Ligeramente seco
    -0.84, 0.84, "Normal",   # Normal
    0.84, 1.28, "Húmedo",    # Húmedo
    1.28, Inf, "Muy húmedo / Extremadamente húmedo"      # Muy húmedo / Extremadamente húmedo
  ), ncol=3, byrow=TRUE)
  new_attribute_name <- paste0(shapefile_name, "_cat")
  shapefile <- shapefile %>%
    mutate(!!new_attribute_name := case_when(
      !!sym(shapefile_name) <= as.numeric(reclass_matrix[1, 2]) ~ reclass_matrix[1, 3],
      !!sym(shapefile_name) > as.numeric(reclass_matrix[2, 1]) & !!sym(shapefile_name) <= as.numeric(reclass_matrix[2, 2]) ~ reclass_matrix[2, 3],
      !!sym(shapefile_name) > as.numeric(reclass_matrix[3, 1]) & !!sym(shapefile_name) <= as.numeric(reclass_matrix[3, 2]) ~ reclass_matrix[3, 3],
      !!sym(shapefile_name) > as.numeric(reclass_matrix[4, 1]) & !!sym(shapefile_name) <= as.numeric(reclass_matrix[4, 2]) ~ reclass_matrix[4, 3],
      !!sym(shapefile_name) > as.numeric(reclass_matrix[5, 1]) & !!sym(shapefile_name) <= as.numeric(reclass_matrix[5, 2]) ~ reclass_matrix[5, 3],
      !!sym(shapefile_name) > as.numeric(reclass_matrix[6, 1]) & !!sym(shapefile_name) <= as.numeric(reclass_matrix[6, 2]) ~ reclass_matrix[6, 3],
      !!sym(shapefile_name) > as.numeric(reclass_matrix[7, 1]) & !!sym(shapefile_name) <= as.numeric(reclass_matrix[7, 2]) ~ reclass_matrix[7, 3],
      !!sym(shapefile_name) > as.numeric(reclass_matrix[8, 1]) ~ reclass_matrix[8, 3],
      TRUE ~ NA_character_
    ))
  shapefile[[new_attribute_name]] <- factor(shapefile[[new_attribute_name]], levels = reclass_matrix[, 3])

  shapefile <- shapefile %>%
  mutate(Macrozona = case_when(
    NOM_REG %in% c("Región de Antofagasta", "Región de Arica y Parinacota", "Región de Atacama", 
                  "Región de Coquimbo", "Región de Tarapacá") ~ "Norte",
    NOM_REG %in% c("Región de Valparaíso", "Región Metropolitana de Santiago", "Región del Maule", 
                  "Región del Libertador Bernardo O'Higgins", "Región del Bío-Bío", 
                  "Región de Ñuble", "Región de La Araucanía") ~ "Centro",
    NOM_REG %in% c("Región de Los Ríos", "Región de Los Lagos", "Región de Aysén del Gral.Ibañez del Campo", 
                  "Región de Magallanes y Antártica Chilena") ~ "Sur",
    TRUE ~ "Desconocido" # Para manejar cualquier región que no esté clasificada
  )) %>% 
  mutate(Fecha = paste0(Year, "-", sprintf("%02d", Month)))


  return(shapefile)
}






