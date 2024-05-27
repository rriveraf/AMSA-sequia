library(raster)
library(tidyverse)
library(sp)
library(sf)
library(lubridate)
library(readxl)
#Estas script conti9ene funciones de utilidad para interpolar un dataset multivariado

#############      CARGANDO PARAMETROS       ##########################
#Se cargan las fechas de inicio y finales de los datasets
fecha_ini<-1979
fecha_fin<-year(as.Date(Sys.time()))
month_fin<-as.numeric(month(as.Date(Sys.time()))-1)

#Variables disponibles en el dataset
variables<-c("spi_1", "spi_3", "spi_6", "spi_12", "spi_24")

#Definimos los elmentos en el dataset, cada uno con un "path" y "variable"
#NOTA:: Se pueden agregar mas elementos al dataset con una coma alfinal
datasets <- list(
  IPE_DGA = list(path=paste0("/IPE_DGA_",
                         fecha_ini,"_",
                         fecha_fin,"_",month_fin,".csv"),
                 variable= variables)
)


#Cargamos la metadata de las estaciones de monitoreo del dataset
metadata<-read_xlsx(paste0(getwd(),"/estaciones_DGA_pp_y_temp.xlsx"))

#Definimos la resolucion para interpolar
resolution=0.05



################       EJECUTANDO INTERPOLADO     ######################

#Finalmente interpolamnos las variables dentro de los datasets
interpolated_data<-interpolate_dataset(datasets, 
                                       fecha_ini = "2024-03-01", 
                                      fecha_fin  ="2024-05-01", 
                                      metadata , resolution ) 



################  VISUALIZANDO RESULTADOS     ######################

#Cargamos shapefile de Chile para poder visualizar resultados
shapefile_path<-paste0(getwd(), "/BBDD/landcover/Comunas/comunas.shp")

#Definimos un shapefile saludable
shape<-fix_shape(shapefile_path)

#Graficamos resultados en un plot con un raster y un shapefile sobrepuesto
raster_plot(interpolated_data[[1]], 
            shape, 
            fecha_plot = "2024-04", 
            variables[4], 
            resolution, 
            comuna_seleccionada = "todas")


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
  extent(raster_template) <- c(-75, -66, -56, -17)
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


##################################################################################################

output_dir<- paste0(getwd(),"/BBDD/output/climate_data/rasters/")
# Loop through the datasets list and the interpolated_data list simultaneously
for (i in seq_along(datasets)) {
  # Extract the filename from the dataset path
  output_filename <- datasets[[i]]$variable
  output_filename<-paste0(output_filename,"_" ,fecha_ini,"_",fecha_fin,".tif")
  # Full path to the output file
  full_output_path <- file.path(output_dir, output_filename)
  # Save the raster
  writeRaster(interpolated_data[[i]], full_output_path, format="GTiff", overwrite=TRUE)
}