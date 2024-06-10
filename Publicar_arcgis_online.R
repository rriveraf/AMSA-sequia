#install.packages("devtools")
#devtools::install_github("R-ArcGIS/r-bridge")
#install.packages("httr")
#install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")

subir_mapas <- function(directorio_base){
# Cargar simbología y parámetros del mapa interactivo
  source(paste0(directorio_base,"/Web_map_renderer.R"))
  # Check the ArcGIS product version MUST BE UP TO DATE!
  arc.check_product()
  #Usuario y clave del servidor Arcgis Online federado del CCG
  usuario<-"Cambioglobal"
  clave<-"Cambioglobaluc2023"
  file_types<-c("Shapefile", "Feature Service", "Web Map")

  #Definimos las variables y el tipo de archivo que vamos a trabajar
  datasets<- list (
  variables_spi_comunas <- c("spi_1_comunas", "spi_3_comunas", "spi_6_comunas", "spi_12_comunas", "spi_24_comunas"),

  variables_spei_comunas<- c("spei_1_comunas", "spei_3_comunas", "spei_6_comunas", "spei_12_comunas", "spei_24_comunas"),

  variables_spi_cuencas <- c("spi_1_cuencas", "spi_3_cuencas", "spi_6_cuencas", "spi_12_cuencas", "spi_24_cuencas"),

  variables_spei_cuencas<- c("spei_1_cuencas", "spei_3_cuencas", "spei_6_cuencas", "spei_12_cuencas", "spei_24_cuencas"),

  variables_ice <- c("ice_1", "ice_3", "ice_6", "ice_12", "ice_24")
  )
  ###############################################################################
  #################              REST API:                       ################
  ################# Manejo de archivos y mapas en Arcgis Online #################
  ###############################################################################

  #Recibimos token de identificación para usar la API REST de Arcgis Online #####
  token<-login_arcgis(usuario, clave)                 
  ###############################################################################
  #El for recorre todas las variables a trabajar en el dataset
  for(j in seq_along(datasets)){

    variable_names <- datasets[[j]]

    for(i in seq_along(variable_names)){

      variable<-variable_names[i] #Se recorren todas las variables
      print(paste0("Procesando variable ", variable))

      directorio_zip <- get_directory(variable, directorio_base) #Directorio donde se encuentran los archivos zip

      
      zipfile_path <- file.path(directorio_zip, paste0(variable, ".zip")) # Archivos de origen
      
      print(zipfile_path)


      #Shapefile
      #Buscamos el shapefile
      shapefile_to_update <- tryCatch({
        buscar_archivo(token, variable, file_types[1])
      }, error = function(e) {
        message("No se encontró ",variable,". --> Uploading.", e$message)
        NULL  # Return NULL or any default value in case of error
      })
      #En el caso de que no exista el shapefile, debemos subir shape y feature service:
      if(is.null(shapefile_to_update)){
      upload_shapefile_arcgis(zipfile_path, token, variable)  #Si no existe, lo subimos
      }  else {


      print(paste0("Se encontró ",variable,". --> Updating"))
      #Si existe el shapefile, entonces Actualizamos:
      result<-update_shapefile_arcgis(token, shapefile_to_update$id, zipfile_path) 
      #Actualizamos Feature Service
      #Buscar
      feature_to_update<-buscar_archivo(token, variable, file_types[2])
      #Borramos el Feature Service anterior
      #(porque la funcion update end-point no funciona bien)
      delete_result<-delete_item(token, item_id = feature_to_update$id)     
      delete_result   

      #Publicamos una nueva Feature Service
      feature<-publicar_shapefile(feature_to_update$title, shapefile_to_update$id, token) 

      #Actualizar el Web Map con la capa del Feature Service
      ## Esta funcion requiere de "/web_map_renderer.R"
      # Para modificar parametros de visualización del mapa referirse a "/web_map_renderer.R"
      #tipo_capa<-check_variable_type(variable)

      #web_map_updated<-update_web_map(variable, token, tipo_capa) 

      }
    }   
  }
}











######################## FUNCIONES  #############################

### LOGIN ARCGIS
login_arcgis<-function(usuario, clave){
token_url <- "https://www.arcgis.com/sharing/rest/generateToken"
response <- POST(token_url, body = list(
  username = usuario,
  password = clave,
  referer = "http://www.arcgis.com",
  f = "json"
))
token <- content(response)$token
if(!is.null(token)){
  print("Login successfull, devolviendo token")
return(token)
  } else{
  print("Login fallido, revisar credenciales y/o servidor online 'https://www.arcgis.com' ")
  return(0)
    }
}

#obtener directorio de lectura de archivos zip
get_directory <- function(variable, directorio_base) {
  if (grepl("spi", variable, ignore.case = TRUE)) {
    if (grepl("comunas", variable, ignore.case = TRUE)) {
      return(file.path(directorio_base, "BBDD", "mapas", "SPI", "comunas", sub("_comunas$", "", variable)))
    } else if (grepl("cuencas", variable, ignore.case = TRUE)) {
      return(file.path(directorio_base, "BBDD", "mapas", "SPI", "cuencas", sub("_cuencas$", "", variable)))
    }
  } else if (grepl("spei", variable, ignore.case = TRUE)) {
    if (grepl("comunas", variable, ignore.case = TRUE)) {
      return(file.path(directorio_base, "BBDD", "mapas", "SPEI", "comunas", sub("_comunas$", "", variable)))
    } else if (grepl("cuencas", variable, ignore.case = TRUE)) {
      return(file.path(directorio_base, "BBDD", "mapas", "SPEI", "cuencas", sub("_cuencas$", "", variable)))
    }
  } else if (grepl("ice", variable, ignore.case = TRUE)) {
    return(file.path(directorio_base, "BBDD", "mapas", "ICE", "points", variable))
  }
  return(NULL)  # Retorna NULL si no coincide con ninguna condición
}
############### UPLOAD A SHAPEFILE
upload_shapefile_arcgis<-function(zipfile_path, token, variable_name){

  # Subir el shapefile a ArcGIS Online
  item_url <- "https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/addItem"
  upload_response <- POST(item_url, body = list(
    f = "json",
    token = token,
    title = paste0(variable_name),
    type = "Shapefile",
    tags = paste0("raster, polígono, interpolado, ",variable_name),
    file = upload_file(zipfile_path)
  ))
  upload_info <- content(upload_response)
  print(upload_info)
  print("He subido el shapefile, procedo a publicar feature layer")

  # Definir los parámetros de publicación
  publish_parameters <- list(
    hasStaticData = TRUE,
    name = paste0(variable_name),
    filetype = "shapefile"
  )

  # Convertir los parámetros a JSON
  publish_parameters_json <- toJSON(publish_parameters, auto_unbox = TRUE)

  # URL del servicio de publicación
  publish_url <- "https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/publish"

  # Publicar el shapefile
  publish_response <- POST(publish_url, body = list(
    itemID = upload_info$id,
    filetype = "shapefile",
    publishParameters = publish_parameters_json,
    f = "json",
    token = token
  ))
  publish_info <- content(publish_response)
  # Verificar la respuesta de la publicación

  print(paste0("He Publicado la Feature Layer id ", publish_info$services[[1]]$serviceItemId))

  #Publicar el Web Map
  #Publicar un nuevo Web Map
  result_web_map<-upload_web_map_arcgis(token, variable_name) 

  Sys.sleep(10)   ##Esperar a que se termine de publicar el mapa, de lo contrario la siguiente operación falla
  print(paste0("He Publicado el Web Map id ", result_web_map$id))

  #Actualizar el Web Map con la capa del Feature Service
  ## Esta funcion requiere de "/web_map_renderer.R"
  # Para modificar parametros de visualización del mapa referirse a "/web_map_renderer.R"

  web_map_updated<-update_web_map(variable_name, token) 
  print("Feature layer agregada al mapa satisfactoriamente")

  print("Procedo a compartir públicamente los archivos subidos")
  # Compartir el shapefile publicado de forma pública
  share_url <- "https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/shareItems"
  share_response <- POST(share_url, body = list(
    f = "json",
    token = token,
    everyone = "true",
    items = upload_info$id
  ))
  share_info <- content(share_response)


  # Compartir la capa de entidades publicada de forma pública
  share_url <- "https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/shareItems"
  share_response <- POST(share_url, body = list(
    f = "json",
    token = token,
    everyone = "true",
    items = publish_info$services[[1]]$serviceItemId
  ))
  share_info <- content(share_response)

  # Compartir el Web Map publicado de forma pública
  share_url <- "https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/shareItems"
  share_response <- POST(share_url, body = list(
    f = "json",
    token = token,
    everyone = "true",
    items = web_map_updated$id
  ))
  share_info <- content(share_response)


  print("He compartido publicamente el shapefile, la feature layer y el web map")
}





#### BUSCAR ARCHIVO

buscar_archivo<-function(token, layer_name, file_type){
buscar_item_url <- "https://www.arcgis.com/sharing/rest/search"
buscar_item_response <- GET(buscar_item_url, query = list(
  f = "json",
  token = token,
  q = paste0("title:",layer_name," AND owner:Cambioglobal"),
  num = 10
))
buscar_item_info <- content(buscar_item_response, as = "parsed", type = "application/json")
if (is.null(buscar_item_info$results)) {
  print(paste0("No se encontró el ", file_type," buscado."))
  return(NULL)
} else {
for(i in seq_along(buscar_item_info$results)){
  if (buscar_item_info$results[[i]]$title == paste0(layer_name) &&
      buscar_item_info$results[[i]]$type == file_type){
    results<-buscar_item_info$results[[i]]
    } 
  }
}
  if (is.null(results)) {
    print(paste0("No se encontró el ", file_type," buscado, devolviendo NULL"))
    return(NULL)
  } else {
 print(paste0("Encontrado ",results$title, " tipo ", results$type, " id: ", results$id ))
 return(results)
    
  }
}




### UPDATE SHAPEFILE
update_shapefile_arcgis <- function(token, item_id_shapefile, zipfile_path) {
  
  
  # Sobrescribir el shapefile existente
  item_url <- paste0("https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/items/", item_id_shapefile, "/update")
  upload_response <- POST(item_url, body = list(
    f = "json",
    token = token,
    file = upload_file(zipfile_path)
  ))
  upload_info <- content(upload_response)
  
  if (is.null(upload_info)) {
    stop(paste("Error al sobrescribir el shapefile:", upload_info$error$message))
  }
  
  # Compartir el shapefile publicado de forma pública
  share_url <- "https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/shareItems"
  share_response <- POST(share_url, body = list(
    f = "json",
    token = token,
    everyone = "true",
    items = upload_info$id
  ))
  share_info <- content(share_response)
  
  print("Archivo sobrescrito exitosamente.")
  return(upload_info)
}

publicar_shapefile<-function(item_title_feature, item_id_shapefile, token){
  
  # Pubblicar la Feature Service
  publish_url <- "https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/publish"
  publish_parameters <- list(
    hasStaticData = TRUE,
    name = paste0(item_title_feature),
    filetype = "shapefile"
  )
  publish_parameters_json <- toJSON(publish_parameters, auto_unbox = TRUE)

  publish_response <- POST(publish_url, body = list(
    itemID = item_id_shapefile,
    filetype = "shapefile",
    publishParameters = publish_parameters_json,
    f = "json",
    token = token
  ), encode = "form")
  publish_info <- content(publish_response)

  if (!is.null(publish_info$error)) {
    stop(paste("Error al publicar la capa de entidades:", feature$services[[1]]$serviceItemId))
  }


  # Compartir la capa de entidades publicada de forma pública
  share_url <- "https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/shareItems"
  share_response <- POST(share_url, body = list(
    f = "json",
    token = token,
    everyone = "true",
    items = publish_info$services[[1]]$serviceItemId
  ))
  share_info <- content(share_response)

  print(paste0("Publicado ",item_title_feature," id ", publish_info$services[[1]]$serviceItemId))




  return(publish_info)
}



#### DELETE ITEM
delete_item<-function(token, item_id){
delete_url <- paste0("https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/items/", item_id, "/delete")
delete_response <- POST(delete_url, body = list(
  f = "json",
  token = token
))

delete_info <- content(delete_response)

if (!is.null(delete_info$error)) {
  stop(paste("Error al eliminar la capa temporal:", delete_info$error$message))
}

#print(paste0("Capa id ", item_id, " eliminada exitosamente."))
return(delete_info)
}



#### UPLOAD WEB MAP


upload_web_map_arcgis <- function(token, web_map_title) {
  # URL for uploading the web map
  upload_url <- "https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/addItem"
  
  # Create a simple web map JSON structure
  web_map_json <- list(
    operationalLayers = list(),
    baseMap = list(
      baseMapLayers = list(
        list(
          id = "defaultBasemap",
          layerType = "ArcGISTiledMapServiceLayer",
          url = "https://services.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer"
        )
      ),
      title = "Basemap"
    ),
    #agregando extensión inicial del mapa y fijando el zoom inicial
    initialState = list(
      viewpoint = list(
        targetGeometry = list(
          spatialReference = list(
            latestWkid = 3857,
            wkid = 102100
          ),
          xmin = -8460884,
          ymin = -6040210,  # Shifted south
          xmax = -7421565,
          ymax = -2410045   # Shifted south
        )
      )
    ),
    mapOptions = list(
      preserveScale = TRUE
    ),    ###termina definición del extent inicial
    version = "2.0"
  )
  
  # Upload the web map
  upload_response <- POST(upload_url, body = list(
    f = "json",
    token = token,
    title = web_map_title,
    type = "Web Map",
    typeKeywords = "ArcGIS Online, Explorer Web Map, Map, Online Map, Web Application",
    tags = web_map_title,
    text = toJSON(web_map_json, auto_unbox = TRUE)
  ), encode = "multipart")
  
  # Check the response
  response_content <- content(upload_response, as = "text", encoding = "UTF-8")
  response_json <- fromJSON(response_content)
  
  # Check for success field in the response
  if (!is.null(response_json$success) && response_json$success) {
    print("Web map uploaded successfully.")
    return(response_json)
  } else {
    if (!is.null(response_json$error)) {
      stop(paste("Error uploading web map:", response_json$error$message))
    } else {
      stop("Unknown error occurred during web map upload.")
    }
  }
}


#### UPDATE WEB MAP
update_web_map<-function(variable, token){
  
  
  
  # Cargamos la capa de division administrativa o hidrografica
  division<-extract_suffix(variable)
  if(division == "comunas"){
    feature_service_url <- "https://services1.arcgis.com/NRDRKdGDhxj8Z4QX/arcgis/rest/services/comunas_ExportFeatures/FeatureServer"
  }
  if(division == "cuencas"){
    feature_service_url <- "https://services1.arcgis.com/NRDRKdGDhxj8Z4QX/arcgis/rest/services/cuencas_ExportFeatures/FeatureServer/0"
  }
  if(grepl("ice", variable)){
    feature_service_url <- "https://services1.arcgis.com/NRDRKdGDhxj8Z4QX/arcgis/rest/services/Red_Hidrografica/FeatureServer/0"
  }
  
  feature_service_response <- GET(feature_service_url, query = list(f = "json", token = token))
  Sys.sleep(2)
  feature_service_content <- content(feature_service_response, as = "parsed")
  
  if (is.null(feature_service_content)) {
    stop(paste("Error al obtener el contenido del Feature Service:", division))
  }
  
  # Extract the first layer from the Feature Service
  division_layer <- list(
    id = division,
    itemId = feature_service_content$serviceItemId,
    title = division,  # Define the appropriate title
    url = feature_service_url,
    layerType = "ArcGISFeatureLayer",
    popupInfo = list(),  # Set popupInfo to NULL to disable pop-ups
    disablePopUp = TRUE, # To disable pop-ups
    drawingInfo = list(
      renderer = list(
        type = "simple",
        symbol = list(
          type = "esriSFS",
          style = "esriSFSSolid",
          color = list(0, 0, 0, 0),  # Example color: semi-transparent red
          outline = list(
            type = "esriSLS",
            style = "esriSLSSolid",
            color = list(0, 0, 0, 255),
            width = 0.1
          )
        )
      )
    ),
    visibility = TRUE,  # Ensure the layer is visible
    minScale = 0,       # Optional: Define the minimum scale
    maxScale = 0        # Optional: Define the maximum scale
  )
  
  
  
  
  tipo_capa<-check_variable_type(variable)
  # Descargamos el contenido del Feature service
  feature_to_update<-buscar_archivo(token, variable, file_types[2])
  feature_service_url <- feature_to_update$url
  feature_service_response <- GET(feature_service_url, query = list(f = "json", token = token))
  Sys.sleep(2)
  feature_service_content <- content(feature_service_response, as = "parsed")
  
  if (is.null(feature_service_content)) {
    stop(paste("Error al obtener el contenido del Feature Service:", variable))
  }
  
  # Extract the first layer from the Feature Service
  feature_service_layer_url <-paste0("https://services1.arcgis.com/NRDRKdGDhxj8Z4QX/arcgis/rest/services/", feature_to_update$title, "/FeatureServer/0")
  new_feature_layer <- list(
    id = feature_to_update$id,
    itemId = feature_to_update$id,
    title = feature_to_update$title,  # Define the appropriate title
    url = feature_service_layer_url,
    layerType = "ArcGISFeatureLayer",
    popupInfo = pop_up_info_default(extract_prefix(variable))
  )
  
  
  #Descargamos el contenido del Web Map
  webmap_to_update<-buscar_archivo(token, variable, file_types[3]) 
  web_map_id<-webmap_to_update$id
  web_map_url <- paste0("https://www.arcgis.com/sharing/rest/content/items/", web_map_id, "/data")
  web_map_response <- GET(web_map_url, query = list(f = "json", token = token))
  web_map_content <- content(web_map_response, as = "parsed")
  
  if (!is.null(web_map_content$error)) {
    stop(paste("Error al obtener el contenido del Web Map:", web_map_content$error$message))
  }
  
  print("Contenido del Web Map descargado exitosamente.")
  
  # Remove the existing operational layers
  web_map_content$operationalLayers <- NULL
  
  # Actualizar el Web Map con la nueva layer:
  web_map_content$operationalLayers <- append(web_map_content$operationalLayers, list(division_layer, new_feature_layer))
  
  # Definir simbología de una capa
  #if(tipo_capa == "poligonos"){
    web_map_content$operationalLayers[[2]]$layerDefinition$drawingInfo<-drawing_info_default(variable)
  #}
  # Convert the updated web map content to JSON
  web_map_content_json <- toJSON(web_map_content, auto_unbox = TRUE)
  
  # Update the Web Map
  update_url <- paste0("https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/items/", web_map_id, "/update")
  update_response <- POST(update_url, body = list(
    f = "json",
    token = token,
    text = web_map_content_json
  ), encode = "form")
  update_info <- content(update_response, as = "parsed")
  
  if (!is.null(update_info$error)) {
    stop(paste("Error al actualizar el Web Map:", update_info$error$message))
    return(NULL)
  }
  
  # Compartir el Web Map publicado de forma pública
  share_url <- "https://www.arcgis.com/sharing/rest/content/users/Cambioglobal/shareItems"
  share_response <- POST(share_url, body = list(
    f = "json",
    token = token,
    everyone = "true",
    items = web_map_id
  ))
  share_info <- content(share_response)
  
  print("Web Map actualizado exitosamente.")
  return(update_info)
}


###### Funciones de utilidad
check_variable_type <- function(variable_name) {
  
  variables_spi_comunas <- c("spi_1_comunas", "spi_3_comunas", "spi_6_comunas", "spi_12_comunas", "spi_24_comunas")
  variables_spei_comunas<- c("spei_1_comunas", "spei_3_comunas", "spei_6_comunas", "spei_12_comunas", "spei_24_comunas")
  variables_spi_cuencas <- c("spi_1_cuencas", "spi_3_cuencas", "spi_6_cuencas", "spi_12_cuencas", "spi_24_cuencas")
  variables_spei_cuencas<- c("spei_1_cuencas", "spei_3_cuencas", "spei_6_cuencas", "spei_12_cuencas", "spei_24_cuencas")
  variables_ice <- c("ice_1", "ice_3", "ice_6", "ice_12", "ice_24")
  
  if (variable_name %in% variables_ice) {
    return("puntos")
  } else if (variable_name %in% c(variables_spei_comunas, variables_spi_comunas, variables_spei_cuencas, variables_spi_cuencas)) {
    return("poligonos")
  } else {
    message <- paste0("Error: '", variable_name, "' No se encontró dentro de las variables del dataset.")
    print(message)
    return(NULL)
  }
}

# Function to extract suffix or return NULL
extract_suffix <- function(variable) {
  suffix <- sub(".*_[0-9]+_?", "", variable)
  return(suffix)
}

extract_prefix <- function(variable) {
  prefix <- sub("(_[0-9]+).*$", "\\1", variable)
  return(prefix)
}