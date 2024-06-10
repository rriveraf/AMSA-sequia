#Este script contiene los parametros necesarios para definir una 
#Simbología específica en un web map de Arcgis Online.
#Además permite definir otros parámetros de la visualziacion itneractiva de mapas
#Como los pop ups, entre otros. 
drawing_info_default<-function(variable){
  
  
  categories <- c(
    "Extremadamente seco",
    "Severamente seco",
    "Muy seco",
    "Seco",
    "Ligeramente seco",
    "Normal",
    "Húmedo",
    "Muy húmedo / Extremadamente húmedo"
  )
  
  # Definir una paleta de colores para las categorías en formato RGB
  colors <- list(
    c(158, 58, 38, 128),    # Extremadamente seco
    c(198, 74, 34, 128),    # Excesivamente seco
    c(235, 108, 28, 128),   # Muy seco
    c(246, 158, 65, 128),   # Seco
    c(244, 209, 102, 128),  # Levemente seco
    c(160, 203, 232, 128),  # Normal
    c(78, 121, 167, 128),   # Humedo
    c(89, 161, 79, 128)    # Muy humedo / Extremadamente húmedo
  )
  
  
  
  if(grepl("ice", variable)){
   atributo_name<-paste0(variable,"_cat")
   
   uniqueValueInfos <- lapply(seq_along(categories), function(i) {
     list(
       value = categories[i],
       label = categories[i],
       symbol = list(
         type = "esriSMS",  # Simple Marker Symbol for point features
         style = "esriSMSCircle",  # Circle style
         color = colors[[i]],
         angle = 0,
         xoffset = 0,
         yoffset = 0,
         size = 7,  # Specify a size for the points
         outline = list(
           type = "esriSLS",
           style = "esriSLSSolid",
           color = c(0, 0, 0, 255),  # Grey outline
           width = 0.75
         )
       )
     )
   })
   
  } else {
   atributo_name<-sub("_[^_]*$", "", variable)
   
   uniqueValueInfos <- lapply(seq_along(categories), function(i) {
     list(
       value = categories[i],
       label = categories[i],
       symbol = list(
         type = "esriSFS",
         style = "esriSFSSolid",
         color = colors[[i]],
         outline = list(
           type = "esriSLS",
           style = "esriSLSSolid",
           color = c(0, 0, 0, 255),  # Black outline
           width = 0
         )
       )
     )
   })
   
   }

drawing_Info<- list(
  renderer = list(
    type = "uniqueValue",
    field1 = atributo_name,  # The field name to symbolize by
    uniqueValueInfos = uniqueValueInfos
  )
)

return(drawing_Info)
}

######## Definición de un web map solo con el mapa base##########

# Define the web map content
web_map_content_default <- list(
  operationalLayers = list(),
  baseMap = list(
    baseMapLayers = list(
      list(
        id = "World_Hillshade_3805",
        opacity = 1,
        title = "Sombreado mundial",
        url = "https://services.arcgisonline.com/arcgis/rest/services/Elevation/World_Hillshade/MapServer",
        visibility = TRUE,
        layerType = "ArcGISTiledMapServiceLayer"
      ),
      list(
        id = "VectorTile_2333",
        opacity = 1,
        title = "Mapa topográfico mundial",
        visibility = TRUE,
        layerType = "VectorTileLayer",
        styleUrl = "https://cdn.arcgis.com/sharing/rest/content/items/0d27022487ad4e09ae8f274e8d502776/resources/styles/root.json"
      )
    ),
    title = "Topográfico"
  ),
  authoringApp = "ArcGISMapViewer",
  authoringAppVersion = "2024.1",
  initialState = list(
    viewpoint = list(
      targetGeometry = list(
        spatialReference = list(
          latestWkid = 3857,
          wkid = 102100
        ),
        xmin = -8460884,
        ymin = -5640210,
        xmax = -7421565,
        ymax = -2010045
      )
    )
  ),
  spatialReference = list(
    latestWkid = 3857,
    wkid = 102100
  ),
  timeZone = "system",
  version = "2.30"
)

####################################################
pop_up_info_default<-function(variable){
  if(grepl("ice", variable)){
    atributo_name<-paste0(variable,"_cat")
  } else {
    atributo_name<-sub("_[^_]*$", "", variable)}
  
    popupInfo = list(
      popupElements = list(
        list(type = "fields"),
        list(
          type = "attachments",
          displayType = "auto"
        )
      ),
      showAttachments = TRUE,
      fieldInfos = list(
        list(
          fieldName = "FID",
          isEditable = FALSE,
          label = "FID",
          visible = FALSE
        ),
        list(
          fieldName = "Altura",
          format = list(
            digitSeparator = TRUE,
            places = 0
          ),
          isEditable = TRUE,
          label = "Altura",
          visible = TRUE
        ),
        list(
          fieldName = variable,
          format = list(
            digitSeparator = TRUE,
            places = 2
          ),
          isEditable = TRUE,
          label = variable,
          visible = TRUE
        ),
        list(
          fieldName = paste0(variable,"_m"),
          format = list(
            digitSeparator = TRUE,
            places = 2
          ),
          isEditable = TRUE,
          label = paste0(variable,"_m"),
          visible = TRUE
        ),
        list(
          fieldName = paste0(variable,"_cat"),
          format = list(
            digitSeparator = TRUE,
            places = 2
          ),
          isEditable = TRUE,
          label = paste0(variable,"_cat"),
          visible = TRUE
        ),
        list(
          fieldName = "Cuenca",
          format = list(
            digitSeparator = TRUE,
            places = 2
          ),
          isEditable = TRUE,
          label = "Cuenca",
          visible = TRUE
        ),
        list(
          fieldName = "Fecha",
          format = list(
            digitSeparator = TRUE,
            places = 2
          ),
          isEditable = TRUE,
          label = "Fecha",
          visible = TRUE
        ),
        list(
          fieldName = "NOM_COM",
          isEditable = TRUE,
          label = "NOM_COM",
          visible = TRUE
        ),
        list(
          fieldName = "NOM_CUEN",
          isEditable = TRUE,
          label = "NOM_CUEN",
          visible = TRUE
        ),
        list(
          fieldName = "Region",
          isEditable = TRUE,
          label = "Region",
          visible = TRUE
        ),
        list(
          fieldName = "Comuna",
          isEditable = TRUE,
          label = "Comuna",
          visible = TRUE
        ),
        list(
          fieldName = "Provincia",
          isEditable = TRUE,
          label = "Provincia",
          visible = TRUE
        ),
        list(
          fieldName = "NOM_PROV",
          isEditable = TRUE,
          label = "NOM_PROV",
          visible = TRUE
        ),
        list(
          fieldName = "NOM_REG",
          isEditable = TRUE,
          label = "NOM_REG",
          visible = TRUE
        ),
        list(
          fieldName = "NOM_SUBC",
          isEditable = TRUE,
          label = "NOM_SUBC",
          visible = TRUE
        ),
        list(
          fieldName = "Nombre",
          isEditable = TRUE,
          label = "Nombre",
          visible = TRUE
        ),
        list(
          fieldName = "Year",
          format = list(
            digitSeparator = TRUE,
            places = 0
          ),
          isEditable = TRUE,
          label = "Year",
          visible = TRUE
        )
      ),
      title = atributo_name
  )
  

return(popupInfo)
}