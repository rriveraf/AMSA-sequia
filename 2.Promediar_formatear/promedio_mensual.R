
library(tidyverse)
#Funcion para calcular el promedio de una base de datos y devolverla como matriz
#Entrada: Year, Month, Day, Codigo_nacional, variable_promediar. Todas numericas
promedio_mensual<-function(base_datos, filtro=0.3){

variable <- names(base_datos)[5]  # Obtener el nombre de la variable a promediar
names(base_datos)[5] <- "nombre_variable_temporal" # cambiamos por nombre temporal conocido

#1) Calcular el promedio 

base_datos <- base_datos %>% 
  mutate_at(vars(-Codigo_nacional), as.numeric) %>% #Por seguridad
  filter(!is.na(Month) & !is.na(Year)) %>%      #Filtramos fechas vacías
  group_by(Year, Month, Codigo_nacional) %>%    #Agrupamos por año, mes y estacion
  mutate(proportion_na = sum(is.na(nombre_variable_temporal))/n()) %>%  #se ofrece la proporcion de NA por mes
  summarize( nombre_variable_temporal := 
            ifelse(mean(is.na(nombre_variable_temporal)) > filtro, #Promedio mensual, 
            NA, #si proporcion.NA >filtro entonces NA 
            mean(nombre_variable_temporal, na.rm = TRUE))) #de lo contrario, calculamos mean

names(base_datos)[4]<-variable
return (base_datos)
}


##Para Precipitación
acumulado_mensual<-function(base_datos, filtro=0.3){
  
  variable <- names(base_datos)[5]  # Obtener el nombre de la variable a promediar
  names(base_datos)[5] <- "nombre_variable_temporal" # cambiamos por nombre temporal conocido

  #1) Calcular el acumulado
  
  base_datos <- base_datos %>% 
    #mutate_all(as.numeric) %>% #Por seguridad
    filter(!is.na(Month) & !is.na(Year)) %>%      #Filtramos fechas vacías
    group_by(Year, Month, Codigo_nacional) %>%    #Agrupamos por año, mes y estacion
    mutate(proportion_na = sum(is.na(nombre_variable_temporal))/n()) %>%  #se ofrece la proporcion de NA por mes
    summarize( nombre_variable_temporal := 
                 ifelse(mean(is.na(nombre_variable_temporal)) > filtro, #Promedio mensual, 
                        NA,                                             #si proporcion.NA >filtro entonces NA 
                        sum(nombre_variable_temporal, na.rm = TRUE)))   #de lo contrario, calculamos mean
  
  names(base_datos)[4]<-variable
  return (base_datos)
}


##Para acumular la precipitación anualmente
acumulado_anual<-function(base_datos, filtro=0.3){
  
  #4 columnas: Year, Month, Codigo_nacional, variable
  variable <- names(base_datos)[4]  # Obtener el nombre de la variable a promediar
  names(base_datos)[4] <- "nombre_variable_temporal" # cambiamos por nombre temporal conocido
  
  #1) Calcular el acumulado
  
  base_datos <- base_datos %>% 
    #mutate_all(as.numeric) %>% #Por seguridad
    filter(!is.na(Month) & !is.na(Year)) %>%      #Filtramos fechas vacías
    group_by(Year, Codigo_nacional) %>%    #Agrupamos por año, mes y estacion
    mutate(proportion_na = sum(is.na(nombre_variable_temporal))/n()) %>%  #se ofrece la proporcion de NA por mes
    summarize( nombre_variable_temporal := 
                 ifelse(mean(is.na(nombre_variable_temporal)) > filtro, #Promedio mensual, 
                        NA,                                             #si proporcion.NA >filtro entonces NA 
                        sum(nombre_variable_temporal, na.rm = TRUE)))   #de lo contrario, calculamos mean
  
  names(base_datos)[3]<-variable
  return (base_datos)
}

#Para formatear la base de datos con pivot_wider

formatear_wider<-function(base_datos){

  # Ojo la base de datos de entrada debe tener 4 columnas: Year, Month, Codigo_nacional, variable
  
  variable <- names(base_datos)[4]  # Obtener el nombre de la variable a promediar
  names(base_datos)[4] <- "nombre_variable_temporal" # cambiamos por nombre temporal conocido
  
  #2) Formateamos con pivot wider para lograr el formato deseado
  base_datos<- base_datos %>% 
    select(Year, Month, Codigo_nacional, nombre_variable_temporal) %>% 
    pivot_wider(names_from = Codigo_nacional, 
                values_from = nombre_variable_temporal, 
                values_fill = NA) 
  return (base_datos)
}

# Para cambiar las fechas al formato correcto para calcular los indicadores
formatear_fecha<-function (base_datos){
  #base_Datos con columnas Year, Month, etc...
  base_datos <- base_datos %>% 
  mutate(
    Mes = case_when(
      Month == 1 ~ "01",
      Month == 2 ~ "02",
      Month == 3 ~ "03",
      Month == 4 ~ "04",
      Month == 5 ~ "05",
      Month == 6 ~ "06",
      Month == 7 ~ "07",
      Month == 8 ~ "08",
      Month == 9 ~ "09",  # Change "sep" to "sept"
      Month == 10 ~ "10",
      Month == 11 ~ "11",
      Month == 12 ~ "12",
      TRUE ~ NA_character_
    )
  )  %>% 
  mutate(date = paste0(Mes,"/", Year))%>%
  select(date, Year, Mes, Month, everything())
  base_datos <-base_datos [,-c(2,3,4)]
  return (base_datos)
}

#Función para cambiar de nombre estación a codigo nacional
formatear_nombres<-function(base_datos,estaciones){
  #estaciones debe contener $Codigo_nacional y base_datos contiene dos primeras columnas sin nombres
  base_datos<-as.data.frame(base_datos)
  names(base_datos) <- estaciones$Codigo_nacional[match(names(base_datos), estaciones$Nombre.estacion)]
return (base_datos)
}

##Formatear codigo_nacional caso que el código se lea de forma XCODIGO.DIGITO
Corregir_Codigo_nacional<-function(Carac_gamma){
  library(tidyverse)
  Carac_gamma <- Carac_gamma %>%
    rename_with(~ gsub("X", "", .), everything()) %>%   # Remove "X" from all column names
    rename_with(~ gsub("\\.", "-", .), everything())
  return(Carac_gamma)
}

#Clasificador de indicadores
reclass_indicador <- function(df, column_name) {
  library(tidyverse)
  new_column_name <- paste0(column_name, "_cat")
  
  df <- df %>%
    mutate(!!new_column_name := case_when(
      !!sym(column_name) <= -2.05 ~ "Extremadamente seco",
      !!sym(column_name) < -1.64 & !!sym(column_name) >= -2.05 ~ "Severamente seco",
      !!sym(column_name) < -1.28 & !!sym(column_name) >= -1.64 ~ "Muy seco",
      !!sym(column_name) < -1.04 & !!sym(column_name) >= -1.28 ~ "Seco",
      !!sym(column_name) < -0.84 & !!sym(column_name) >= -1.04 ~ "Ligeramente seco",
      !!sym(column_name) < 0.84 & !!sym(column_name) >= -0.84 ~ "Normal",
      !!sym(column_name) < 1.28 & !!sym(column_name) >= 0.84 ~ "Húmedo",
      !!sym(column_name) <  2.05 & !!sym(column_name) >= 1.28 ~ "Muy húmedo",
      !!sym(column_name) >= 2.05 ~ "Extremadamente húmedo",
      TRUE ~ NA_character_ # Handle other cases if necessary
    ))  %>%
    mutate(!!sym(column_name) := round(!!sym(column_name), 2)) # Round the specified column to 2 decimal places
  
  
  return(df)
}

#Clasificador de regiones
reclass_macrozona <- function(df, region_column_name) {
  
  library(dplyr)
  new_column_name <- "Macrozona"
  
  df <- df %>%
    mutate(
      {{new_column_name}} := case_when(
        !!sym(region_column_name) %in% c("Región de Arica y Parinacota", "Región de Antofagasta",
                                         "Región de Atacama", "Región de Tarapacá", "Región de Coquimbo") ~ "Norte",
        !!sym(region_column_name) %in% c("Región de Valparaíso", "Región Metropolitana de Santiago",
                                         "Región del Maule", "Región del Ñuble",
                                         "Región del Libertador Bernardo O'Higgins", "Región del Bío-Bío",
                                         "Región de La Araucanía") ~ "Centro",
        !!sym(region_column_name) %in% c("Región de Los Ríos", "Región de Los Lagos",
                                         "Región de Aysén del Gral.Ibañez del Campo",
                                         "Región de Magallanes y Antártica Chilena", "Zona sin demarcar") ~ "Sur",
        TRUE ~ "Null"
      )
    )
  
  region_index <- which(names(df) == quo_name(enquo(region_column_name)))
  df <- df %>% select(1:region_index, {{new_column_name}}, everything()[(region_index + 1):ncol(df)])
  
  return(df)
}


rename_subcuencas<-function(base_datos){
  base_datos<-base_datos %>% mutate(ID = 
                          case_when(
                            Nombre_subcuenca == "Costeras entre Rios Quilimari y Petorca" ~ "CQP", 
                            Nombre_subcuenca == "Petorca Medio" ~ "RPM",
                            Nombre_subcuenca == "Rio Ligua Alto (Estero Alicahue)" ~ "RLA",
                            Nombre_subcuenca == "Rio Ligua Bajo (Entre Estero Los Angeles y Desembocadura)" ~ "RLB",
                            Nombre_subcuenca == "Rio Ligua Medio (entre Quebrada La Cerrada y Los Angeles)" ~ "RLM",
                            Nombre_subcuenca == "Rio Petorca Alto (hasta despues Junta Rio Sobrante)" ~ "RPA",
                            Nombre_subcuenca == "Rio Petorca Bajo (Entre Las Palmas y Desembocadura)" ~ "RPB",
                            Nombre_subcuenca == "Rio Quilimari Entre Cajon Ingienillo y Desembocadura" ~ "RQD",
                            Nombre_subcuenca == "Rio Quilimari entre muro Embalse Culimo y Bajo Cajon Ingienillo" ~ "RQC",
                            Nombre_subcuenca == "Rio Quilimari hasta muro Embalse Culimo" ~ "RQE"
                          )
  )
  return(base_datos)
}
