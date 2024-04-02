library(readxl)
library(writexl)
library(tidyverse)

#DEfinir variables climatologicas
variables<-c("caudal_mean","temp_max","temp_min","pp_min")

#Escoger la variable a convertir
i<-3

#Definir fecha de inicio y fecha de termino
fecha_fin1<-"_2021_09"
fecha_fin2<-"_2023_06"
  
#Definir nombres de archivos
archivos1<-paste0(variables,fecha_fin1,".xlsx")
archivos2<-paste0(variables,fecha_fin2,".xlsx")

#Definir ruta de ae srchivos
path1<- paste0(getwd(),"/BBDD/", archivos1)
path2<- paste0(getwd(),"/BBDD/", archivos2)

#Se carga la data
data1<- read_excel(path1[i], sheet= gsub("\\.xlsx$", "", archivos1[i]))
data2<- read_excel(path2[i], sheet= gsub("\\.xlsx$", "", archivos2[i]))

#Definir nombres de las primeras columnas
colnames(data1)[c(2,3,4)]<-c("Year", "Month", "Day")

#Cambiar formato de la BBDD descargada
data2 <- pivot_wider(data2, names_from = Codigo_nacional, values_from = variables[i])
data2 <- data2 %>% 
  mutate(Fecha = ymd(paste(Year, Month, Day, sep = "-")))%>%
  select(Fecha, Year, Month, Day, everything())
data1<- data1 %>%
  filter(Fecha < ymd("2020-01-01"))

#Hacemos coincidir el formato de los nombres de columnas
data2 <- data2 %>%
  mutate_at(vars(all_of(names(data2))), as.character)
data1 <- data1 %>%
  mutate_at(vars(all_of(names(data1))), as.character)

#Finalmente unimos
result<-full_join(data1,data2)

#Transformamos todos los datos en numericos (excepto las primeras 4 columnas)
result <- result %>% mutate(across(-c(1:4), as.numeric))

#OUTPUT TO EXCEL
write_xlsx(
  result,
  path = path2[i],
  col_names = TRUE,
  format_headers = TRUE
)





