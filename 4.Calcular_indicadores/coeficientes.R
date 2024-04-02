library(tidyverse)
library(writexl)
lista_completa<-"C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/metadatos/DMC/Catastro.xlsx"
lista_chica<-"C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/metadatos/DMC/Metadatos_DMC.xlsx"

lista_completa<-readxl::read_excel(lista_completa)
lista_chica<-readxl::read_excel(lista_chica)


lista_completa <- lista_completa %>% 
  mutate(Latitud = str_replace(Latitud, "(^.{3})", "\\1.")) %>%
  mutate(Longitud = str_replace(Longitud, "(^.{3})", "\\1."))

names(lista_chica)[c(3,4)]<-c("Latitud","Longitud")
lista_chica<-lista_chica[order(-lista_chica$Latitud), ]

lista_chica$Latitud<-as.numeric(lista_chica$Latitud)
lista_completa$Latitud<-as.numeric(lista_completa$Latitud)
lista_chica$Longitud<-as.numeric(lista_chica$Longitud)
lista_completa$Longitud<-as.numeric(lista_completa$Longitud)

merged<-left_join(lista_chica, lista_completa, 
           by = c("Latitud", "Longitud"))
which(duplicated(merged$Estacion))
fixed<-merged[-c(122,125,170,173,212,213,580,583,616,836,837,133),]

coef<-read.csv("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/coeficientes/COEF_IPE-6_DMC.csv")

Estaciones<-names(coef)

estaciones_dmc<-data.frame(names(coef)[-1])
names(estaciones_dmc)<-"Estacion"

result<-NULL

result <- left_join(estaciones_dmc, fixed, by = "Estacion") 
result<-result[-1172,]
match_result <- identical(result$Estacion, estaciones_dmc$Estacion)
names(coef)[-1]<-result$`Codigo Nacional`
# Check if there is a match in every row
all_match <- all(match_result)

names(coef)[-1]<-estaciones_dmc$Estacion



write_xlsx(
  result,
  path =paste0("C:/Users/56984/Documents/CCGUC/ANID_Sequias/BBDD/metadatos/DMC/estaciones_DMC_fixed.xlsx"),
  
  col_names = TRUE,
  format_headers = TRUE
)