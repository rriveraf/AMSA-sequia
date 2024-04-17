# Example data frames (replace these with your actual data frames)

Test_rsquared<-function(TEST_df, VALID_df){
  #Las df se entregan con la primera columna "date" en formato "mm/yyyy".
  #TEST DEBE ESTAR ENTRE 01/1989 y 12/2018.
directorio<-getwd()
source(paste0(directorio, "/2.Promediar_formatear/promedio_mensual.R"))

#TEST_df <- read.csv(paste0(directorio,
#                      "/BBDD/indicadores/IPE/DGA/IPE12/IPE_12_DGA_1991_2023.csv"))

#DF2 ES Dataframe con datos VALIDADOs
#VALID_df <- read.csv(paste0(directorio,
#                       "/BBDD/indicadores/IPE/DGA/IPE12/IPE12_DGA_1989_2018_valid.csv"))

colnames(TEST_df)[1]<-"date"
colnames(VALID_df)[1]<-"date"
TEST_df<-Corregir_Codigo_nacional(TEST_df)
VALID_df<-Corregir_Codigo_nacional(VALID_df)

# Merge data frames based on the Date column
merged_df <- merge(TEST_df, VALID_df, by = "date", suffixes = c("_df1", "_df2"))
#Clean NA columns
merged_df <- merged_df[,colSums(is.na(merged_df))<nrow(merged_df)]

result_df<-data.frame()
#Calcular R2 para todas las columnas
for(i in 2:length(colnames(merged_df))){
  columna<-colnames(merged_df)[i]
  Codigo_nacional<-substr(colnames(merged_df)[i],1,10)
  dataframe_id<-substr(colnames(merged_df)[i],12,15)
  if(dataframe_id=="df1"){ 
      columna_pareja<-paste0(Codigo_nacional, "_df2")
  } else{
    columna_pareja<-paste0(Codigo_nacional, "_df1")
  }
  
  if (columna_pareja %in% colnames(merged_df)) {
    dummy_df<-data.frame(merged_df[,1], merged_df[[columna]], merged_df[[columna_pareja]])
    names(dummy_df)<-c("date","columna","columna_pareja")
    # Fit the linear regression model using the constructed formula and merged_df
    model <- lm(columna ~ columna_pareja, data = dummy_df)
    rsquared <- summary(model)$r.squared
    result_df<-rbind(result_df, c(Codigo_nacional, rsquared))
    
  } else {
    print(paste0("Column ",columna_pareja," is not present in the DataFrame."))
  }
}
names(result_df)<-c("Codigo_nacional", "R2")

return(result_df)
}
directorio<-getwd()
TEST_df<-read.csv(paste0(directorio,
          "/BBDD/indicadores/IPEE/DGA/IPEE12/IPEE12_DGA_1990_2019_TEST.csv"))
VALID_df <- read.csv(paste0(directorio,
          "/BBDD/indicadores/IPEE/DGA/IPEE12/IPEE12_DGA_1989_2019_VALID.csv"))
#Misma temporalidad
VALID_df<-VALID_df[-c(1:12),]
TEST_df<-TEST_df[-c(349:360),]

result<-Test_rsquared(TEST_df , VALID_df)