#library(xlsx)
library("readxl")
rm(list=ls())
library(dplyr)
datos<-read.csv2("inf_dist.csv")
notificados<-as.vector(as.data.frame(read_excel("inf_dist_notificados.xls"))[,1])
#seleccionamos los de Lugo y en estado de docmentacion completa
datos$Estado<-as.factor(datos$Estado)
datos<-dplyr::filter(datos,datos$Provincia=="LUGO"&datos$Estado=="DOCUMENTACION COMPLETA")
#Arreglamos los encabezados
colnames(datos)[c(1,2,6,7,8,11,12,13,14,15,16)]<-c("Número_Instrumento","ID_Monte","Informe_Distrito","Superficie_Ordenada","Trámite_Audiencia","Data_Presentación","Data_Paralización","Meses_Paralización","Data_Petición","Data_Devolución","Data_Fin_Plazo")

#################1.- Determinamos los expedientes informados por el distrito################################
datos$Data_Petición<-as.Date(datos$Data_Petición,tryFormats = "%Y-%m-%d")
datos$Data_Devolución<-as.character(datos$Data_Devolución)
datos$Data_Devolución[which(datos$Data_Devolución=="")]<-NA
datos$Data_Devolución<-as.Date(datos$Data_Devolución,tryFormats = "%Y-%m-%d")

dias<-datos$Data_Petición-datos$Data_Devolución
informados<-datos$Número_Instrumento[!is.na(dias)]
no_informados<-datos$Número_Instrumento[is.na(dias)]
#Pendentes de informe de distrito
no_informados<-as.data.frame(no_informados)
colnames(no_informados)<-c("Número_Instrumento")
no_informados_join<-left_join(no_informados,datos,by="Número_Instrumento")
(no_inf<-no_informados_join[!duplicated(no_informados_join$Número_Instrumento),][,c(1,3,4,5,14)])
no_inf[order(no_inf$Distrito),]

#write.csv(no_inf,"pte_doc_compl.csv")
#Mostramos os que xa foron informados pero non notifiquei ao promotor
(inf<-informados)
informados_no_notificados<-droplevels(as.factor(informados[!informados%in%(notificados)]))
#Retiramos los que están siendo tramitados en EIA
EIA<-as.vector(as.data.frame(read_excel("EIA.xls",col_names = F)))[,1]
informados_no_notificados[!informados_no_notificados%in%EIA]

#write.csv(no_inf,file="D:/Master_R/Temas_R/Expedientes_Ordenacion/Estado_tramitacion/doc_comp_no_inf.csv")
#write.csv(inf,file="D:/Master_R/Temas_R/Expedientes_Ordenacion/Estado_tramitacion/doc_comp_inf.csv")


###############2.- COMPROBAMOS LOS INFORMES SECTORIALES#####################################################
library("readxl")

library(dplyr)
data<-read.csv2("inf_sect.csv")
#seleccionamos los de Lugo y en estado de docmentacion completa
data$Estado<-as.factor(data$Estado)
data<-filter(data,data$Provincia=="LUGO"&data$Estado=="DOCUMENTACION COMPLETA")
#Arreglamos los encabezados
colnames(data)[c(1,2,9,10,11,14,15,16)]<-c("Número_Instrumento","ID_Monte","Data_Presentación","Data_Paralización","Meses_Paralización","Data_Petición","Data_Acuse","Data_Devolución")

#Cambiamos los formatos para fechas
data$Data_Petición<-as.Date(data$Data_Petición,tryFormats = "%Y-%m-%d")
#Mostramos los expedientes donde hayan transcurrido los plazos para la emisión de los informes
for(i in unique(data$Número_Instrumento)){
  maximo<-max(abs(data$Data_Petición[data$Número_Instrumento==i]-as.Date(Sys.time(),tryFormats = "%Y-%m-%d")))
  if(maximo>=90)
    cat(i,"\n","Maximo de dias transcurridos=",maximo,"\n")
  else
    next
}
#Mostramos los expedientes que superando los 90 dias no tengan el informe de distrito. para avisarlos 

j=1
superados<-c()
for(i in unique(data$Número_Instrumento)){
  maximo<-max(abs(data$Data_Petición[data$Número_Instrumento==i]-as.Date(Sys.time(),tryFormats = "%Y-%m-%d")))
  if(maximo>=90){
    superados[j]<-i
    j=j+1
  }
  
  else{
    j=j+1 
  }
  
}
#Comprobamos los que superando los 90 dias no estan informados por distrito
no_distrito<-superados[superados%in%no_informados]
no_distrito<-as.data.frame(no_distrito)
colnames(no_distrito)<-"Número_Instrumento"
no_distrito_join<-left_join(no_distrito,data,by="Número_Instrumento")
no_distrito_join[!duplicated(no_distrito_join$Número_Instrumento),][,c(1,4,5,14)]


#Os que se poden pasar a trámite de audiencia que non estan en EIA
superados[superados%in%informados][!(superados[superados%in%informados])%in%EIA]






#############3.- COMPROBAMOS OS INFORMES DE DISTRITO EN FASE DE CONTESTADO TRÁMITE DE AUDIENCIA###################
datos<-read.csv2("Inf_dist.csv")
datos$Estado<-as.factor(datos$Estado)
datos<-filter(datos,datos$Provincia=="LUGO"&datos$Estado=="CONTESTADO TRAMITE DE AUDIENCIA")
#Arreglamos los encabezados
colnames(datos)[c(1,2,6,7,8,11,12,13,14,15,16)]<-c("Número_Instrumento","ID_Monte","Informe_Distrito","Superficie_Ordenada","Trámite_Audiencia","Data_Presentación","Data_Paralización","Meses_Paralización","Data_Petición","Data_Devolución","Data_Fin_Plazo")
#Seleccionamos os rexistros con tramite de audiencia
datosTdA<-datos[datos$Trámite_Audiencia=="S",]
#Comprobamos as datas
datosTdA$Data_Petición<-as.Date(datosTdA$Data_Petición,tryFormats = "%Y-%m-%d")
datosTdA$Data_Devolución<-as.character(datosTdA$Data_Devolución)
datosTdA$Data_Devolución[which(datosTdA$Data_Devolución=="")]<-NA
datosTdA$Data_Devolución<-as.Date(datosTdA$Data_Devolución,tryFormats = "%Y-%m-%d")
dias<-datosTdA$Data_Petición-datosTdA$Data_Devolución
informadosTdA<-datosTdA$Número_Instrumento[!is.na(dias)]
no_informadosTdA<-datosTdA$Número_Instrumento[is.na(dias)]
informadosTdA
no_informadosTdA
#Expedientes que no tienen 2do informe de distrito pero tienen otros informes sectoriales
unique(datos$Número_Instrumento)[!unique((datos$Número_Instrumento))%in%c(informadosTdA,no_informadosTdA)]

#write.csv(informadosTdA,file="D:/Master_R/Temas_R/Expedientes_Ordenacion/Estado_tramitacion/TdA_inf.csv")


library(dplyr)
no_informadosTdA<-as.data.frame(no_informadosTdA)
colnames(no_informadosTdA)<-c("Número_Instrumento")
no_informadosTdA_join<-left_join(no_informadosTdA,datosTdA,by=c("Número_Instrumento"))
(no_inf_CTdA<-no_informadosTdA_join[,c(1,3,4,14)])
#write.csv(no_inf_CTdA,"no_inf_CTdA.csv")
#write.csv(no_inf_CTdA,file="D:/Master_R/Temas_R/Expedientes_Ordenacion/Estado_tramitacion/no_inf_CTdA.csv")
#pas<-paste(no_informadosTdA$Número_Instrumento,"\n","Distrito",no_informadosTdA$Distrito,"\n")
#cat(pas)

#Comprobamos os que se poden pasar a resolución.
contestadosTdA<-read.csv2("contestadoTdA.csv")
  #Concatenamos vectores factoriales. Para ilo hai que transformalos
distrito<-as.factor(c(as.character(no_informadosTdA$Número_Instrumento),as.character(informadosTdA)))
#Os que poderian pasar a resolución agás dependan doutros informes diferentes a distrito:
distrito[(distrito%in%informadosTdA)]




