#Esta funcion saca los días feriados y no feriados, primer parametro, los días del csv, el segundo, el volumen de tráfico, el tercero la hora, y el ultimo, confirmar. este parametro es un true o false
Holidays <- function(HD, Tr, H, Conf,ID){
  NoneDay <- HD == "None"
  holidays <- !NoneDay
  if(Conf){
    id <- which(holidays)
    indices_feriadas <- which(as.Date(H) %in% as.Date(H[id]))
    if(ID)
      return (Tr[indices_feriadas])
    else  
      return (indices_feriadas)
  }
  else{
    id <- which(holidays)
    ALL <- 1:length(HD)
    indices_feriadas <- which(as.Date(H) %in% as.Date(H[id]))
     nonHoliday <- ALL[!ALL %in% indices_feriadas]
    if(ID)
      return (Tr[nonHoliday])
    else
      return (nonHoliday)
  }
}


#La segunda condición es con por lo menos cuatro rangos de temperatura
#Hay unos datos que no considere porque su temperatura es igual a 0, entonces, esos datos se deben de analizar de otra forma
#para los datos, son 10 "nan" de temperatura, 11899 al 11902 no se registro temperatura y 11947 al 11952 la caracteristica de esos días es que son claros en cuanto a nubes y ocurren en la madrugada del 2 de febrero del 2014 a las 3 de la mañana hasta las 8 de la mañana(los datos 11947 al 11952) y para los otros datos ddel 31 de enero del 2014  a las 3 de la mañana hasta las 6 de la mañana

Temperatura<-function(temp,Tr,rango){
  vtemp<-temp>0
  Maximo<-max(temp[vtemp])
  Minimo<-min(temp[vtemp])
  Rangos<-(Maximo-Minimo)/4
  if(rango==1)
    return (Tr[which( (temp>=Minimo & temp<=Minimo+Rangos))])
  if(rango==2)
    return (Tr[which( (temp>Minimo+Rangos & temp<=Minimo+2*Rangos))])
  if(rango==3)
    return (Tr[which((temp>Minimo+2*Rangos & temp<=Minimo+3*Rangos))])
  if(rango==4)
    return (Tr[which((temp>Minimo+3*Rangos & temp<=Minimo+4*Rangos))])
}
#
#Por lo menos tres rangos de mm de lluvia 
#
#

LLuvia <- function(MM,Tr,rango){
  vMM<-MM!=max(MM)
  Maximo<-max(MM[vMM])
  Minimo<-min(MM[vMM])
  Rangos<-(Maximo-Minimo)/6
  if(rango==1) #No llueve
    return (Tr[which(MM==Minimo)])
  if(rango==2)# Lluvia baja
    return (Tr[which(MM<=2&MM>Minimo)])
  if(rango==3)#lluvia media de intensidad
    return (Tr[which(MM<=15 &MM>2)])
  if(rango==4)#lluvia fuerte
    return (Tr[which(MM>15& MM<=60)])
  if(rango==5)#lluvia extremadamente fuerte
    return (Tr[which(MM>60)])
}

#
#
#
#Por los menos tres rangos de mm de nieve
Nieve <- function(MM,Tr,rango){
  Maximo<-max(MM)
  Minimo<-min(MM)
  Rangos<-(Maximo-Minimo)/3
  if(rango==1) #No nieva
    return (Tr[which(MM==Minimo)])
  if(rango==2) #Nieve baja
    return (Tr[which(MM>Minimo & MM<=Minimo+Rangos)])
  if(rango==3)#Nieve alta
    return (Tr[which(MM>Minimo+Rangos & MM<=Minimo+2*Rangos)])
  if(rango==4)
    return (Tr[which(MM>Minimo+2*Rangos & MM<=Minimo+3*Rangos)])
}
##
#
#
#Por los menos cinco rangos de porcentje de capas de nubes
Nubes <- function(MM,Tr,rango){
  Maximo<-max(MM)
  Minimo<-min(MM)
  Rangos<-(Maximo-Minimo)/5
  if(rango==1) #No nubes
    return (Tr[which(MM>=Minimo & MM<=Minimo+Rangos)])
  if(rango==2)# Nubes baja
    return (Tr[which(MM>Minimo+Rangos&MM<=Minimo+2*Rangos)])
  if(rango==3)#Nubes media de intensidad
    return (Tr[which(MM>Minimo+2*Rangos &MM<=Minimo+3*Rangos)])
  if(rango==4)#Nubes fuerte
    return (Tr[which(MM>Minimo+3*Rangos& MM<=Minimo+4*Rangos)])
  if(rango==5)#Nubes extremadamente fuerte
    return (Tr[which(MM>Minimo+4*Rangos)])

}

#Variables primarias
  #Temporal: Horas del día del 0 al 24

Hora<-function(Horas,hora,Tr){
 return(Tr[which(Horas==hora)])
}

   #Día de la semana domingo a lunes
 #Domingo en es '0'

diasSemana<-function(Day,dia,Tr,id){
  if(id==TRUE)
    return (Tr[which(Day==dia)])
  else  
    return (which(Day==dia))
}
#Días hábiles y no hábiles, los habiles, son de lunes a viernes, sabados y domingos y dias festivos    
WorkDay <- function(i, Tr,Condi,id) {
  day <- which(Week_day == i)
  indices_feriadas <- Holidays(Holiday, traffic, FormDate, TRUE, FALSE)
  # Excluir días no hábiles y feriados
  Week_dayCor <- (i >= 1 & i <= 6) &!(day %in% indices_feriadas)
  Week_dayCorNH <- (i >= 1 & i <= 6) &(day %in% indices_feriadas)
if(Condi){
  if(id)
    return (Tr[day[Week_dayCor]])
  else
    return(day[Week_dayCor])
    # Retornar tráfico para días hábiles (lunes a viernes y feriados) 
}else{
  if(id){
    if(i>0 & i<6 )
      return (Tr[day[Week_dayCorNH]])
    else
      return (Tr[day[!Week_dayCor]])
    }
  else{
    if(i>0 &i <6)
      return (day[Week_dayCorNH])
    else
      return (day[!Week_dayCor])
    }
  }
}
#Meses del año enero a diciembre a la culumna 
YearMonth <- function(Years, Year, Months, Month, Tr) {
  return(Tr[which((Years == Year) & (Months == Month))])
}    
#Años 2012 al 2018
Year <- function(Years, year, Tr,Confi) {
  if(Confi)
    return(Tr[which(Years == year)])
  else
    return(which(Years == year))
}
#Horas habiles
#Horas del día para días hábiles y no hábiles 0 a 24 de lunes a viernes(habiles). De 0 a 24 sabados y domingos(no habiles)
HourAvailable <- function(day, hours, is_workday, Tr) {
 if(is_workday&(day!=0)){
   #dias habiles
   Valores<-WorkDay(day,Tr,is_workday,FALSE)
   all<-which(FormDate$hour==hours)
   return (Tr[intersect(Valores,all)])
 }else{
   #no disponibles
  Valores<-WorkDay(day,Tr,is_workday,FALSE)
  all<-which(FormDate$hour==hours)
  return(Tr[intersect(Valores,all)])
 }
}
#Horas del días para meses del año 0 a 24 de enero a diciem  
YearMonth <- function(Years, Year, Months, Month, Horas, horas, Tr) {
  return(Tr[which((Years == Year) & (Months == Month) & (Horas == horas))])
}       
calc_sesgo <- function(datos) {
  n <- length(datos)
  media <- mean(datos)
  desviacion <- sqrt(sum((datos - media)^2) / n)
  sesgo <- sum((datos - media)^3) / (n * desviacion^3)
  return(sesgo)
}
calc_kurtosis <- function(datos) {
  n <- length(datos)
  media <- mean(datos)
  desviacion <- sqrt(sum((datos - media)^2) / n)
  kurtosis <- sum((datos - media)^4) / (n * desviacion^4) - 3
  return(kurtosis)
}
#######Pregunta a
PercentilesYear<-function(i,Fpermin,Fpermax,Npermin,Npermax){
  indices_feriadas <- Holidays(Holiday, traffic, FormDate, TRUE, FALSE)
  #Constante
  indices_noferiados<-Holidays(Holiday, traffic, FormDate, FALSE, FALSE)
  #######
  yearsFlux<-Year(year_tf,i, traffic,FALSE)
  ##const
  Pers_feriadas<-traffic[intersect(yearsFlux,indices_feriadas)]
  ####const

  Pers_noferiadas<-traffic[intersect(yearsFlux,indices_noferiados)]
  ####const
  plot(Pers_feriadas,1:length(Pers_feriadas), main = paste("Gráfico de Dispersión de los días feriados del",i), xlab = "Cantidad de datos", ylab = "Eje Personas",pch = 16, col = "blue")
   plot(Pers_noferiadas,1:length(Pers_noferiadas), main = paste("Gráfico de Dispersión de los días no feriados del",i), xlab = "Cantidad de datos", ylab = "Eje Personas",pch = 16, col = "darkblue")
  percentilFer<-quantile(Pers_feriadas,probs=seq(0.01,0.99,by=0.01),na.rm=TRUE)
  ###const
  percentilnoFer<-quantile(Pers_noferiadas,probs=seq(0.01,0.99,by=0.01),na.rm=TRUE)##
  #######
  Datospersfer<-Pers_feriadas[which(Pers_feriadas>=percentilFer[Fpermin]&Pers_feriadas<=percentilFer[Fpermax])]
  Datospersnofer<-Pers_noferiadas[which(Pers_noferiadas>=percentilnoFer[Npermin]&Pers_noferiadas<=percentilnoFer[Npermax])]
  PromedioAnualFest<-mean(Datospersfer)
  PromedioAnualNorm<-mean(Datospersnofer)
  medianafest<-median(Datospersfer)
  medianaNofest<-median(Datospersnofer)
  cat("Flujo medio de personas en dias festivos:",PromedioAnualFest,",",medianafest,"\n", "Flujo medio de personas en dias normales:",PromedioAnualNorm,",",medianaNofest,"\n","Diferencia anual",(medianafest-PromedioAnualFest),"diferernciaNormal",(medianaNofest-PromedioAnualNorm),"\n")
plot(Datospersfer,1:length(Datospersfer), main = paste("Gráfico de Dispersión de los días feriados del",i), xlab = "Cantidad de datos", ylab = "Eje Personas",pch = 16, col = "blue")
abline(v = medianafest, col = "red", lty = 2)
plot(Datospersnofer,1:length(Datospersnofer), main = paste("Gráfico de Dispersión de los días no feriados del",i), xlab = "Cantidad de datos", ylab = "Eje Personas",pch = 16, col = "darkblue")
abline(v = medianaNofest, col = "red", lty = 2)
}
Fluxmedium<-function(i,condi){
  if(i==0){
    indices_feriadas <- Holidays(Holiday, traffic, FormDate, TRUE, FALSE)
    indices_noferiados<-Holidays(Holiday, traffic, FormDate, FALSE, FALSE)
    GenteFestiva<-sum(Holidays(Holiday,traffic,FormDate,TRUE,TRUE))
    GenteDiaNrom<-sum(Holidays(Holiday,traffic,FormDate,FALSE,TRUE))
    DiasFestiv<-length(unique(as.Date(FormDate[indices_feriadas])))
    DiastotalNom <-length(unique(as.Date(FormDate[indices_feriadas])))
    PromedFest<-GenteFestiva/DiasFestiv
    PromedDiaN<-GenteDiaNrom/(DiastotalNom)
    
    PromedioGen<-(GenteFestiva+GenteDiaNrom)/(DiasFestiv+DiastotalNom)
    medianaGen<-median(c(Holidays(Holiday,traffic,FormDate,FALSE,TRUE),Holidays(Holiday,traffic,FormDate,TRUE,TRUE)))
    medianaNorm<-median(Holidays(Holiday,traffic,FormDate,FALSE,TRUE))
    medianaFestiv<-median(Holidays(Holiday,traffic,FormDate,TRUE,TRUE))
    cat("Flujo medio de personas en dias festivos:",PromedFest,"\n", "Flujo medio de personas en dias normales:",PromedDiaN,"\n","Promedio general",PromedioGen,"\n","Mediana general",medianaGen,"\n","Mediana festiva",medianaFestiv,"\n", "Mediana normal",medianaNorm,"\n")
    if(condi)
      return(PromedFest)
    else
      return(PromedDiaN)
  }
  else{
    indices_feriadas <- Holidays(Holiday, traffic, FormDate, TRUE, FALSE)
    indices_noferiados<-Holidays(Holiday, traffic, FormDate, FALSE, FALSE)
    yearsFlux<-Year(year_tf,i, traffic,FALSE)
    Pers_feriadas<-traffic[intersect(yearsFlux,indices_feriadas)]
    Pers_noferiadas<-traffic[intersect(yearsFlux,indices_noferiados)]
    percentilFer<-quantile(Pers_feriadas,probs=seq(0.01,0.99,by=0.01),na.rm=TRUE)
    percentilnoFer<-quantile(Pers_noferiadas,probs=seq(0.01,0.99,by=0.01),na.rm=TRUE)
Datospersfer<-Pers_feriadas[which(Pers_feriadas>=percentilFer[10]&Pers_feriadas<=percentilFer[90])]
    Datospersnofer<-Pers_noferiadas[which(Pers_noferiadas>=percentilnoFer[5]&Pers_noferiadas<=percentilnoFer[90])]
    PromedioAnualFest<-mean(Datospersfer)
    PromedioAnualNorm<-mean(Datospersnofer)
    medianafest<-median(Datospersfer)
    medianaNofest<-median(Datospersnofer)
   # cat("Flujo medio de personas en dias festivos:",PromedioAnualFest,",",medianafest,"\n", "Flujo medio de personas en dias normales:",PromedioAnualNorm,",",medianaNofest,"\n")
    if(condi)
      return(PromedioAnualFest)
    else
      return(PromedioAnualNorm)
  }
}
calculate_mode <- function(x) {
  tbl <- table(x)
  mode_values <- as.numeric(names(tbl[tbl == max(tbl)]))
  return(mode_values)
}
PercentilesTotal<-function(Fpermin,Fpermax,Npermin,Npermax){
  Personas_feriadas <- Holidays(Holiday, traffic, FormDate, TRUE, TRUE)
  #ConstanteT
  Personas_noferiados<-Holidays(Holiday, traffic, FormDate, FALSE, TRUE)
  #######
  ##const
  Indices_noferiados<-1:length(Personas_noferiados)

  Q1 <- quantile(Personas_feriadas, 0.25)
  Q3 <- quantile(Personas_feriadas, 0.75)
  QNo1 <- quantile(Personas_noferiados, 0.25)
  QNo3 <- quantile(Personas_noferiados, 0.75)

  # Definir la proporción a excluir
  prop_a_excluir <- 0.1
  prop_a_excluirNo <- 0.1

  # Calcular los límites inferior y superior
  limite_inferior <- Q1 - prop_a_excluir * (Q3 - Q1)
  limite_superior <- Q3 + prop_a_excluir * (Q3 - Q1)
  limite_inferiorNo <- QNo1 - prop_a_excluirNo * (QNo3 - QNo1)
  limite_superiorNo <- QNo3 + prop_a_excluirNo * (QNo3 - QNo1)

  # Excluir los valores fuera de los límites
  datos_recortados <- Personas_feriadas[Personas_feriadas >= limite_inferior & Personas_feriadas <= limite_superior]
  datos_recortadosNo <- Personas_noferiados[Personas_noferiados >= limite_inferiorNo & Personas_noferiados <= limite_superiorNo]

skew_value <- calc_sesgo(datos_recortados)    
  kurtosis_value <- calc_kurtosis(datos_recortados)   
skew_valueNo <- calc_sesgo(datos_recortadosNo)
kurtosis_valueNo <- calc_kurtosis(datos_recortadosNo)

hist(datos_recortados, main = "Histograma de los días de feriados", xlab = "Valores", col = "lightblue", border = "black")
hist(datos_recortadosNo, main = "Histograma de los días no feriados", xlab = "Valores", col = "lightblue", border = "black")
  # Calcular la media de los datos recortados
  media_intercuartilica_feriados <- mean(datos_recortados)
  media_intercuartilica_feriadosNo <- mean(datos_recortadosNo)
  cat("Este es la media truncada de los dias feriados",media_intercuartilica_feriados,"\n","Esta es la media intercuartilica de los días no feriados",media_intercuartilica_feriadosNo,"\n","Dias feriados","Sesgo",skew_value,"\n","Kurtosis",kurtosis_value,"\n","Dias no feriados","Sesgo",skew_valueNo,"\n","Kurtosis",kurtosis_valueNo,"\n")
}