#Podemos empezar analizando cada condición
#La primera condición es Apor medio de los días festivos y nos festivos
library(graphics)
source("funciones.r")
data<-read.csv("Metro_Interstate_Traffic_Volume (1).csv")
Holiday<-data$holiday
traffic<-data$traffic_volume
HourDate<-data$date_time
FormDate<-strptime(HourDate,"%Y-%m-%d %H:%M:%S")
Hour<-FormDate$hour
Temp<-data$temp
datos_por_dia <- table(as.Date(FormDate))
idDias<-1:length(datos_por_dia)
MMlluvia<-data$rain_1h
MMnieve<-data$snow_1h
MMnubes<-data$clouds_all
Hour_of_day<-FormDate$hour
Week_day<-FormDate$wday
year_tf <- FormDate$year+1900
month_tf <- as.integer(format(FormDate, "%m"))
Tempe_traffic <- Temperatura(Temp,traffic,4)# Imprimir la tabla

#####    Medidas de tendencia central    #####
###    Temperatura    ###
Tempe_traffic <- Temperatura(Temp,traffic,4)
Tvarianza <- var(Tempe_traffic, na.rm = TRUE)
cat("varianza Temperatura: ", Tvarianza, "\n")
Tmedia_aritmetica <- mean(Tempe_traffic, na.rm = TRUE)
cat("Media Aritmetica Temperatura:", Tmedia_aritmetica, "\n")
Tmedia_geometrica <- exp(mean(log(Tempe_traffic), na.rm = TRUE))
cat("Media Geometrica Temperatura:", Tmedia_geometrica, "\n")
Tmediana <- median(Tempe_traffic, na.rm = TRUE)
cat("Mediana Temperatura:", Tmediana, "\n")
Tmoda <- table(Tempe_traffic)
Tmoda <- as.numeric(names(Tmoda[Tmoda == max(Tmoda)]))
cat("Moda Temperatura:", Tmoda, "\n")
cuartiles <- quantile(Tempe_traffic, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
cat("Q1 Temperatura:", cuartiles[1], "\n")
cat("Q2(Mediana) Temperatura:", cuartiles[2], "\n")
cat("Q3 lluvia:", cuartiles[3], "\n")
deciles <- quantile(Tempe_traffic, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
for (i in seq_along(deciles)) {
  cat("Decil Temperatura", i, ":", deciles[i], "\n")
}
percentiles <- quantile(Tempe_traffic, probs = seq(0.01, 0.99, by = 0.01), na.rm = TRUE)
print(paste("Percentil 1 Temperatura :", percentiles[1]))
cat("Percentil 5 Temperatura :", percentiles[5], "\n")
cat("Percentil 50(mediana) Temperatura:", percentiles[50], "\n")
cat("Percentil 95 Temperatura:", percentiles[95], "\n")
cat("Percentil 99 Temperatura:", percentiles[99], "\n")

####      Lluvia   ####
Lluvia_traffic <- LLuvia(MMlluvia,traffic,4)
varianza <- var(Lluvia_traffic, na.rm = TRUE)
cat("varianza lluvia: ", varianza, "\n")
media_aritmetica <- mean(Lluvia_traffic, na.rm = TRUE)
cat("Media Aritmetica lluvia:", media_aritmetica, "\n")
media_geometrica <- exp(mean(log(Lluvia_traffic), na.rm = TRUE))
cat("Media Geometrica lluvia:", media_geometrica, "\n")
mediana <- median(Lluvia_traffic, na.rm = TRUE)
cat("Mediana lluvia:", mediana, "\n")
moda <- table(Lluvia_traffic)
moda <- as.numeric(names(moda[moda == max(moda)]))
cat("Moda lluvia:", moda, "\n")
cuartiles <- quantile(Lluvia_traffic, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
cat("Q1 lluvia:", cuartiles[1], "\n")
cat("Q2(Mediana) lluvia:", cuartiles[2], "\n")
cat("Q3 lluvia:", cuartiles[3], "\n")
deciles <- quantile(Lluvia_traffic, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
for (i in seq_along(deciles)) {
  cat("Decil lluvia", i, ":", deciles[i], "\n")
}
percentiles <- quantile(Lluvia_traffic, probs = seq(0.01, 0.99, by = 0.01), na.rm = TRUE)
cat("Percentil 1 lluvia :", percentiles[1], "\n")
cat("Percentil 50(mediana) lluvia:", percentiles[50], "\n")
cat("Percentil 95 lluvia:", percentiles[95], "\n")
cat("Percentil 99 lluvia:", percentiles[99], "\n")

####      Nieve    ####
Nieve_traffic <- Nieve(MMnieve,traffic,4)
Nvarianza <- var(Nieve_traffic, na.rm = TRUE)
cat("Nubes varianza: " , Nvarianza,"\n")
Nmedia_aritmetica <- mean(Nieve_traffic, na.rm = TRUE)
cat("Media Aritmetica:", Nmedia_aritmetica, "\n")
Nmedia_geometrica <- exp(mean(log(Nieve_traffic), na.rm = TRUE))
cat("Media Geometrica Nieve:", Nmedia_geometrica, "\n")
Nmediana <- median(Nieve_traffic, na.rm = TRUE)
cat("Mediana Nieve:", mediana, "\n")
Nmoda <- table(Nieve_traffic)
Nmoda <- as.numeric(names(Nmoda[Nmoda == max(Nmoda)]))
cat("Moda Nieve:", Nmoda, "\n")
Ncuartiles <- quantile(Nieve_traffic, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
cat("Q1 Nieve:", Ncuartiles[1], "\n")
cat("Q2(Mediana)Nieve:", Ncuartiles[2], "\n")
cat("Q3 Nieve:", Ncuartiles[3], "\n")
Ndeciles <- quantile(Nieve_traffic, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
  for (i in seq_along(Ndeciles)) {
    cat("Decil Nieve ", i, ":", Ndeciles[i], "\n")
  }
Npercentiles <- quantile(Nieve_traffic, probs = seq(0.01, 0.99, by = 0.01), na.rm = TRUE)
cat("Percentil 1 Nieve:", Npercentiles[1], "\n")
cat("Percentil 50(mediana) Nieve:", Npercentiles[50], "\n")
cat("Percentil 95 Nieve:", Npercentiles[95], "\n")
cat("Percentil 99 Nieve:", Npercentiles[99], "\n")
hist(Nieve_traffic, main = "Frecuencias de personas respecto a la Nieve", xlab = "Personas", col = "white", border = "black",breaks = 10)#plot

####      Nubes      ####
Nubes_traffic <- Nubes(MMnubes,traffic,4)
Nuvarianza <- var(Nubes_traffic, na.rm = TRUE)
cat("Nubes varianza: " , Nuvarianza,"\n")
Numedia_aritmetica <- mean(Nubes_traffic, na.rm = TRUE)
cat("Media Aritmetica Nubes:", Numedia_aritmetica, "\n")
Numedia_geometrica <- exp(mean(log(Nubes_traffic), na.rm = TRUE))
cat("Media Geometrica Nubes:", Numedia_geometrica, "\n")
Numediana <- median(Nubes_traffic, na.rm = TRUE)
cat("Mediana Nubes:", mediana, "\n")
Numoda <- table(Nubes_traffic)
Numoda <- as.numeric(names(Numoda[Numoda == max(Numoda)]))
cat("Moda Nubes:", Numoda, "\n")
Nucuartiles <- quantile(Nubes_traffic, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
cat("Q1 Nubes:", Nucuartiles[1], "\n")
cat("Q2(Mediana)Nubes:", Nucuartiles[2], "\n")
cat("Q3 Nubes:", Nucuartiles[3], "\n")
Nudeciles <- quantile(Nubes_traffic, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
  for (i in seq_along(Nudeciles)) {
    cat("Decil Nubes ", i, ":", Nudeciles[i], "\n")
  }
Nupercentiles <- quantile(Nubes_traffic, probs = seq(0.01, 0.99, by = 0.01), na.rm = TRUE)
cat("Percentil 1 Nubes:", Nupercentiles[1], "\n")
cat("Percentil 50(mediana) Nubes:", Nupercentiles[50], "\n")
cat("Percentil 99 Nubes:", Nupercentiles[99], "\n")
cat("Percentil 95 Nubes:", Nupercentiles[95], "\n")

####      Pregunta A      ####
Year2012<-Year(year_tf,2012, traffic,TRUE)
Year2013<-Year(year_tf,2013, traffic,TRUE)
Year2014<-Year(year_tf,2014, traffic,TRUE)
Year2015<-Year(year_tf,2015, traffic,TRUE)
Year2016<-Year(year_tf,2016, traffic,TRUE)
Year2017<-Year(year_tf,2017, traffic,TRUE)
Year2018<-Year(year_tf,2018, traffic,TRUE)
percentil_traffic<-quantile(traffic,probs=seq(0.01,0.99,by=0.01),na.rm=TRUE)
(percentil_traffic[1])

PercentilesYear(2012,3,98,3,93)
PercentilesTotal(25,75,25,75)

Datosy<-c(Fluxmedium(2012,FALSE),Fluxmedium(2013,FALSE),Fluxmedium(2014,FALSE),Fluxmedium(2015,FALSE),Fluxmedium(2016,FALSE),Fluxmedium(2017,FALSE),Fluxmedium(2018,FALSE))
DatosY<-c(Fluxmedium(2012,TRUE),Fluxmedium(2013,TRUE),Fluxmedium(2014,TRUE),Fluxmedium(2015,TRUE),Fluxmedium(2016,TRUE),Fluxmedium(2017,TRUE),Fluxmedium(2018,TRUE))
Datosx <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018")
barplot(matrix(c(DatosY,Datosy), ncol = 7, byrow = TRUE), beside = TRUE, col = c("skyblue", "orange"), main = "Cantidad de personas por días feriados y no feriados al año", xlab = "Años", ylab = "Población",names.arg=Datosx)
legend("top",legend = c("Días Feriados","Días No Feriados"), fill = c("skyblue", "orange"),cex=0.8,)

####      Pregunta B      ####
Dias_habiles_personas<-c(sum(WorkDay(1,traffic,TRUE,TRUE)),sum(WorkDay(2,traffic,TRUE,TRUE)),sum(WorkDay(3,traffic,TRUE,TRUE)),sum(WorkDay(4,traffic,TRUE,TRUE)),sum(WorkDay(5,traffic,TRUE,TRUE)),sum(WorkDay(6,traffic,TRUE,TRUE)))
Maximo_dato<-max(Dias_habiles_personas)
Position<-which(Dias_habiles_personas==max(Dias_habiles_personas))
cat("Pregunta b: El Valor máximo de personas que llega  a haber en los días hábiles de lunes a sabado sin días feriados es de",Maximo_dato,"El cual, está en la posicion",Position,"Por lo que indica que es un viernes","\n")
barplot(Dias_habiles_personas, col = "darkgreen", main = "Cantidad de personas por días hábiles", xlab = "Días hábiles", ylab = "Población", names.arg = c("Lunes","Martes","Miercoles","Jueves","Viernes","Sabado"))
legend("topleft",legend = c("Días Hábiles"), fill =  "darkgreen",cex=0.8)

####      Pregunta C: turno matutino dias habiles 8:00 am a las 12:00 pm      ####
Lunes<-c(sum(HourAvailable(1,8,TRUE,traffic)),sum(HourAvailable(1,9,TRUE,traffic)),sum(HourAvailable(1,10,TRUE,traffic)),sum(HourAvailable(1,11,TRUE,traffic)),sum(HourAvailable(1,12,TRUE,traffic)))
Martes<-c(sum(HourAvailable(2,8,TRUE,traffic)),sum(HourAvailable(2,9,TRUE,traffic)),sum(HourAvailable(2,10,TRUE,traffic)),sum(HourAvailable(2,11,TRUE,traffic)),sum(HourAvailable(2,12,TRUE,traffic)))
Miercoles<-c(sum(HourAvailable(3,8,TRUE,traffic)),sum(HourAvailable(3,9,TRUE,traffic)),sum(HourAvailable(3,10,TRUE,traffic)),sum(HourAvailable(3,11,TRUE,traffic)),sum(HourAvailable(3,12,TRUE,traffic)))
Jueves<-c(sum(HourAvailable(4,8,TRUE,traffic)),sum(HourAvailable(4,9,TRUE,traffic)),sum(HourAvailable(4,10,TRUE,traffic)),sum(HourAvailable(4,11,TRUE,traffic)),sum(HourAvailable(4,12,TRUE,traffic)))
Viernes<-c(sum(HourAvailable(5,8,TRUE,traffic)),sum(HourAvailable(5,9,TRUE,traffic)),sum(HourAvailable(5,10,TRUE,traffic)),sum(HourAvailable(5,11,TRUE,traffic)),sum(HourAvailable(5,12,TRUE,traffic)))

Sabado<-c(sum(HourAvailable(6,8,TRUE,traffic)),sum(HourAvailable(6,9,TRUE,traffic)),sum(HourAvailable(6,10,TRUE,traffic)),sum(HourAvailable(6,11,TRUE,traffic)),sum(HourAvailable(6,12,TRUE,traffic)))
Ocho<-sum(Lunes[1],Martes[1],Miercoles[1],Jueves[1],Viernes[1],Sabado[1])
Nueve<-sum(Lunes[2],Martes[2],Miercoles[2],Jueves[2],Viernes[2],Sabado[2])
Diez<-sum(Lunes[3],Martes[3],Miercoles[3],Jueves[3],Viernes[3],Sabado[3])
Once<-sum(Lunes[4],Martes[4],Miercoles[4],Jueves[4],Viernes[4],Sabado[4])
Doce<-sum(Lunes[5],Martes[5],Miercoles[5],Jueves[5],Viernes[5],Sabado[5])
###############################
MaximasHoras<-c(max(Ocho),max(Nueve),max(Diez),max(Once),max(Doce))
which.max(MaximasHoras)
PositionHourMorning<-c(which(Lunes==max(Lunes)),which(Martes==max(Martes)),which(Miercoles==max(Miercoles)),which(Jueves==max(Jueves)),which(Viernes==max(Viernes)),which(Sabado==max(Sabado)))
moda<-calculate_mode(PositionHourMorning)
Positionday<-c(which(PositionHourMorning==moda))
cat("Pregunta c: La hora con mayor afluencia es la posición",moda,"Que es a las 8:00AM,y tienden a ser los días",  Positionday,"Que ES DE Lunes viernes","\nSe verifica que Si se encuentranlos pasajeros totales, denotamos que sigue dando que la hora más común es a las 8")
barplot(matrix(c(Lunes, Martes,Miercoles,Jueves,Viernes,Sabado), ncol = 5, byrow = TRUE), beside = TRUE, col = c("skyblue", "orange","red","yellow","darkgreen","darkred"), main = "Cantidad de personas por días Hábiles y horas matutinas", xlab = "Días", ylab = "Población", names.arg = rep(c("8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM" ), each = 6))
legend("topright",legend = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sabado"), fill =  c("skyblue", "orange","red","yellow","darkgreen","darkred"),cex=0.8)

####      Pregunta D: turno vespertino dias habiles 1:00pm a las 7:00pm      ####
LunesA<-c(sum(HourAvailable(1,13,TRUE,traffic)),sum(HourAvailable(1,14,TRUE,traffic)),sum(HourAvailable(1,15,TRUE,traffic)),sum(HourAvailable(1,16,TRUE,traffic)),sum(HourAvailable(1,17,TRUE,traffic)),sum(HourAvailable(1,18,TRUE,traffic)),sum(HourAvailable(1,19,TRUE,traffic)))
MartesA<-c(sum(HourAvailable(2,13,TRUE,traffic)),sum(HourAvailable(2,14,TRUE,traffic)),sum(HourAvailable(2,15,TRUE,traffic)),sum(HourAvailable(2,16,TRUE,traffic)),sum(HourAvailable(2,17,TRUE,traffic)),sum(HourAvailable(2,18,TRUE,traffic)),sum(HourAvailable(2,19,TRUE,traffic)))
MiercolesA<-c(sum(HourAvailable(3,13,TRUE,traffic)),sum(HourAvailable(3,14,TRUE,traffic)),sum(HourAvailable(3,15,TRUE,traffic)),sum(HourAvailable(3,16,TRUE,traffic)),sum(HourAvailable(3,17,TRUE,traffic)),sum(HourAvailable(3,18,TRUE,traffic)),sum(HourAvailable(3,19,TRUE,traffic)))
JuevesA<-c(sum(HourAvailable(4,13,TRUE,traffic)),sum(HourAvailable(4,14,TRUE,traffic)),sum(HourAvailable(4,15,TRUE,traffic)),sum(HourAvailable(4,16,TRUE,traffic)),sum(HourAvailable(4,17,TRUE,traffic)),sum(HourAvailable(4,18,TRUE,traffic)),sum(HourAvailable(4,19,TRUE,traffic)))
ViernesA<-c(sum(HourAvailable(5,13,TRUE,traffic)),sum(HourAvailable(5,14,TRUE,traffic)),sum(HourAvailable(5,15,TRUE,traffic)),sum(HourAvailable(5,16,TRUE,traffic)),sum(HourAvailable(5,17,TRUE,traffic)),sum(HourAvailable(5,18,TRUE,traffic)),sum(HourAvailable(5,19,TRUE,traffic)))
SabadoA<-c(sum(HourAvailable(6,13,TRUE,traffic)),sum(HourAvailable(6,14,TRUE,traffic)),sum(HourAvailable(6,15,TRUE,traffic)),sum(HourAvailable(6,16,TRUE,traffic)),sum(HourAvailable(6,17,TRUE,traffic)),sum(HourAvailable(6,18,TRUE,traffic)),sum(HourAvailable(6,19,TRUE,traffic)))

Uno<-sum(LunesA[1],MartesA[1],MiercolesA[1],JuevesA[1],ViernesA[1],SabadoA[1])
Dos<-sum(LunesA[2],MartesA[2],MiercolesA[2],JuevesA[2],ViernesA[2],SabadoA[2])
Tres<-sum(LunesA[3],MartesA[3],MiercolesA[3],JuevesA[3],ViernesA[3],SabadoA[3])
Cuatro<-sum(LunesA[4],MartesA[4],MiercolesA[4],JuevesA[4],ViernesA[4],SabadoA[4])
Cinco<-sum(LunesA[5],MartesA[5],MiercolesA[5],JuevesA[5],ViernesA[5],SabadoA[5])
Seis<-sum(LunesA[6],MartesA[6],MiercolesA[6],JuevesA[6],ViernesA[6],SabadoA[6])

Siete<-sum(LunesA[7],MartesA[7],MiercolesA[7],JuevesA[7],ViernesA[7],SabadoA[7])
MaximasHorasTarde<-c(max(Uno),max(Dos),max(Tres),max(Cuatro),max(Cinco),max(Seis),max(Siete))
which.max(MaximasHorasTarde)
PositionHourAfternoon<-c(which(LunesA==max(LunesA)),which(MartesA==max(MartesA)),which(MiercolesA==max(MiercolesA)),which(JuevesA==max(JuevesA)),which(ViernesA==max(ViernesA)),which(SabadoA==max(SabadoA)))
ValorMaximoTarde<-calculate_mode(PositionHourAfternoon)
PositionAfternoon<-c(which(PositionHourAfternoon==ValorMaximoTarde))
cat("Pregunta d: La hora con mayor afluencia es la posición",ValorMaximoTarde,"Que es a las 16:00pm,y tienden a ser los días",  PositionAfternoon,"Que son los días Lunes, Jueves y Viernes","\nSe verifica que Si se encuentranlos pasajeros totales, denotamos que sigue dando que la hora más común es a las 4")
barplot(matrix(c(LunesA, MartesA,MiercolesA,JuevesA,ViernesA,SabadoA), ncol = 7, byrow = TRUE), beside = TRUE, col = c("skyblue", "orange","red","yellow","darkgreen","darkred"), main = "Cantidad de personas por días Hábiles y horas Vespertinas", xlab = "Días", ylab = "Población", names.arg = rep(c("1:00 PM", "2:00 PM", "3:00 PM", "4:00 PM", "5:00 PM","6:00 PM","7:00 PM" ), each = 6))

legend("topright",legend = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sabado"), fill =  c("skyblue", "orange","red","yellow","darkgreen","darkred"),cex=0.8)

####      Pregunta E      ####
hist(Tempe_traffic, main = "Frecuencias de personas respecto a la Temperatura", xlab = "Personas", col = "red", border = "black", breaks = 10)
hist(Lluvia_traffic, main = "Frecuencias de personas respecto a la lluvia", xlab = "Personas", col = "blue", border = "black", breaks = 10)
hist(Nubes_traffic, main = "Frecuencias de personas respecto a las Nubes", xlab = "Personas", col = "skyblue", border = "black", breaks = 10)
moda_rango_1_nubes <- Nubes(MMnubes, traffic, 1)
moda_rango_2_nubes <- Nubes(MMnubes, traffic, 2)
moda_rango_3_nubes <- Nubes(MMnubes, traffic, 3)
moda_rango_4_nubes <- Nubes(MMnubes, traffic, 4)
moda_rango_5_nubes <- Nubes(MMnubes, traffic, 5)

moda_subconjunto <- table(c(moda_rango_1_nubes, moda_rango_2_nubes, moda_rango_3_nubes, moda_rango_4_nubes, moda_rango_5_nubes))
moda_subconjunto <- as.numeric(names(moda_subconjunto[moda_subconjunto == max(moda_subconjunto)]))
barplot(table(c(moda_rango_1_nubes, moda_rango_2_nubes, moda_rango_3_nubes, moda_rango_4_nubes, moda_rango_5_nubes)), 
  main = "Modalidades de Nubes por Rango", xlab = "Modalidad", ylab = "Frecuencia", col = "skyblue")

####      Pregunta F      ####    
percentil_95_temp <- quantile(Tempe_traffic, 0.95, na.rm = TRUE)

traffic_percentil_95_temp <- traffic[Tempe_traffic <= percentil_95_temp]
grupos_temperatura <- cut(Tempe_traffic[Tempe_traffic <= percentil_95_temp], breaks = 4, labels = FALSE)
num_grupos <- length(unique(grupos_temperatura))
cat("Pregunta f: Número de grupos de temperatura en el percentil 95:", num_grupos, "\n")
percentil_95_temp <- quantile(traffic, 0.95, na.rm = TRUE)#en base al flujo de personas 
temperaturas_hasta_percentil_95 <- Temp[Temp <= percentil_95_temp]
histograma <- hist(temperaturas_hasta_percentil_95, main = "Frecuencias de la Temperatura", xlab = "Temperatura Kelvins ", col = "red", border = "black", breaks = 100)
#plot
altura_maxima <- max(histograma$counts)
intervalo_moda <- histograma$breaks[which.max(histograma$counts)]
cat("Moda Temperatura en Kelvins:", intervalo_moda, "\n")
####      Pregunta G        ####
(thirddecil<-quantile(traffic,.3)) #general
Rango1<-quantile(Nubes(MMnubes,traffic,1),.3)
#ThirdRango1<-Rango1[which(Rango1<=thirddecil)]
Rango2<-quantile(Nubes(MMnubes,traffic,2),.3)
#ThirdRango2<-Rango2[which(Rango2<=thirddecil)]
Rango3<-quantile(Nubes(MMnubes,traffic,3),.3)
#ThirdRango3<-Rango3[which(Rango3<=thirddecil)]
Rango4<-quantile(Nubes(MMnubes,traffic,4),.3)
#ThirdRango4<-Rango4[which(Rango4<=thirddecil)]
Rango5<-quantile(Nubes(MMnubes,traffic,5),.3)
Rango1
Rango2
Rango3
Rango4
Rango5
barplot(c(Rango1, Rango2, Rango3, Rango4, Rango5, thirddecil),
  names.arg = c("Rango 1", "Rango 2", "Rango 3", "Rango 4", "Rango 5", "General"),
  col = c("red", "green", "blue", "purple", "orange", "gray"),
  main = "Efecto del Porcentaje de Nubes en el Tercer Decil del Flujo de Usuarios",
  ylab = "Valor del Tercer Decil",
  ylim = c(0, max(thirddecil, Rango1, Rango2, Rango3, Rango4, Rango5)))
cat("Pregunta g: Al comparar los datos,se encuentra que los rangos dos y cuatro son mucho mayores que el general,el quinto rango y tercero están ligeramente cercanos al valor general, pero siguen siendo ligeramente mayores, mientras que el primer rango tiene menor afluencia")
ThirdRango5<-Rango5[which(Rango5<=thirddecil)]

####      Pregunta H      ####
descripcion<-data$weather_description
freq<-table(descripcion)
freqtraffic<-table(traffic)
modaflux<-as.numeric(names(freqtraffic[freqtraffic==max(freqtraffic)]))
modaflux
idmoda<-data[traffic==modaflux,]
descripModa<-idmoda$weather_description
(freqDescripModa<-table(descripModa))
car<-names(freqDescripModa)
par(mar = c(12, 4, 4, 2))
par(mgp = c(10,1,0))
barplot(freqDescripModa,las=2,main="Frecuencia de la Moda",xlab="Descripción",ylab="Frecuencia",col="skyblue",names.arg=car)
moda_descripcion_flujo <- names(freqDescripModa[freqDescripModa == max(freqDescripModa)])
cat("Pregunta h: Encontramos que una de las descripciones con mayor influencia con el flujo de usuarios es:",moda_descripcion_flujo)