#Leyendo el archivo csv
datos<-read.csv("basededatos.csv")
#head(datos)
#Fechas
Fechas<-datos$Fecha
Fechas<-strptime(Fechas,"%d/%m/%Y")
ry<-1900
#Precios
AperturaVenta<-datos$SF43784
CierreVenta<-datos$SF43786
#Quitar N/E
Lbl<-"N/E"
idNE1 <- AperturaVenta==Lbl
idNE2 <- CierreVenta==Lbl
AperturaVenta[idNE1] =-1
CierreVenta[idNE2] =-1
#Fechas desde 1900
xy= Fechas$year+ry
#Graficas que muestran el crecimiento dfe paertura y cierre de compra desde 1991 a 2023
#plot(xy,AperturaVenta,main="Grafica de tipo de cambio",xlab="Fecha",ylab="Tipo de cambio")
#plot(xy,CierreVenta,main="Grafica de tipo de cambio",xlab="Fecha",ylab="Tipo de cambio")
#Quitar los -1
idV= CierreVenta!=-1
idV1= AperturaVenta!=-1
#Selecciona un año
s_year= 2001
sy=s_year-ry
idY= (xy-ry)==sy
#Busca ese año en el vector de fechas, también los valores 
xs=Fechas[idY&idV]
xs1=Fechas[idY&idV1]
Ys1=AperturaVenta[idY&idV]
Ys2=CierreVenta[idY&idV1]
#plot(xs$yday,Ys1,main=sprintf("Grafica de tipo de cambio(%d)",s_year),xlab="Dia del año",ylab="Tipo de cambio",col="dark green",pch="$",cex=1)
#plot(xs1$yday,Ys2,main=sprintf("Grafica de tipo de cambio(%d)",s_year),xlab="Dia del año",ylab="Tipo de cambio",col="dark green",pch="$",cex=1)
#Indices para los precios
index1=(1:length(Ys1))
index2=(1:length(Ys2))
#Hace las diferencias
Diferencia=as.double(Ys1[index1])-as.double(Ys2[index2])
#Les quita el signo con una funcion continua
SinSignoDif=(sqrt(Diferencia**2))
#Saca el valor maximo
vmaxDif=max(SinSignoDif)
#Busca el valor de aquel quien fue el maximo
local=(SinSignoDif==vmaxDif)
#Irmpime en consola
Texto1="Precio de Apertura"
Texto2="Precio de Cierre"
FechaDif="Fecha de diferencia"
Texto1
Ys1[local]
Texto2
Ys2[local]
FechaDif
xs[local]
"Valor maximo de diferencia"
vmaxDif
"Fechas posteriores de 2020, no hay valores de cierre"