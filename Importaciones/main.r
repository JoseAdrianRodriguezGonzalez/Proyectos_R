#Analice la base de datos “Mexico.csv” que corresponde a las estadísticas comerciales de México. La base de datos fue extraída del dataset “commodity_trade_statistics_data.csv”. La base de datos contiene 11 columnas que corresponde de la siguiente manera: 
#“”: Índice de posición
#integrantes
#Jose Adrian Rodriguez Gonzalez
#Maria Isabel Rocha vargas
#“country_or_area”: Nombre del país o zona 
#“year”: Año de transacción 
#“comm_code”: Código común de transacción 
#“commodity”: Nombre del producto de la transacción 
#“flow”: Flujo de la transacción 
#“trade_usd”: Precio de la transacción en USD 
#“weight_kg”: Peso de la transacción en kilogramos 
#“quantity_name”: Nombre de la unidad en la transacción 
#“quantity”: Cantidad de la transacción 
#“category”: Categoría de la transacción 
datos <- read.csv("Mexico.csv")
#1. ¿Las exportaciones promedio en los 90s, 00s y 10s, se han incrementado o decrementado? CHECK:D
# Función para calcular el promedio de la cantidad de productos exportados en una década sin considerar valores faltantes
promedio_exportaciones <- function(datos, inicio, fin) {
  datos_decada <- datos[datos$year >= inicio & datos$year <= fin&datos$flow=="Export", ]
  datos_decada_sin_na <- datos_decada[complete.cases(datos_decada$trade_usd), ]
  return(mean(datos_decada_sin_na$trade_usd))
}
# Década de los 90s
promedio_90s <- promedio_exportaciones(datos, 1990, 1999)
# Década de los 00s
promedio_00s <- promedio_exportaciones(datos, 2000, 2009)
# Década de los 10s
promedio_10s <- promedio_exportaciones(datos, 2010, 2019)

# Comparar los promedios de cantidad exportada entre décadas
cat(" \t\t1.-Primera pregunta\n\n")
cat("Promedio de cantidad exportada en los 90s:", promedio_90s, "\n")
cat("Promedio de cantidad exportada en los 00s:", promedio_00s, "\n")
cat("Promedio de cantidad exportada en los 10s:", promedio_10s, "\n")

# Comparar los promedios de exportaciones entre décadas
if (promedio_90s < promedio_00s) {
  cat("Las exportaciones aumentaron de los 90s a los 00s.\n")
} else if (promedio_90s > promedio_00s) {
  cat("Las exportaciones disminuyeron de los 90s a los 00s.\n")
} else {
  cat("Las exportaciones se mantuvieron constantes de los 90s a los 00s.\n")
}

if (promedio_00s < promedio_10s) {
  cat("Las exportaciones aumentaron de los 00s a los 10s.\n")
} else if (promedio_00s > promedio_10s) {
  cat("Las exportaciones disminuyeron de los 00s a los 10s.\n")
} else {
  cat("Las exportaciones se mantuvieron constantes de los 00s a los 10s.\n")
}

######Cual  gener o  mas ingersos
#moda 
#2.¿Cuál es el producto que más fue importado en el 2016, y cuál fue su comportamiento durante las tres décadas anteriores? check:DDD

datos_2016 <- datos[datos$year == 2016&datos$flow=="Import"&datos$commodity!="ALL COMMODITIES", ]
datos_2016_na<-datos_2016[complete.cases(datos_2016$quantity), ]
Maxim2016<-max(datos_2016_na$quantity)
Producto<-datos_2016_na$commodity[which(datos_2016_na$quantity==Maxim2016)]

datos_2010 <- datos[(datos$year >= 2010&datos$year<=2019)&datos$flow=="Import", ]
datos_2010_na<-datos_2010[complete.cases(datos_2010$quantity), ]
datos_2000 <- datos[(datos$year >= 2000&datos$year<=2009)&datos$flow=="Import", ]
datos_2000_na<-datos_2000[complete.cases(datos_2000$quantity), ]
datos_1990 <- datos[(datos$year>=1990&datos$year<=1999)&datos$flow=="Import", ]
datos_1990_na<-datos_1990[complete.cases(datos_1990$quantity), ]
Producto2010<-datos_2010_na$quantity[which(datos_2010_na$commodity==Producto)]
#Producto2010
par(mar = c(6, 6, 6, 5))
barplot(Producto2010, main=paste("Importaciones en el \n2010",Producto), xlab="Importaciones al año", ylab="Cantidad", col="blue",names.arg=c("2016","2015","2014","2013","2012","2011","2010"))
Producto2000<-datos_2000_na$quantity[which(datos_2000_na$commodity==Producto)]
#Producto2000
barplot(Producto2000, main=paste("Importaciones en el 2000\n",Producto), xlab="Importaciones al año", ylab="Cantidad", col="blue",        names.arg =c("2009","2008","2007","2006","2005","2004","2003","2002","2001","2000"))

Producto1990<-datos_1990_na$quantity[which(datos_1990_na$commodity==Producto)]

barplot(Producto1990, main=paste("Importaciones en 1990\n",Producto), xlab="Importaciones al año", ylab="Cantidad", col="blue",names.arg=c("1999","1998","1997","1996","1995","1994","1993","1992","1991","1990"))
#Producto1990
cat("\n\n\t\t2.-Segunda pregunta\n\n","El produco más importado en 2016 es: ",Producto,"En la decada del 2010, tuvo el siguiente comportamiento: ",Producto2010,"En la decada del 2000 tuvo el siguiente comportamiento",Producto2000,"En la decada de 1990 tuvo el siguiente comportamiento",Producto1990,"\n\t")
#frecuencias <- table(datos_2016$commodity)
# Encontrar la moda (valor más frecuente)
#moda <- names(frecuencias)[which.max(frecuencias)]

#cat("La moda de la columna 'commodity' para el año 2016 es:", moda, "\n")

#inciso 3
#a cada columna le aplicamos la funcion 
datos_1990_producto<-datos[datos$year>=1990&datos$year<2000&datos$commodity!="ALL COMMODITIES",]
datos_decada_sin_na2 <- datos_1990_producto[complete.cases(datos_1990_producto$trade_usd), ]

productos_decada_1990<-unique(datos_1990_producto$commodity)
ganancias_por_producto <- numeric(length(productos_decada_1990))
for (i in seq_along(productos_decada_1990)) {
  datos_producto_actual <- datos_1990_producto[datos_1990_producto$commodity == productos_decada_1990[i], ]

  # Separar los datos en exportaciones e importaciones
  exportaciones <- datos_producto_actual[datos_producto_actual$flow == "Export", ]
  importaciones <- datos_producto_actual[datos_producto_actual$flow == "Import", ]

  # Calcular las ganancias netas para el producto actual
  ganancias_netas_producto_actual <- sum(exportaciones$trade_usd) - sum(importaciones$trade_usd)

  # Almacenar el resultado en el vector
  ganancias_por_producto[i] <- ganancias_netas_producto_actual
}
  
resultados <- data.frame(Producto = productos_decada_1990, Ganancias_Netas = ganancias_por_producto)
MejorProducto90<-resultados$Producto[which(resultados$Ganancias_Netas==max(resultados$Ganancias_Netas))]
Ganancias90<-max(resultados$Ganancias_Netas)
datos_2000_producto <- datos[datos$year >= 2000 & datos$year<2010 &datos$commodity == MejorProducto90, ]
datos_2010_producto <- datos[datos$year >= 2010 & datos$year<2020 &datos$commodity == MejorProducto90, ]

# Separar los datos en exportaciones e importaciones
exportaciones <- datos_2000_producto[datos_2000_producto$flow == "Export", ]
importaciones <- datos_2000_producto[datos_2000_producto$flow == "Import", ]

# Calcular las ganancias netas
ganancias_netas_del_2000 <- sum(exportaciones$trade_usd) - sum(importaciones$trade_usd)

# Mostrar el resultado
exportaciones <- datos_2010_producto[datos_2010_producto$flow == "Export", ]
importaciones <- datos_2010_producto[datos_2010_producto$flow == "Import", ]

# Calcular las ganancias netas
ganancias_netas_del_2010 <- sum(exportaciones$trade_usd) - sum(importaciones$trade_usd)


cat("\n\n\t\t3.-Tercera pregunta\n\n","El producto con más ingresos en 1990 fue:",MejorProducto90,"con un comportamiento de ",Ganancias90,"su comportamiento a lo largo de los años fue de ",ganancias_netas_del_2000,"Estas son las ganancias en la decáda del 2010,",ganancias_netas_del_2010,"\n")
barplot(c(Ganancias90,ganancias_netas_del_2000,ganancias_netas_del_2010), main=paste("Ganancias del",MejorProducto90), xlab="Productos", ylab="Ganancias en USD", col="blue",names.arg=c("1990","2000","2010"))

#inciso 4#4. ¿Qué categoría representa el mayor ingreso en el año 2016? 
datos_2016_categoria <- datos[datos$year == 2016&datos$category!="all_commodities",]
####################

productos_categoria_2016<-unique(datos_2016_categoria$category)
ganancias_por_categoria <- numeric(length(productos_categoria_2016))
for (i in seq_along(productos_categoria_2016)) {
  datos_producto_actual1 <- datos_2016_categoria[datos_2016_categoria$category == productos_categoria_2016[i], ]

  # Separar los datos en exportaciones e importaciones
  exportaciones2016 <- datos_producto_actual1[datos_producto_actual1$flow == "Export", ]
  importaciones2016 <- datos_producto_actual1[datos_producto_actual1$flow == "Import", ]

  # Calcular las ganancias netas para el producto actual
  ganancias_netas_producto_actual2016 <- sum(exportaciones2016$trade_usd) - sum(importaciones2016$trade_usd)

  # Almacenar el resultado en el vector
  ganancias_por_categoria[i] <- ganancias_netas_producto_actual2016
}

resultados2016 <- data.frame(Categoria = productos_categoria_2016, Ganancias_Netas = ganancias_por_categoria)
MejorCategoria2016<-resultados2016$Categoria[which(resultados2016$Ganancias_Netas==max(resultados2016$Ganancias_Netas))]
cat("\n\n\t\t4.-Cuarta pregunta\n\n","La categoria que con mayor ingreso en 2016 es ",MejorCategoria2016)
#####################
#5 ¿Qué producto generó el mayor costo de importación en el 2016? 
Maximo_import_2016<-max(datos_2016$trade_usd)
Producto_mas_caro<-datos_2016$commodity[which(datos_2016$trade_usd==Maximo_import_2016)]
cat("\n\n\t\t5.-Quinta pregunta\n","\nEl produco más caro de importar es ",Producto_mas_caro,"\n")
# Encontrar la categoría con el mayor ingreso

#cat("La categoría con el mayor ingreso en el año 2016 es:", categoria_max_ingreso, "\n")

##6. ¿Cuál es el producto que sufrió el mayor incremento en la importación entre el año 1990 y 2016? 
# Filtrar datos para el período 1990-2016
datos_1990_2016 <- datos[datos$year >= 1990 & datos$year <= 2016 & datos$commodity!="ALL COMMODITIES", ]
diferencia_importacion <- tapply(datos_1990_2016$trade_usd, datos_1990_2016$commodity, function(x) tail(x, 1) - x[1])
producto_max_incremento <- names(diferencia_importacion)[which.max(diferencia_importacion)]
cat("\n\n\t\t6.-Sexta pregunta\n\n"," El producto que sufrió el mayor incremento en importación entre 1990 y 2016 es:", producto_max_incremento, "\n")
Producto_grave2010<-datos_2010$trade_usd[which(datos_2010$commodity==producto_max_incremento)]  
Producto_grave2000<-datos_2000$trade_usd[which(datos_2000$commodity==producto_max_incremento)]
Producto_grave1990<-datos_1990$trade_usd[which(datos_1990$commodity==producto_max_incremento)]
#7 Considerando la relación entre los productos importados y exportados en el 1990 y 2016, ¿En qué productos pasamos de ser exportadores a importadores? 

datos_importado_exportado<-datos[datos$year %in% c(1990, 2016) & datos$commodity != "ALL COMMODITIES", ]
datos_importado_exportado_na<-datos_importado_exportado[complete.cases(datos_importado_exportado$quantity), ]
datos_1990_importar <- datos_importado_exportado_na[datos_importado_exportado_na$year == 1990, ]
datos_2016_importar <- datos_importado_exportado_na[datos_importado_exportado_na$year == 2016, ]
datos_comparativos <- merge(datos_1990_importar, datos_2016_importar, by = c("commodity", "flow"), suffixes = c("_1990", "_2016"))
# Identificar productos donde cambiamos de exportador a importador
productos_cambio_export_import <- datos_comparativos[datos_comparativos$quantity_1990 > 0 & datos_comparativos$quantity_1990 < datos_comparativos$quantity_2016 & datos_comparativos$flow == "Import"& datos_comparativos$commodity!="NA", "commodity"]


cat("\n\n\t\t7.-Pregunta siete\n\n","Estos son los productos que cambiaron de exportadores a importadores",productos_cambio_export_import)
#8. ¿Cuál es el rango intercuartílico(cuartiles) por categoría de las importaciones para el año 2015? 
# Filtrar datos para el año 2015
datos_2015 <- datos[datos$year == 2015, ]
iqr_por_categoria <- tapply(datos_2015$quantity, datos_2015$category, function(x) IQR(x, na.rm = TRUE))
cat("\n\n\t\t8.- Octava pregunta\n\n","Rango intercuartílico por categoría de las importaciones para el año 2015:\n")
print(iqr_por_categoria)
###Pregunta 9 ¿En qué año el sesgo es mayor para las exportaciones para la categoría 31? 
calc_sesgo <- function(datos) {
  n <- length(datos)
  media <- mean(datos)
  desviacion <- sqrt(sum((datos - media)^2) / n)
  sesgo <- sum((datos - media)^3) / (n * desviacion^3)
  return(sesgo)
}

sesgo_year<-list()
for(year in unique(datos$year)){
  datos_categoria_31_export <- datos$quantity[datos$category=="31_fertilizers"&datos$flow=="Export"&datos$year==year]
  sesgo_year_<-calc_sesgo(datos_categoria_31_export)
  sesgo_year[[as.character(year)]]<-sesgo_year_
}
year_max_sesgo <- which.max(unlist(sesgo_year))
cat("\n\n\t\t9.-Pregunta Nueve\n\n","Este fue el año que tuvo más sesgo para esta categoría",unique(datos$year)[year_max_sesgo],"Con un sesgo de ",sesgo_year[[year_max_sesgo]])
###Pregunta 10

datos_2000<-datos[datos$year==2000,]
varianza_producto_2000 <- tapply(datos_2000$quantity, datos_2000$commodity, var,na.rm=TRUE)
Producto_maximo_varianza<-names(varianza_producto_2000)[which.max(varianza_producto_2000)]
cat("\n\n\t\t10.- Pregunta Diez\n\n","El producto con la mayor varianza en el año 2000 es",Producto_maximo_varianza,"con un valor de",varianza_producto_2000[which.max(varianza_producto_2000)])
                            
                            