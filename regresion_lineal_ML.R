# Importamos nuestra data "articulos_ml.csv"
datos = articulos_ml

# Aplicar attach para manipular las columnas de la data
attach(datos)

#Dimension de la data
dim(datos)

#primeros registros
head(datos)

#Summary de la data
summary(datos)

# Creacion de data frame para mejor manipulacion de los datos
df_datos = data.frame(datos)

# Asignar nombres a las columnas
colnames(df_datos) = c("title", "url", "wordcount", "links", "comments", "images_videos", "elapsed_days", "shares")

#Histogramas de la data de entrada
par(mfrow=c(3,2), bg="light gray")
hist(df_datos$wordcount, main = "Word count", col = "red")
hist(df_datos$links, main = "Nro de Links", col = "red")
hist(df_datos$comments, main = "Nro de Comentarios", col = "red")
hist(df_datos$images_videos, main = "Nro de Imagenes y videos", col = "red")
hist(df_datos$elapsed_days, main = "Nro de dias transcurridos", col = "red")
hist(df_datos$shares, main = "Nro de Shares", col = "red")

#Filtrar los datos en la zona donde se concentran mas los puntos
# esto es en el eje X: entre 0 y 3.500
# y en el eje Y: entre 0 y 80.000

filter_data = subset(df_datos, wordcount <= 3500 & shares <= 80000)
View(filter_data)

#Grafica de valores filtrados
par(mfrow=c(1,1), bg="light gray")
plot(x = filter_data$wordcount, y = filter_data$shares, type = "p", col=c("orange", "blue"))

# Asignamos nuestra variable de entrada X para entrenamiento y las etiquetas Y o variables independientes.  
X_train = filter_data$wordcount
Y_train = filter_data$shares

# Creamos el objeto o modelo de Regresion Linear
mod = lm(Y_train ~ X_train)
mod

#Resumen estadistico del modelo
summary(mod)

#Visualizar la recta de regresion
abline(mod, col="red")

# Para comprobar:
# Quiero predecir cuantos "Shares" voy a obtener por un articulo con 2.000 palabras,
# segun nuestro modelo, hacemos:
y_dosmil = predict(mod, data.frame(X_train = 2000))
y_dosmil

# REGRESION LINEAL MULTIPLE
# intentar mejorar el Modelo, con una dimensi�n m�s: 
# Para poder graficar en 3D, se creara una variable nueva que ser� la suma de los enlaces, comentarios e im�genes

suma = filter_data$links + (filter_data$comments[is.na(filter_data$comments)] <- 0) + filter_data$images_videos

dataX2 = data.frame(filter_data$wordcount, suma)
colnames(dataX2) = c("Word Count", "Suma")

XY_train = as.numeric(dataX2)
z_train = filter_data$shares
length(XY_train)

word_count = filter_data$wordcount
links = filter_data$links
comments = filter_data$comments
images_videos = filter_data$images_videos  

mod2 = lm(z_train ~ word_count + links + comments + images_videos, data = filter_data)
summary(mod2)

y_dosmil = predict(mod, data.frame(X_train = 2000))
y_dosmil

z_Dosmil = predict(mod2, data.frame(word_count = 2000, links = 10, comments = 4, images_videos = 6))
z_Dosmil                   
