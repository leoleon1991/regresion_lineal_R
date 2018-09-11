# Importamos nuestra data "articulos_ml.csv"
datos = articulos_ml

# Aplicamos attach para manipular las columnas de la data
attach(datos)

#Dimension de la data
dim(datos)

#primeros registros
head(datos)
str(datos)

#Summary de la data
summary(datos)

#Histogramas de la data de entrada
par(mfrow=c(3,2), bg="light gray")
hist(`Word count`, main = "Word count", col = "red")
hist(`# of Links`, main = "Nro de Links", col = "red")
hist(`# of comments`, main = "Nro de Comentarios", col = "red")
hist(`# Images video`, main = "Nro de Imagenes y videos", col = "red")
hist(`Elapsed days`, main = "Nro de dias transcurridos", col = "red")
hist(`# Shares`, main = "Nro de Shares", col = "red")

#RECORTAR los datos en la zona donde se concentran más los puntos
# esto es en el eje X: entre 0 y 3.500
# y en el eje Y: entre 0 y 80.000

word_count = `Word count`[`Word count` <= 3500]
word_count
length(word_count)

shares = `# Shares`[`# Shares` <= 80000]
shares
length(shares)

filtered_data = c(word_count, shares)
filtered_data

par(mfrow=c(1,1), bg="light gray")
plot(x = word_count, y = shares, type = "p", col=c("orange", "blue"))
#HAcer grafico para word count > 1808 en orange y <= 1808 en azul

# Asignamos nuestra variable de entrada X para entrenamiento y las etiquetas Y o variables independientes.  
X_train = word_count
Y_train = shares

# Creamos el objeto o modelo de Regresión Linear
mod = lm(Y_train ~ X_train)
mod

#Resumen estadistico del modelo
summary(mod)

#Visualizar la recta de regresion
abline(mod, col="red")

#Vamos a comprobar:
# Quiero predecir cuántos "Shares" voy a obtener por un artículo con 2.000 palabras,
# según nuestro modelo, hacemos:
articulo_predict = data.frame(X_train = 2000)
y_dosmil = predict(mod, articulo_predict)
y_dosmil
