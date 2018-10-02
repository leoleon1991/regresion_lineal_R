# Un analista de deportes quiere saber si existe una relación entre el número de bateos
# que realiza un equipo de béisbol y el número de runs que consigue. En caso de 
# existir y de establecer un modelo, podría predecir el resultado del partido.

equipos <- c("Texas","Boston","Detroit","Kansas","St.","New_S.","New_Y.",
             "Milwaukee","Colorado","Houston","Baltimore","Los_An.","Chicago",
             "Cincinnati","Los_P.","Philadelphia","Chicago","Cleveland","Arizona",
             "Toronto","Minnesota","Florida","Pittsburgh","Oakland","Tampa",
             "Atlanta","Washington","San.F","San.I","Seattle")

numero_bateos <- c(5659,  5710, 5563, 5672, 5532, 5600, 5518, 5447, 5544, 5598,
                   5585, 5436, 5549, 5612, 5513, 5579, 5502, 5509, 5421, 5559,
                   5487, 5508, 5421, 5452, 5436, 5528, 5441, 5486, 5417, 5421)

runs <- c(855, 875, 787, 730, 762, 718, 867, 721, 735, 615, 708, 644, 654, 735,
          667, 713, 654, 704, 731, 743, 619, 625, 610, 645, 707, 641, 624, 570,
          593, 556)

datos <- data.frame(equipos,numero_bateos,runs)
attach(datos)
head(datos)

# Verificar linealidad entre las variables mediante representacion grafica
require(ggplot2)
ggplot(data = datos, mapping = aes(x = numero_bateos, y = runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

cor.test(x = datos$numero_bateos, y = datos$runs, method = "pearson")

# El gráfico y el test de correlación muestran una relación lineal, de intensidad 
# considerable (r = 0.61) y significativa (p-value = 0.0003388). Tiene sentido 
# intentar generar un modelo de regresión lineal que permita predecir el número de
# runs en función del número de bateos del equipo. 

# Calcular el modelo de Regresion Lineal
modelo_lineal_beisbol <- lm(runs ~ numero_bateos, datos)
summary(modelo_lineal_beisbol)

#  La primera columna (Estimate) devuelve el valor estimado para los dos 
# parámetros de la ecuación del modelo lineal (β0^ y β1^) que equivalen a la 
# ordenada en el origen y la pendiente.

# Se muestran los errores estándar, el valor del estadístico t y el p-value 
# (dos colas) de cada uno de los dos parámetros. Esto permite determinar si los 
# parámetros son significativamente distintos de 0, es decir, que tienen 
# importancia en el modelo. En los modelos de regresión lineal simple, el 
# parámetro más informativo suele ser la pendiente.

# Para el modelo generado, tanto la ordenada en el origen como la pendiente son 
# significativas (p-values < 0.05).

#  El valor de R2 indica que el modelo calculado explica el 37.29% de la 
# variabilidad presente en la variable respuesta (runs) mediante la variable 
# independiente (número de bateos).

# El p-value obtenido en el test F (0.0003388) determina que sí es 
# significativamente superior la varianza explicada por el modelo en comparación
# a la varianza total. Es el parámetro que determina si el modelo es significativo
# y por lo tanto se puede aceptar.

#  El modelo lineal generado sigue la ecuación runs = -2789.2429 + 0.6305 bateos. 
# Por cada unidad que se incrementa el número de bateos, el número de runs aumenta 
# en promedio 0.6305 unidades.

# Intervalos de confianza para los parametros del modelo

confint(modelo_lineal_beisbol)

# Representacion grafica del modelo
ggplot(data = datos, mapping = aes(x = numero_bateos, y = runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Runs ~ número de bateos', x  =  'número  de bateos') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Además de la línea de mínimos cuadrados es recomendable incluir los límites 
# superior e inferior del intervalo de confianza. Esto permite identificar la 
# región en la que, según el modelo generado y para un determinado nivel de 
# confianza, se encuentra el valor promedio de la variable dependiente.

# Para poder representar el intervalo de confianza a lo largo de todo el modelo se
# recurre a la función  predict() para predecir valores que abarquen todo el eje X. 
# Se añaden al gráfico líneas formadas por los límites superiores e inferiores 
# calculados para cada predicción.

# Se genera una secuencia de valores x_i que abarquen todo el rango de las
# observaciones de la variable X
puntos <- seq(from = min(datos$numero_bateos), to = max(datos$numero_bateos),
              length.out = 100)

# Se predice el valor de la variable Y junto con su intervalo de confianza para
# cada uno de los puntos generados. En la función predict() hay que nombrar a 
# los nuevos puntos con el mismo nombre que la variable X del modelo.
# Devuelve una matriz.
limites_intervalo <- predict(object = modelo_lineal_beisbol,
                             newdata = data.frame(numero_bateos = puntos),
                             interval = "confidence", level = 0.95)
head(limites_intervalo,3)

# Finalmente se añaden al gráfico las líneas formadas por los límites
# superior e inferior.
plot(datos$numero_bateos, datos$runs, col = "firebrick", pch = 19, ylab = "runs",
     xlab = "número de bateos", main = 'Runs ~ número de bateos')
abline(modelo_lineal_beisbol, col = 1)
lines(x = puntos, y = limites_intervalo[,2],type = "l", col = 2, lty = 3)
lines(x = puntos, y = limites_intervalo[,3],type = "l", col = 3, lty = 3)

# La función geom_smooth() del paquete ggplot2 genera la regresión y su intervalo de forma directa.
ggplot(data = datos, mapping = aes(x = numero_bateos, y = runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Verificar condiciones para poder aceptar un modelo lineal 

# Relación lineal entre variable dependiente e independiente: 
# Se calculan los residuos para cada observación y se representan (scatterplot). 
# Si las observaciones siguen la línea del modelo, los residuos se deben 
# distribuir aleatoriamente entorno al valor 0.

#La función lm() calcula y almacena los valores predichos por el modelo y los residuos.
datos$prediccion <- modelo_lineal_beisbol$fitted.values
datos$residuos <- modelo_lineal_beisbol$residuals
head(datos)

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Los residuos se distribuyen de forma aleatoria entorno al 0 por lo que se acepta la linealidad. 
#Distribución normal de los residuos: 
# Los residuos se deben distribuir de forma normal con media 0. Para comprobarlo 
# se recurre a histogramas, a los cuantiles normales o a un test de contraste de normalidad.

ggplot(data = datos, aes(x = residuos)) +
  geom_histogram(aes(y = ..density..)) +
  labs(title = "histograma de los residuos") +
  theme_light()

qqnorm(modelo_lineal_beisbol$residuals)
qqline(modelo_lineal_beisbol$residuals)
shapiro.test(modelo_lineal_beisbol$residuals)

# Tanto la representación gráfica como el contraste de hipótesis confirman la distribución normal de los residuos. 

# Varianza constante de los residuos (Homocedasticidad): 
# La variabilidad de los residuos debe de ser constante a lo largo del eje X. Un patrón cónico es indicativo de falta de homogeneidad en la varianza.

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  geom_smooth(se = FALSE, color = "firebrick") +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#Ni la representación gráfica ni el contraste de hipótesis muestran evidencias que haga sospechar falta de homocedasticidad. 

# Test de Breush-Pagan
install.packages("lmtest")
library(lmtest)
bptest(modelo_lineal_beisbol)

# Ni la representación gráfica ni el contraste de hipótesis muestran evidencias que haga sospechar falta de homocedasticidad.

# Autocorrelación de residuos: 
# Cuando se trabaja con intervalos de tiempo, es muy importante comprobar que no 
# existe aoutocorrelación de los residuos, es decir que son independientes. 
# Esto puede hacerse detectando visualmente patrones en la distribución de los 
# residuos cuando se ordenan según se han registrado o con el test de Durbin-Watson 
# dwt() del paquete Car.

ggplot(data = datos, aes(x = seq_along(residuos), y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_line(size = 0.3) +
  labs(title = "Distribución de los residuos", x = "index", y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# En este caso, la representación de los residuos no muestra ninguna tendencia. 
