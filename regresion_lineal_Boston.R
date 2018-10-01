#Se van a emplear funciones contenidas en el paquete MASS y datos del paquete ISLR.
require(MASS)
require(ISLR)
data("Boston")

# El dataset Boston del paquete MASS recoge la media del valor de la vivienda 
# en 506 áreas residenciales de Boston. Junto con el precio, se han registrado 
# 13 variables adicionales:

# crim: radio de criminalidad per cápita de cada ciudad.
# zn: Proporción de zonas residenciales con edificaciones de más de 25.000 pies cuadrados.
# indus: proporción de zona industrializada.
# chas: Si hay río en la ciudad (= 1 si hay río; 0 no hay).
# nox: Concentración de óxidos de nitrógeno (partes per 10 millón).
# rm: promedio de habitaciones por vivienda.
# age: Proporción de viviendas ocupadas por el propietario construidas antes de 1940.
# dis: Media ponderada de la distancias a cinco centros de empleo de Boston.
# rad: Índice de accesibilidad a las autopistas radiales.
# tax: Tasa de impuesto a la propiedad en unidades de $10,000.
# ptratio: ratio de alumnos/profesor por ciudad.
# black: 1000(Bk - 0.63)^2 donde Bk es la proporción de gente de color por ciudad.
# lstat: porcentaje de población en condición de pobreza.
# medv: Valor medio de las casas ocupadas por el dueño en unidades de $1000s.

# En primer lugar se realiza un análisis básico de los datos de forma numérica y gráfica.

install.packages("psych")
require(psych)

# La variable chas es una variable categórica por lo que se transforma a factor
Boston$chas <- as.factor(Boston$chas)
attach(Boston)
summary(Boston)

# Dado que hay muchas variables, se grafican por grupos de 4, excluyendo las
# categóricas
multi.hist(x = Boston[,1:3], dcol = c("blue", "red"), dlty = c("dotted", "solid"), main = "")
multi.hist(x = Boston[,5:9], dcol = c("blue", "red"), dlty = c("dotted", "solid"), main = "")
multi.hist(x = Boston[,10:14], dcol = c("blue", "red"), dlty = c("dotted", "solid"), main = "")

# REGRESION LINEAL SIMPLE
# Se pretende predecir el valor de la vivienda en función del porcentaje de 
# pobreza de la población, es decir Y = medv, X = lstat. 

modelo_simple <- lm(data = Boston, formula = medv ~ lstat)

# Para ver el contenido del modelo se emplea la función names()
names(modelo_simple)

# Para visualizar los principales parámetros del modelo generado se utiliza summary()
summary(modelo_simple)

# En la información devuelta por el summary se observa que el p-value del 
# estadístico F es muy pequeño, indicando que al menos uno de los predictores del 
# modelo está significativamente relacionado con la variable respuesta. 
# Al tratarse de un modelo simple, el p-value de estadístico F es el mismo que el 
# p-value del estadístico t del único predictor incluido en el modelo (lstat). 
# La evaluación del modelo en conjunto puede hacerse a partir de los valores RSE o 
# del valor R2 devuelto en el summary.

# Residual standar error (RSE): En promedio, cualquier predicción del modelo se aleja 
# 6.216 unidades del verdadero valor. Teniendo en cuenta que el valor promedio 
# de la variable respuesta medv es de 22.53, RSE es de 6.216/22.53=27%.

# R2 : El predictor lstatus empleado en el modelo es capaz de explicar el 54.44% 
# de la variabilidad observada en el precio de las viviendas.

# Los dos coeficientes de regresión (β0 y β1) estimados por el modelo son 
# significativos y se pueden interpretar como:

# - Intercept(β0): El valor promedio del precio de la vivienda cuando el lstatus es 0 
# es de 34.5538 unidades.

# - Predictor lstat(β1): por cada unidad que se incrementa el predictor lstat 
# el precio de la vivienda disminuye en promedio 0.9500 unidades.

# La estimación de todo coeficiente de regresión tiene asociada un error estándar, 
# por lo tanto todo coeficiente de regresión tiene su correspondiente intervalo de confianza.

confint(object = modelo_simple, level = 0.95)

# Como era de esperar dado que el p-value del predictor lstat ha resultado 
# significativo para un α=0.05, su intervalo de confianza del 95% no contiene el valor 0.

# Una vez generado el modelo, es posible predecir el valor de la vivienda sabiendo 
# el estatus de la población en la que se encuentra. Toda predicción tiene asociado 
# un error y por lo tanto un intervalo. Es importante diferenciar entre dos tipos de intervalo:

# Intervalo de confianza: Devuelve un intervalo para el valor promedio de todas las 
# viviendas que se encuentren en una población con un determinado porcentaje de pobreza, 
# supóngase lstat=10.
predict(object = modelo_simple, newdata = data.frame(lstat = c(10)), 
        interval = "confidence", level = 0.95)

# Intervalo de predicción: Devuelve un intervalo para el valor esperado de una 
# vivienda en particular que se encuentre en una población con un determinado 
# porcentaje de pobreza.
predict(object = modelo_simple, newdata = data.frame(lstat = c(10)), 
        interval = "prediction", level = 0.95)

# Como es de esperar ambos intervalos están centrados en torno al mismo valor. 
# Si bien ambos parecen similares, la diferencia se encuentra en que los intervalos
# de confianza se aplican al valor promedio que se espera de "y" para un determinado
# valor de x, mientras que los intervalos de predicción no se aplican al promedio. 
# Por esta razón los segundos siempre son más amplios que los primeros.

# La creación de un modelo de regresión lineal simple suele acompañarse de una 
# representación gráfica superponiendo las observaciones con el modelo. 
# Además de ayudar a la interpretación, es el primer paso para identificar 
# posibles violaciones de las condiciones de la regresión lineal.
plot(x = lstat, y = medv, main = "medv vs lstat", pch = 20, col = "grey30")
abline(modelo_simple, lwd = 3, col = "red")

# La representación gráfica de las observaciones muestra que la relación entre 
# ambas variables estudiadas no es del todo lineal, lo que apunta a que otro tipo 
# de modelo podría explicar mejor la relación. Aun así la aproximación no es mala.

# Una de las mejores formas de confirmar que las condiciones necesarias para un 
# modelo de regresión lineal simple por mínimos cuadrados se cumplen es mediante 
# el estudio de los residuos del modelo.

# Gráficos más típicos para la evaluación de los residuos de un modelo.
par(mfrow = c(2,2))
plot(modelo_simple)
par(mfrow = c(1,1))

# Grafico 1:  Los residuos confirman que los datos no se distribuyen de forma 
# lineal, ni su varianza constante

# Grafico 2: Además se observa que la distribución de los residuos no es normal

# Grafico 3: Algunas observaciones tienen un residuo estandarizado absoluto mayor 
# de 3 (1.73 si se considera la raíz cuadrada) lo que es indicativo de observación 
# atípica

# Grafico 4: Valores de Leverages (hat) mayores que 2.5x((p+1)/n), siendo p el 
# número de predictores y n el número de observaciones, o valores de Cook mayores 
# de 1 se consideran influyentes.

# Todo ello reduce en gran medida la robustez de la estimación del error estándar
# de los coeficientes de correlación estimados y con ello la del modelo es su conjunto.

# Otra forma de identificar las observaciones que puedan ser outliers o puntos con 
# alta influencia (leverage) es emplear las funciones rstudent() y hatvalues().
plot(x = modelo_simple$fitted.values, y = abs(rstudent(modelo_simple)),
     main = "Absolute studentized residuals vs predicted values", pch = 20,
     col = "grey30")
abline(h = 3, col = "red")

plot(hatvalues(modelo_simple), main = "Medicion de leverage", pch = 20)
# Se añade una línea en el threshold de influencia acorde a la regla
# 2.5x((p+1)/n)

abline(h = 2.5*((dim(modelo_simple$model)[2]-1 + 1)/dim(modelo_simple$model)[1]),
       col = "red")

# En este caso muchos de los valores parecen posibles outliers o puntos con alta
# influencia porque los datos realmente no se distribuyen de forma lineal en los extremos.

# REGRESION LINEAL MULTIPLE
#  Se desea generar un modelo que permita explicar el precio de la vivienda de una 
# población empleando para ello cualquiera de las variables disponibles en el 
# dataset Boston y que resulten útiles en el modelo.

# crear un modelo con todas las variables incluidas en un data.frame
modelo_multiple <- lm(formula = medv ~ ., data = Boston)
summary(modelo_multiple)

# El p-value obtenido para el estadístico F es muy pequeño (< 2.2e-16) lo que 
# indica que al menos uno de los predictores introducidos en el modelo está 
# relacionado con la variable respuesta medv. El modelo es capaz de explicar el 
# 74% de la variabilidad observada en el precio de la vivienda (R2=0.74)

# En el summary se puede observar que algunos predictores tienen p-values muy altos,
# sugiriendo que no contribuyen al modelo por lo que deben ser excluidos, por ejemplo 
# age e indus. La exclusión de predictores basándose en p-values no es aconsejable,
# en su lugar se recomienda emplear métodos de best subset selection, stepwise selection 
# (forward, backward e hybrid) o Shrinkage/regularization.

step(modelo_multiple, direction = "both", trace = 0)

# La selección de predictores empleando stepwise selection (hybrid/doble) ha 
# identificado como mejor modelo el formado por los predictores crim, zn, chas, 
# nox, rm, dis, rad, tax, ptratio, black, lstat.

modelo_multiple <- lm(formula = medv ~ crim + zn + chas +  nox + rm +  dis +
                      rad + tax + ptratio + black + lstat, data = Boston)

# También se pueden indicar todas las variables de un data.frame y exluir algunas
# modelo_multiple <- lm(formula = medv~. -age -indus, data = Boston)
summary(modelo_multiple)

# En los modelos de regresión lineal con múltiples predictores, además 
# del estudio de los residuos vistos en el modelo simple, es necesario descartar 
# colinealidad o multicolinealidad entre variables.
par(mfrow = c(2,2))
plot(modelo_multiple)
par(mfrow = c(1,1))

# Para la colinealidad se recomienda calcular el coeficiente de correlación entre
# cada par de predictores incluidos en el modelo:

install.packages("corrplot")
require(corrplot)
corrplot.mixed(corr = cor(Boston[, c("crim", "zn", "nox", "rm", "dis", "rad", 
                                     "tax", "ptratio", "black", "lstat", "medv")],
                          method = "pearson"))

# El análisis muestra correlaciones muy altas entre los predictores rad y tax 
# (positiva) y entre dis y nox (negativa).

attach(Boston)
par(mfrow = c(2,2))
plot(x = tax, y = rad, pch = 20)
plot(x = tax, y = nox, pch = 20)
plot(x = dis, y = nox, pch = 20)
plot(x = medv, y = rm, pch = 20)
par(mfrow = c(1,1))

# Si la correlación es alta y por lo tanto las variables aportan información 
# redundante, es recomendable analizar si el modelo mejora o no empeora excluyendo 
# alguno de estos predictores.

# Para el estudio de la multicolinealidad una de las medidas más utilizadas es el
# factor de inflación de varianza VIF. Puede calcularse mediante la función vif() 
# del paquete car.

install.packages("car")
require(car)
vif(modelo_multiple)

# Los indices VIF son bajos o moderados, valores entre 5 y 10 indican posibles 
# problemas y valores mayores o iguales a 10 se consideran muy problemáticos. 

# INTERACCION ENTRE PREDICTORES
# Una de las asunciones del modelo de regresión lineal múltiple es la de aditividad, 
# según la cual los efectos que causan sobre la variable respuesta Y variaciones 
# en el predictor Xi son independientes del valor que tomen los otros predictores. 
# Se conoce como efecto de interacción cuando el efecto de un predictor varía 
# dependiendo del valor que adquiera otro predictor. Si esto ocurre, el modelo 
# mejorará si se incluye dicha interacción. Si en un modelo se incorpora una 
# interacción, se deben incluir también los predictores individuales que forman 
# la interacción aun cuando por sí solos no sean significativos.

# Supóngase que empleando las variables age y lstat del data set Boston se quiere
# generar un modelo lineal que permita predecir el valor medio de la vivienda medv.

# El modelo lineal múltiple empleando ambos predictores resulta en:
modelo <- lm(medv ~ lstat + age, data = Boston)
summary(modelo)  

# Dado que es un modelo con dos predictores continuos se puede representar el plano de regresión.

rango_lstat <- range(Boston$lstat)
nuevos_valores_lstat <- seq(from = rango_lstat[1], to = rango_lstat[2], 
                            length.out = 20)

rango_age <- range(Boston$age)
nuevos_valores_age <- seq(from = rango_age[1], to = rango_age[2],
                          length.out = 20)

predicciones <- outer(X = nuevos_valores_lstat, Y = nuevos_valores_age, 
                      FUN = function(lstat, age) {
                        predict(object = modelo, newdata = data.frame(lstat, age))
                      })

superficie <- persp(x = nuevos_valores_lstat, y = nuevos_valores_age, 
                    z = predicciones, 
                    theta = 20, phi = 5, 
                    col = "lightblue", shade = 0.1, 
                    zlim = range(-10,100), xlab = "lstat", ylab = "age", zlab = "medv", 
                    ticktype = "detailed", 
                    main = "Prediccion precio medio ~ lstat y age")

observaciones <- trans3d(Boston$lstat, Boston$age, Boston$medv, superficie)
error <- trans3d(Boston$lstat, Boston$age, fitted(modelo), superficie)
points(observaciones, col = "red", pch = 16)
segments(observaciones$x, observaciones$y, error$x, error$y)

# En R se puede generar un modelo con interacción de dos formas: indicando de 
# forma explícita los predictores individuales y entre que predictores se quiere 
# evaluar la interacción

lm(medv ~ lstat + age + lstat:age, data = Boston)

# O de forma directa:

modelo_interaccion <- lm(medv ~ lstat * age, data = Boston)
summary(modelo_interaccion)

#  La interacción, aunque significativa, apenas aporta mejora al modelo, 
# R-squared sin interacción = 0.5513 y R-squared con interacción = 0.5557. 
# Por lo tanto, siguiendo el principio de parsimonia, el modelo más adecuado 
# (dentro de lo limitado ya que solo explica el 55% de variabilidad) es el 
# modelo sin interacción. 

# REGRESION POLINOMIAL: INCORPORAR NO-LINEALIDAD A LOS MODELOS LINEALES.
# La Regresión Polinomial, aunque permite describir relaciones no lineales, 
# se trata de un modelo lineal en el que se incorporan nuevos predictores 
#elevando el valor de los ya existentes a diferentes potencias.

# Cuando se intenta predecir el valor de la vivienda en función del estatus de la
# población, el modelo lineal generado no se ajusta del todo bien debido a que las 
# observaciones muestran una relación entre ambas variables con cierta curvatura.

attach(Boston)
plot(x = lstat, y = medv, main = "medv vs lstat", pch = 20, col = "grey30")
abline(modelo_simple, lwd = 3, col = "red")

# La curvatura descrita apunta a una posible relación cuadrática, por lo que un 
# polinomio de segundo grado podría capturar mejor la relación entre las variables.

# Se pueden generar modelos de regresión polinómica de diferentes formas:
# - Identificando cada elemento del polinomio: 
# modelo_pol2 <- lm(formula = medv ~ lstat + I(lstat^2), data = Boston) 
# El uso de I() es necesario ya que el símbolo ^ tiene otra función dentro de las
# formulas de R.

# - Con la función poly(): lm(formula = medv ~ poly(lstat, 2), data = Boston)

modelo_pol2 <- lm(formula = medv ~ poly(lstat, 2), data = Boston)
summary(modelo_pol2)

# El p-value próximo a 0 del predictor cuadrático de lstat indica que contribuye 
# a mejorar el modelo.

# Grafica del nuevo modelo
plot(x = lstat, y = medv, main = "medv vs lstat cuadratico", pch = 20, col = "grey30")
points(lstat, fitted(modelo_pol2), col = "red", pch = 20)

# A la hora de comparar dos modelos se pueden evaluar sus R2. En este caso el 
# modelo cuadrático es capaz de explicar un 64% de variabilidad frente al 54% del
# modelo lineal.

# Cuando se comparan dos modelos anidados (el modelo de menor tamaño está formado
# por un subset de predictores del modelo mayor), se puede saber si el modelo 
# mayor aporta una mejora sustancial estudiando si los coeficientes de regresión 
# de los predictores adicionales son distintos a cero. 
# El test estadístico empleado para hacerlo es el ANOVA.

# Modelomenor:  y=β0+β1x1+...+βkxk10
# Modelomayor:  y=β0+β1x1+...+βkxk+βk+1xk+1+...+βpxp

# La hipótesis a contrastar es que todos los coeficientes de regresión de los 
# predictores adicionales son igual a cero, frente a la hipótesis alternativa de 
# que al menos uno es distinto.

# H0:βk+1=...=βp

# El estadístico empleado es:

# F=(SEEModelomenor−SEEModelomayor)/(p−k)SEEModelomayor/(n−p−1)

# Dado que un polinomio de orden n siempre va a estar anidado a uno de orden n+1,
# se pueden comparar modelos polinómicos dentro un rango de grados haciendo 
# comparaciones secuenciales.

anova(modelo_simple, modelo_pol2)

#  El p-value obtenido para el estadístico F confirma que el modelo cuadrático 
# es superior.
