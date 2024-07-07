#Para filtrar datos de una tabla STATA  en RSTUDIO, primero se necesita importar los datos a R y luego tenemos que utilizar las funciones del siguiente paquetes
#'dplyr' para realizar el filtrado 

###instalamos los paquetes

#NOTA: Se tienen que ir ejecutando los paquetes uno a uno, ejemplo seleccionar una linea luego presionar en RUN y esperar a que nuestra consola nos indique
#Que termino de realizar la funcion que se le esta indicando
install.packages("haven")
install.packages("dplyr")

###importamos las librerias
#Paquete 'haven' nos permite importar datos de STATA
library(haven)
#Paquete 'dplyr' este paquete nos permite manipular los datos de nuestra DB 
library(dplyr)
#asignamos un nuevo nombre al nuevo conjunto de datos que se llamara data y este obtendra los datos mediante un dataset de nuestro archivo de datos
#.dta [data] = Nombre de nuestra variable que contiene nuestros datos <- le asignamos un apuntador donde le asignamos una funcion llamada [read_stata]
#Luego le indicamos el path en donde se encuentra nuestro banco de datos incial debemos cambiar la ruta en donde se encuentra nuestro archivo en 
#o dentro de las comillas "../" lo recomendable para que funcione en cualquier pc con windows es tenerla dentro de C:/ en este caso como se utiliza
#una distribucion Linux de Ubuntu los path de las rutas cambian...
data <- read_stata("C:/V-Dem-CY-FullOthers-v14_stata/V-Dem-CY-Full+Others-v14.dta")

#Creamos una nueva partida de datos, en donde utilizamos la funcion filter en donde 
#filter_data = nueva base de datos donde solo tendra nuestra consulta con los filtros de los datos
#data = a la variable que tiene todos los datos originales
# %>% = Variables a comparar dependiento de la consulta es como contruimos nuestro comparador logico
# filter = funcion que nos permitira filtrar por columnas especificamente nuestros datos
filter_data <- data %>% filter(country_name == "Poland", year > 1980, e_gdppc > 10.0)

#Para mostrar nuestros datos podemos realizarlo de 2 maneras en una vista en una tabla o bien mediante la consola de la terminal en donde:
#View = la funcion que nos retorna una vista grafica nueva (datagrid)
#filter_data = a el nuevo conjunto de datos que estamos filtrando de nuestro origen o banco de datos
View(filter_data)

#En el siguiente modo podemos imprimir nuestros querys o consultas (filtros) en la consola de nuestro Rstudio <- Consume menos memoria 
#print le indica que queremos que nuestros datos se impriman o muestren en nuestra consola
#filter_data = a el nuevo conjunto de datos que estamos filtrando de nuestro origen o banco de datos
print(filter_data)

#Debemos Seleccionar solo las columnas que nos interesan para poder trabajar de una manera mas comoda
#Supongamos que deseas seleccionar las columnas var1, var2 y var3.
# Selecciona solo las columnas var1, var2 y var3
selected_data <- filter_data %>%select(country_name, year, e_gdppc, v2x_regime, e_boix_regime, v2xps_party, v2lginvstp, v2x_libdem)

#Imprimimos la seleccion de los datos en la consola:
print(selected_data)

#Imprimimos los datos seleccionados en una vista
View(selected_data)


#####----------ANALISIS DE ESTADISTICA DESCRIPTIVA ---------------------

#Para realizar una tabla de estadísticas descriptivas y una visualización gráfica de los datos en RStudio, se implementan los paquetes dplyr y ggplot2.
#creación de la tabla descriptiva como la visualización gráfica.

#Instalar y cargar los paquetes necesarios:
install.packages("ggplot2")
install.packages("psych")  # para estadísticas descriptivas adicionales

#Importacion de las librerias
library(ggplot2)
library(psych)
###----------------UNA TABLA PARA TENER UN PRIMER VISTAZO-----------------------------------------------------------------
# Tabla de estadísticas descriptivas
descriptive_stats <- describe(selected_data)
print(descriptive_stats)

# Visualización gráfica
# Histograma de year
ggplot(selected_data, aes(x = year )) +
  geom_histogram(binwidth = 3.7, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histograma de Polonia", x = "Year", y = "Frecuencia")

# Gráfico de dispersión de var1 y var2
ggplot(selected_data, aes(x = e_gdppc, y = year)) +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(title = "Gráfico de Dispersión de Polonia", x = "country_name", y = "year")

# Boxplot de var1
ggplot(selected_data, aes(y = v2xps_party)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot de Índice de institucionalización de partidos", y = "Índice de institucionalización de partidos")

#####------------------------GRAFICOS PARA COMPRENDER DE MEJOR MANERA LOS DATOS--------------------------------------
#Crear gráficos que nos permitan tener una mejor descripción de los datos y la relación
#entre las variables independientes en función de la variable dependiente. En ese
#sentido, se les pide no solo graficar, sino también la interpretación de

# Gráfico de dispersión de v2x_libdem = Indice de democracia liberal vs v2x_regime = Tipo de regimen politico, facetado por Índice de institucionalización de partidos
ggplot(selected_data, aes(x = v2x_libdem, y = v2x_regime)) +
  geom_point(color = "blue") +
  facet_wrap(~ v2xps_party) +
  theme_minimal() +
  labs(title = "Relación entre v2x_libdem = Indice de democracia liberal vs v2x_regime = Tipo de regimen politico, facetado por Índice de institucionalización de partidos",
       x = "Indice de democracia liberal", y = "Tipo de regimen politico")

# Boxplot de var_dep agrupado por v2x_regime = Tipo de régimen político
ggplot(selected_data, aes(x = v2x_regime, y = v2x_libdem)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot de var_dep agrupado por v2x_regime = Tipo de régimen político", x = "Tipo de régimen político", y = "Índice de democracia liberal")


# Gráfico de densidad de v2x_libdem = Índice de democracia liberal, coloreado por v2x_regime = Tipo de régimen político
ggplot(data, aes(x = v2x_libdem, fill = v2x_regime)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Densidad de v2x_libdem = Índice de democracia liberal coloreado por v2x_regime = Tipo de régimen político", x = "Índice de democracia liberal", y = "Densidad")

###########===========================La variable dependiente en función a las cinco variables independientes + (año)
# Histograma de var_dep por año
ggplot(filter_data, aes(x = v2x_libdem)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  facet_wrap(~ year) +
  theme_minimal() +
  labs(title = "Histograma de var_dep por año", x = "v2x_libdem = Índice de democracia liberal", y = "Frecuencia")

# Gráfico de dispersión de var_dep vs var_ind1, coloreado por año
ggplot(filter_data, aes(x = e_gdppc, y = v2x_libdem, color = factor(year))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relación entre e_gdppc = Producto Interno Bruto (PIB) per cápita y v2x_libdem = Índice de democracia liberal coloreado por año", x = "e_gdppc", y = "v2x_libdem")

# Gráfico de dispersión de v2x_libdem = Índice de democracia liberal vs v2x_regime = Tipo de régimen político, coloreado por año
ggplot(filter_data, aes(x = v2x_regime, y = v2x_libdem, color = factor(year))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relación entre v2x_regime = Tipo de régimen político y v2x_libdem = Índice de democracia liberal coloreado por año", x = "v2x_regime = Tipo de régimen político", y = "v2x_libdem = Índice de democracia liberal")

# Gráfico de dispersión de v2x_libdem = Índice de democracia liberal vs e_boix_regime = Medida para la democracia, coloreado por año
ggplot(filter_data, aes(x = e_boix_regime, y = v2x_libdem, color = factor(year))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relación entre e_boix_regime = Medida para la democracia y var_dep coloreado por año", x = "e_boix_regime = Medida para la democracia", y = "v2x_libdem = Índice de democracia liberal")

# Gráfico de dispersión de v2x_libdem = Índice de democracia liberal vs v2xps_party = Índice de institucionalización de partidos, coloreado por año
ggplot(filter_data, aes(x = v2xps_party, y = v2x_libdem, color = factor(year))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relación entre var_ind4 y var_dep coloreado por año", x = "v2xps_party = Índice de institucionalización de partidos", y = "v2x_libdem = Índice de democracia liberal")

# Gráfico de dispersión de v2x_libdem = Índice de democracia liberal vs v2lginvstp = La legislatura investiga en la práctica, coloreado por año
ggplot(filter_data, aes(x = v2lginvstp, y = v2x_libdem, color = factor(year))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relación entre var_ind5 y var_dep coloreado por año", x = "v2lginvstp = La legislatura investiga en la práctica", y = "v2x_libdem = Índice de democracia liberal")

# Boxplot de var_dep por año
ggplot(filter_data, aes(x = factor(year), y = v2x_libdem)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot de v2x_libdem = Índice de democracia liberal por año", x = "Año", y = "v2x_libdem = Índice de democracia liberal")

# Gráfico de densidad de v2x_libdem = Índice de democracia liberal coloreado por año
ggplot(filter_data, aes(x = v2x_libdem, fill = factor(year))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Densidad de v2x_libdem = Índice de democracia liberal coloreado por año", x = "v2x_libdem = Índice de democracia liberal", y = "Densidad")





###comprobar esa relación bivariado mediante un modelo de regresión de mínimos
modelo <- lm(v2x_libdem ~ e_gdppc, data = filter_data)
summary(modelo)
# Gráfico de dispersión con línea de regresión
ggplot(data, aes(x = e_gdppc, y = v2x_libdem)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Relación entre PIB per cápita y el Índice de Democracia Liberal",
       x = "PIB per cápita (e_gdppc)",
       y = "Índice de Democracia Liberal (v2x_libdem)")
#Interpretación del resultado
#1 - Salida del resumen del modelo: El resumen del modelo proporcionará coeficientes de regresión, errores estándar, valores t y p, R-squared, y otras estadísticas relevantes.
#2 - Gráfico de dispersión con la línea de regresión: Este gráfico te permitirá visualizar la relación entre el PIB per cápita y el Índice de Democracia Liberal, así como la línea de ajuste resultante de la regresión lineal.




###-------------------------Calcular el intervalo de confianza para los coeficientes de esta regresión.
# Realizar la regresión lineal
modelo <- lm(v2x_libdem ~ e_gdppc, data = filter_data)

# Resumen del modelo
summary(modelo)

# Calcular el intervalo de confianza para los coeficientes
confint(modelo)

# Gráfico de dispersión con línea de regresión
ggplot(data, aes(x = e_gdppc, y = v2x_libdem)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Relación entre PIB per cápita y el Índice de Democracia Liberal",
       x = "PIB per cápita (e_gdppc)",
       y = "Índice de Democracia Liberal (v2x_libdem)")
#Interpretación del resultado
# 1 - Salida del resumen del modelo: El resumen del modelo proporcionará coeficientes de regresión, errores estándar, valores t y p, R-squared, y otras estadísticas relevantes.
# 2- Intervalo de confianza para los coeficientes: La salida de confint(modelo) proporcionará los intervalos de confianza al 95% para los coeficientes de la regresión, lo cual te permitirá entender el rango en el cual se espera que los verdaderos coeficientes se encuentren con un 95% de confianza.

#Este procedimiento te permitirá obtener tanto los coeficientes de la regresión como sus intervalos de confianza, proporcionando una visión más completa de la incertidumbre asociada con las estimaciones de los coeficientes.





#----------------------------------Poner en una tabla los resultados de la regresión.
install.packages("broom")
library(broom)

# Realizar la regresión lineal
modelo <- lm(v2x_libdem ~ e_gdppc, data = filter_data)

# Obtener los resultados de la regresión en formato de tabla
resultados <- tidy(modelo)

# Calcular los intervalos de confianza para los coeficientes
intervalos_confianza <- confint(modelo)
colnames(intervalos_confianza) <- c("conf.low", "conf.high")

# Unir los resultados de la regresión con los intervalos de confianza
resultados <- cbind(resultados, intervalos_confianza)

# Mostrar la tabla con los resultados de la regresión
print(resultados)

# Gráfico de dispersión con línea de regresión
ggplot(data, aes(x = e_gdppc, y = v2x_libdem)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Relación entre PIB per cápita y el Índice de Democracia Liberal",
       x = "PIB per cápita (e_gdppc)",
       y = "Índice de Democracia Liberal (v2x_libdem)")

#Salida esperada de la tabla
#La tabla resultante (resultados) debería contener las siguientes columnas:
#term: El nombre del coeficiente (por ejemplo, (Intercept) para el intercepto y e_gdppc para el coeficiente del PIB per cápita).
#estimate: La estimación del coeficiente.
#std.error: El error estándar de la estimación.
#statistic: El valor t del coeficiente.
#p.value: El valor p asociado con el coeficiente.
#conf.low: El límite inferior del intervalo de confianza al 95%.
#conf.high: El límite superior del intervalo de confianza al 95%.
#Este procedimiento te permitirá tener una tabla clara y ordenada con todos los resultados importantes de tu regresión, incluyendo los intervalos de confianza.





####--------------Realizar la interpretación tanto estadística como sustantiva del efecto de la variable independiente sobre la variable dependiente.
#Para interpretar los resultados de la regresión entre el Índice de Democracia Liberal (v2x_libdem) y el Producto Interno Bruto per cápita (e_gdppc), es importante considerar tanto la interpretación estadística (coeficientes, significancia, intervalos de confianza) como la interpretación sustantiva (implicaciones reales del efecto).

# Interpretación Estadística

#1 - Coeficiente de regresión (estimate):
#El coeficiente de e_gdppc representa el cambio esperado en v2x_libdem por cada unidad adicional en e_gdppc, manteniendo constantes otras variables (en este caso, no hay otras variables).

#2 - Error estándar (std.error):
#El error estándar proporciona una medida de la precisión de la estimación del coeficiente. Un menor error estándar indica una estimación más precisa.

#3 - Valor t (statistic):
#El valor t se utiliza para evaluar la significancia del coeficiente. Se calcula como el coeficiente dividido por su error estándar. Un valor t alto (en valor absoluto) indica que es menos probable que el coeficiente sea cero.

#4- Valor p (p.value):
#El valor p indica la probabilidad de observar un valor del coeficiente tan extremo como el observado, si la verdadera relación fuera nula. Un valor p menor a un nivel de significancia común (por ejemplo, 0.05) indica que el coeficiente es significativamente diferente de cero.

#5 - Intervalo de confianza (conf.low, conf.high):
#El intervalo de confianza proporciona un rango dentro del cual es probable que se encuentre el verdadero valor del coeficiente con un cierto nivel de confianza (por ejemplo, 95%). Si el intervalo no incluye cero, el coeficiente es significativo al nivel de confianza especificado.



#---------------------------------Interpretación Sustantiva--------------------------------------
#1 - Efecto del PIB per cápita (e_gdppc) sobre el Índice de Democracia Liberal (v2x_libdem):
#Si el coeficiente de e_gdppc es positivo y significativo, se puede interpretar que a medida que aumenta el PIB per cápita, el Índice de Democracia Liberal también tiende a aumentar. Esto sugiere que países con mayor PIB per cápita tienden a tener niveles más altos de democracia liberal.
#Si el coeficiente es negativo y significativo, la interpretación sería que un aumento en el PIB per cápita está asociado con una disminución en el Índice de Democracia Liberal.

#2 - Implicaciones prácticas:
#Si se encuentra una relación positiva y significativa, esto podría implicar que políticas orientadas a mejorar el crecimiento económico (aumento del PIB per cápita) también podrían tener efectos beneficiosos sobre la democracia liberal.
#Si no se encuentra una relación significativa, podría sugerir que el PIB per cápita no es un determinante importante de la democracia liberal, y que otros factores podrían ser más relevantes.


#------------------------Ejemplo de Interpretación con Resultados Hipotéticos
# Supuestos resultados del modelo
resultados <- data.frame(
  term = c("(Intercept)", "e_gdppc"),
  estimate = c(0.5, 0.001),
  std.error = c(0.1, 0.0002),
  statistic = c(5, 5),
  p.value = c(0.0001, 0.0001),
  conf.low = c(0.3, 0.0006),
  conf.high = c(0.7, 0.0014)
)

print(resultados)

#--------------------Interpretación Estadística:
# Intercepto
#Coeficiente: 0.5
#Error estándar: 0.1
#Valor t: 5
#Valor p: 0.0001 (significativo)
#Intervalo de confianza: [0.3, 0.7]



#e_gdppc:
#Coeficiente: 0.001
#Error estándar: 0.0002
#Valor t: 5
#Valor p: 0.0001 (significativo)
#Intervalo de confianza: [0.0006, 0.0014]

#----------------Interpretación Sustantiva:------------------
#El coeficiente de e_gdppc (0.001) indica que, en promedio, un aumento de 1000 unidades en el PIB per cápita se asocia con un incremento de 1 en el Índice de Democracia Liberal, manteniendo constantes otras variables (aunque en este caso no hay otras variables en el modelo).
#El valor p de 0.0001 sugiere que la relación es estadísticamente significativa al nivel del 1%, lo que significa que es muy improbable que esta relación se deba al azar.
#El intervalo de confianza para el coeficiente de e_gdppc [0.0006, 0.0014] no incluye cero, lo que confirma la significancia del coeficiente al 95% de confianza.

#En conclusión, existe una relación positiva y significativa entre el PIB per cápita y el Índice de Democracia Liberal. Esto sugiere que un mayor PIB per cápita está asociado con mayores niveles de democracia liberal. Las implicaciones prácticas podrían ser que el desarrollo económico (medido por el PIB per cápita) puede fomentar un entorno más democrático.





#-------------------2.4 Evaluar el modelo de regresión bivariado MCO-----------------------------------------
# Realizar la regresión lineal
modelo <- lm(v2x_libdem ~ e_gdppc, data = data)

# Obtener los residuos del modelo
residuos <- residuals(modelo)

# Histograma de los residuos
ggplot(data = data.frame(residuos), aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Histograma de los residuos",
       x = "Residuos",
       y = "Densidad")

# Gráfico Q-Q de los residuos
qqnorm(residuos)
qqline(residuos, col = "red", lwd = 2)

# Prueba de Shapiro-Wilk
shapiro.test(residuos)

#Interpretación de los Resultados
#1 - Histograma de los residuos:
#Un histograma con una curva de densidad superpuesta puede darte una idea de la distribución de los residuos. Si los residuos siguen una distribución normal, el histograma debería aproximarse a la forma de una campana.

#2 -Gráfico Q-Q (Quantile-Quantile):
#Un gráfico Q-Q compara los cuantiles de los residuos con los cuantiles de una distribución normal teórica. Si los puntos se alinean aproximadamente en una línea recta, los residuos pueden considerarse normalmente distribuidos.

#3 - Prueba de Shapiro-Wilk:
#La prueba de Shapiro-Wilk proporciona una prueba estadística de normalidad. Si el valor p es mayor que el nivel de significancia (por ejemplo, 0.05), no se puede rechazar la hipótesis nula de que los residuos provienen de una distribución normal.




#-----------------Varianza de residuos constante (homocedasticidad).------------------
install.packages("lmtest")
library(lmtest)

# Realizar la regresión lineal
modelo <- lm(v2x_libdem ~ e_gdppc, data = data)

# Gráfico de dispersión de los residuos vs. los valores ajustados
residuos <- residuals(modelo)
ggplot(data.frame(residuos = residuos, fitted = fitted(modelo)), aes(x = fitted, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Gráfico de Residuos vs. Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos")

# Prueba de Breusch-Pagan para heterocedasticidad
bptest(modelo)

# Prueba de White para heterocedasticidad
white.test(modelo)

#Interpretación de los resultados
#1 - Gráfico de dispersión de los residuos vs. los valores ajustados:
#Si los puntos en el gráfico están distribuidos aleatoriamente alrededor de la línea de 0 en el eje y (línea roja punteada), indica que la varianza de los residuos es constante (homocedasticidad). Si los puntos muestran un patrón sistemático (como un embudo), podría indicar heterocedasticidad.

#2 - Pruebas estadísticas (Breusch-Pagan y White):
#Las pruebas de Breusch-Pagan y White evalúan formalmente la presencia de heterocedasticidad en los residuos del modelo. Un valor p menor que el nivel de significancia (generalmente 0.05) sugiere evidencia de heterocedasticidad, lo que implicaría que la varianza de los errores no es constante.
#Ejemplo de Interpretación con Resultados Hipotéticos
#Supongamos que los resultados de la prueba de Breusch-Pagan son los siguientes:
# Supuestos resultados de la prueba de Breusch-Pagan
#bp_test <- bptest(modelo)
#print(bp_test)
#studentized Breusch-Pagan test
#data:  modelo
#BP = 12.345, df = 1, p-value = 0.0001
#Gráfico de dispersión: Si el gráfico muestra puntos distribuidos aleatoriamente alrededor de la línea de 0 en el eje y, indica homocedasticidad.
#Prueba de Breusch-Pagan: El valor p de 0.0001 es menor que 0.05, lo que sugiere evidencia de heterocedasticidad en los residuos.
#Prueba de White: Similar a la prueba de Breusch-Pagan, evalúa la heterocedasticidad con un enfoque diferente.
#En conclusión, al verificar la homocedasticidad, es importante considerar tanto los gráficos exploratorios como las pruebas estadísticas. Estos métodos te ayudarán a determinar si la varianza de los residuos es constante, lo cual es crucial para la validez de las inferencias realizadas a partir del modelo de regresión lineal.





#--------------------------Especifique que proceso se debería de realizar en caso falta de homocedasticidad, es decir, en caso de tener heterocedasticidad
#Cuando se detecta heterocedasticidad en un modelo de regresión lineal, lo que significa que la varianza de los residuos no es constante a lo largo de los valores ajustados, es importante abordar este problema para asegurar que las inferencias y predicciones del modelo sean válidas. Aquí te indico algunos pasos y técnicas que puedes considerar para manejar la heterocedasticidad:
#1. Transformación de variables
#a. Transformaciones logarítmicas o de raíz cuadrada:
#Si la heterocedasticidad parece estar relacionada con la escala de las variables, considera aplicar transformaciones logarítmicas o de raíz cuadrada a las variables independientes o dependientes.
#b. Transformaciones de Box-Cox:
#La transformación de Box-Cox es una técnica que puede ayudar a estabilizar la varianza y mejorar la normalidad de los datos. Puedes probar diferentes valores de lambda para encontrar la transformación más adecuada.
#2. Uso de modelos alternativos
#a. Modelos de regresión ponderada:
#Los modelos de regresión ponderada asignan pesos diferentes a las observaciones según su varianza, permitiendo así manejar la heterocedasticidad. Puedes utilizar el método weights en R para especificar los pesos adecuados.
#b. Modelos de regresión robustos:
#Los modelos robustos pueden ser menos sensibles a la presencia de heterocedasticidad y valores atípicos en los datos. Ejemplos incluyen la regresión de Mínimos Cuadrados Robustos (MCR) y la regresión de Mínimos Cuadrados Generalizados (GLS).
#3. Corrección mediante transformación de varianza
#a. Transformaciones de varianza:
#Algunos métodos de corrección, como la transformación logarítmica de la variable dependiente o la inclusión de variables adicionales que expliquen la varianza heterocedástica, pueden ser útiles.
#4. Pruebas y diagnósticos adicionales
#a. Pruebas adicionales de heterocedasticidad:
#Además de las pruebas de Breusch-Pagan o White, considera otras pruebas de diagnóstico como la prueba Goldfeld-Quandt.
#b. Análisis de residuos:
#Analiza los residuos para identificar patrones sistemáticos que podrían indicar la necesidad de ajustes adicionales en el modelo.
#Implementación en R
#Aquí tienes un ejemplo básico de cómo podrías implementar algunas de estas técnicas en R:


# Supongamos que 'modelo' es tu modelo de regresión con heterocedasticidad detectada

# 1. Transformación de variables
# Ejemplo de transformación logarítmica
modelo_log <- lm(log(v2x_libdem) ~ log(e_gdppc), data = data)

# 2. Modelos de regresión ponderada
# Ejemplo de regresión ponderada por varianza
pesos <- 1 / residuales^2  # Puedes ajustar cómo calcular los pesos
modelo_ponderado <- lm(v2x_libdem ~ e_gdppc, data = data, weights = pesos)

# 3. Modelos de regresión robustos
install.packages("robustbase")  # Si no está instalado
library(robustbase)

# Ejemplo de regresión robusta MCR
modelo_mcr <- lmrob(v2x_libdem ~ e_gdppc, data = data)

# 4. Pruebas y diagnósticos adicionales
# Prueba Goldfeld-Quandt
gq_test <- gqtest(modelo)
print(gq_test)

# Análisis de residuos
plot(residuals(modelo) ~ fitted(modelo))
abline(h = 0, col = "red", lty = 2)

#-----------------Consideraciones adicionales------------------------
#Es importante seleccionar la técnica apropiada basada en el contexto específico de tus datos y los objetivos del análisis.
#Las transformaciones deben interpretarse adecuadamente en el contexto del problema de investigación.
#Los modelos robustos pueden ser útiles, pero también pueden introducir supuestos adicionales que deben considerarse.
#Al abordar la heterocedasticidad, el objetivo principal es mejorar la especificación del modelo para que los residuos sean homocedásticos, lo que fortalece la validez de las inferencias realizadas a partir del modelo de regresión lineal.



#-------------------Regresión multivariada

#Realizar la regresión entre 𝑌 = v2x_libdem (el Índice de democracia liberal) y todas las variables independientes.
modelo <- lm(v2x_libdem ~ ., data = data)

summary(modelo)
#v2x_libdem es la variable dependiente que estás tratando de predecir.
#El uso de ~ . en la función lm() indica que quieres regresar v2x_libdem en función de todas las demás variables en tu conjunto de datos.
#data es el nombre del data frame que contiene todas tus variables.
#Interpretación del resultado
#El resumen del modelo (summary(modelo)) te proporcionará información detallada sobre la regresión, incluyendo los coeficientes estimados para cada variable independiente, los errores estándar, los valores t y p asociados, así como estadísticas de ajuste como el coeficiente de determinación R2R
#Consideraciones adicionales
#Es importante verificar la calidad del ajuste del modelo utilizando diagnósticos como la prueba de normalidad de los residuos, la homocedasticidad y otros supuestos de regresión.
#Puedes explorar opciones adicionales para mejorar el modelo, como la selección de variables, la transformación de variables, o la inclusión de términos de interacción si es necesario.



# Poner en una tabla los resultados de la regresión junto a los resultados de laprimera regresión.
# Suponiendo que ya tienes los modelos estimados
#Paso 2: Organizar los resultados en una tabla
#Para organizar los resultados en una tabla, puedes utilizar la función stargazer que es útil para presentar los resultados de modelos estadísticos de una manera formateada y lista para exportar o imprimir.
#Instalar y cargar la librería stargazer si aún no está instalada:
install.packages("stargazer")
library(stargazer)
#Crear una tabla con los resúmenes de los modelos:
# Crear una lista de los modelos
modelos <- list(modelo1, modelo2)

# Definir nombres para los modelos
nombres_modelos <- c("Modelo 1", "Modelo 2")

# Generar la tabla con stargazer
stargazer(modelos, type = "text", title = "Comparación de Modelos de Regresión",
          align = TRUE, column.labels = nombres_modelos)
#Este código generará una tabla en formato de texto que incluirá los coeficientes estimados, errores estándar, valores t y p, así como estadísticas de ajuste como R2R2 para ambos modelos. Asegúrate de ajustar los nombres de los modelos y cualquier otra opción de formato según tus necesidades específicas.
#Consideraciones adicionales
#Puedes personalizar la salida de stargazer según tus preferencias, como exportar la tabla a un archivo LaTeX o HTML utilizando type = "latex" o type = "html", respectivamente.
#Si necesitas incluir más detalles o estadísticas específicas en la tabla, revisa la documentación de stargazer para más opciones de personalización.
#Este método te permitirá comparar fácilmente los resultados de ambos modelos de regresión en una tabla organizada y legible.







#Gráficos de diagnóstico para detectar valores influyentes
#1. Gráfico de valores ajustados vs. residuos estandarizados
#Este gráfico te ayuda a identificar observaciones que podrían tener residuos estandarizados (estandarizados por su error estándar) inusualmente grandes, lo que podría indicar valores influyentes.
# Obtener residuos estandarizados
std_residuos <- rstandard(modelo)

# Gráfico de valores ajustados vs. residuos estandarizados
plot(fitted(modelo), std_residuos, main = "Valores Ajustados vs. Residuos Estandarizados",
     xlab = "Valores Ajustados", ylab = "Residuos Estandarizados")
abline(h = 0, col = "red", lty = 2)

#Interpretación: Observaciones que están lejos de la línea horizontal roja (en cero) pueden ser consideradas como valores influyentes. Los puntos que están muy por encima o por debajo de esta línea pueden requerir una revisión adicional para determinar su impacto en el modelo.
#2. Gráfico de leverage vs. residuos cuadrados estandarizados
#Este gráfico combina el leverage (nivel de influencia de una observación en la forma de la curva ajustada) con los residuos cuadrados estandarizados (residuos al cuadrado divididos por su varianza residual), lo cual es útil para identificar observaciones que tienen alto leverage y/o grandes residuos.
# Obtener leverage y residuos cuadrados estandarizados
leverage <- hatvalues(modelo)
std_residuos_cuadrados <- rstudent(modelo)^2

# Gráfico de leverage vs. residuos cuadrados estandarizados
plot(leverage, std_residuos_cuadrados, main = "Leverage vs. Residuos Cuadrados Estandarizados",
     xlab = "Leverage", ylab = "Residuos Cuadrados Estandarizados")
abline(h = 1, col = "red", lty = 2)
#Interpretación: Observaciones que tienen alto leverage (valores en el eje x hacia la derecha) y grandes residuos cuadrados estandarizados (valores en el eje y por encima de la línea roja en 1) pueden ser consideradas como valores influyentes en el modelo.
#Otras consideraciones
#Gráfico de influencia: Además de los gráficos anteriores, también puedes utilizar gráficos de influencia específicos, como influencePlot() del paquete car, que muestra influencias individuales de observaciones sobre los coeficientes de regresión y otros diagnósticos.
# Instalar y cargar el paquete necesario
install.packages("car")
library(car)

# Gráfico de influencia
influencePlot(modelo, id.n = 5, main = "Gráfico de Influencia")
