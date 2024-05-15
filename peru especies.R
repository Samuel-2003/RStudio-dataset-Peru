

# Leer el archivo CSV usando el punto y coma como delimitador
Dataset_Derecho_Acuicola290424 <- read_delim("D:/imformes/proyectos de Rstudio/entregable2/Dataset_Derecho_Acuicola290424.csv", 
                                             delim = ";")

# Ver los datos para confirmar que se han leído correctamente
View(Dataset_Derecho_Acuicola290424)


library(dplyr)
library(ggplot2)
library(tidyr)


# Cargar los datos
Dataset_Derecho_Acuicola290424 <- read_delim("D:/informes/proyectos de Rstudio/entregable2/Dataset_Derecho_Acuicola290424.csv", 
                      delim = ";")

# Separar las especies y contar por departamento
dataset_dep <- Dataset_Derecho_Acuicola290424 %>%
  separate_rows(ESPECIE, sep = ", ") %>%
  group_by(DEPARTAMENTO, ESPECIE) %>%
  summarise(n = n(), .groups = "drop")

# Contar la cantidad total de especies únicas por departamento
especies_por_departamento <- dataset_dep %>%
  group_by(DEPARTAMENTO) %>%
  summarise(cantidad_especies = n_distinct(ESPECIE))


ggplot(especies_por_departamento, aes(x = DEPARTAMENTO, y = cantidad_especies, fill = DEPARTAMENTO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cantidad de Especies por Departamento",
       x = "Departamento",
       y = "Cantidad de Especies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclina las etiquetas del eje x para mejor visualización





# Filtrar por el departamento de Cusco y contar especies
cusco_especies <- Dataset_Derecho_Acuicola290424 %>%
  filter(DEPARTAMENTO == "CUSCO") %>%
  separate_rows(ESPECIE, sep = ", ") %>%
  count(ESPECIE) %>%
  arrange(desc(n))  # Opcional, para ordenar las especies por cantidad


ggplot(cusco_especies, aes(x = ESPECIE, y = n, fill = ESPECIE)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cantidad de Especies en el Departamento de Cusco",
       x = "Especies",
       y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotar las etiquetas para mejor visualización

# Usar solo las 10 especies más frecuentes para evitar un gráfico sobrecargado
top_especies_cusco <- cusco_especies %>%
  top_n(10, n)

ggplot(top_especies_cusco, aes(x = reorder(ESPECIE, n), y = n, fill = ESPECIE)) +
  geom_col() +
  coord_flip() +  # Intercambiar los ejes para mejor visualización
  theme_minimal() +
  labs(title = "Top 10 Especies en el Departamento de Cusco",
       x = "Especies",
       y = "Cantidad") +
  theme(legend.position = "none")

#--------------------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
# Cargar el paquete magrittr
library(magrittr)


# Instalar dplyr si aún no está instalado
if (!require(dplyr)) {
  install.packages("dplyr")
}

if (!require(caret)) {
  install.packages("caret")
}

# Cargar el paquete caret
library(caret)


# Cargar el paquete dplyr
library(dplyr)



# Convertir fechas de formato AAAAMMDD
Dataset_Derecho_Acuicola290424 <- Dataset_Derecho_Acuicola290424 %>%
  mutate(
    FECHA_EMISION = ymd(FECHA_EMISION),
    FECHA_VIGENCIA = ymd(FECHA_VIGENCIA),
    FECHA_CORTE = ymd(FECHA_CORTE)
  )

# Ajustar formato de fecha si es necesario (este paso puede no ser necesario)
Dataset_Derecho_Acuicola290424 <- Dataset_Derecho_Acuicola290424 %>%
  mutate(
    FECHA_EMISION = as.character(FECHA_EMISION),
    FECHA_VIGENCIA = as.character(FECHA_VIGENCIA),
    FECHA_CORTE = as.character(FECHA_CORTE),
    FECHA_EMISION = ymd(gsub("(....)(..)(..)", "\\1-\\2-\\3", FECHA_EMISION)),
    FECHA_VIGENCIA = ymd(gsub("(....)(..)(..)", "\\1-\\2-\\3", FECHA_VIGENCIA)),
    FECHA_CORTE = ymd(gsub("(....)(..)(..)", "\\1-\\2-\\3", FECHA_CORTE))
  )

# Resumen de los datos
summary(Dataset_Derecho_Acuicola290424)

# Visualización de la distribución del área por departamento
ggplot(Dataset_Derecho_Acuicola290424, aes(x = DEPARTAMENTO, y = AREA)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tendencias de emisiones por año
Dataset_Derecho_Acuicola290424 %>%
  group_by(AÑO = year(FECHA_EMISION)) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AÑO, y = n)) +
  geom_line()

# Correlación entre variables numéricas
cor(Dataset_Derecho_Acuicola290424 %>% select_if(is.numeric))

# Preparación de datos para modelado, incluyendo conversión de factores
datos_modelo <- Dataset_Derecho_Acuicola290424 %>%
  mutate(DEPARTAMENTO = as.factor(DEPARTAMENTO),
         ESPECIE = as.factor(ESPECIE))

# Partición de datos para entrenamiento y prueba         1
set.seed(123)
train_index <- createDataPartition(datos_modelo$AREA, p = .8, list = FALSE)
train <- datos_modelo[train_index, ]
test <- datos_modelo[-train_index, ]

# Control de entrenamiento para validación cruzada
fit_control <- trainControl(method = "cv", number = 10)

#-------------------------1
library(caret)
library(rpart)

# Verifica los niveles de todas las variables factor en el conjunto de entrenamiento
sapply(train[, sapply(train, is.factor)], function(x) length(unique(x)))

# Eliminar variables con un solo nivel
train <- train[, sapply(train, function(x) !is.factor(x) || length(unique(x)) > 1)]
test <- test[, sapply(test, function(x) !is.factor(x) || length(unique(x)) > 1)]

# Usar createDataPartition con estratificación
fit_control <- trainControl(method = "cv", number = 10, sampling = "up")

# Asegúrate de que el caret entiende que es un problema de clasificación si AREA es un factor
train$AREA <- as.factor(train$AREA)
modelo_cv <- train(AREA ~ ., data = train, method = "lm", trControl = fit_control)

# Revisar la preparación de datos antes de partición
summary(train$DEPARTAMENTO) # Por ejemplo, revisar resumen de variable categórica


# Entrenamiento de modelo lineal  2
modelo_cv <- train(AREA ~ ., data = train, method = "lm", trControl = fit_control)

# Modelo de árbol de decisión
# Cargar la librería necesaria para rpart
library(rpart)

modelo_arbol <- rpart(AREA ~ ., data = train)
printcp(modelo_arbol)  # Visualización de la complejidad del árbol

# Visualizar el árbol
plot(modelo_arbol)
text(modelo_arbol)



# Predicciones y cálculo de error cuadrático medio
predicciones <- predict(modelo_arbol, test, type = "vector")
mse <- mean((predicciones - test$AREA)^2)
print(mse)

# Visualización del árbol de decisión
rpart.plot(modelo_arbol)











