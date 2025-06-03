# TRABAJOFINAL
# Análisis de Salud Mental en Tweets durante COVID-19

Este proyecto analiza el contenido emocional de más de 24.000 tweets publicados en España durante la pandemia del COVID-19. El objetivo es identificar menciones relacionadas con **ansiedad** y **depresión**, y analizar sus emociones con visualizaciones.

---

## PASOS PARA REPRODUCIR EL ANÁLISIS EN R

### 1. Instalar las librerías necesarias
```r
install.packages(c(
  "readr", "dplyr", "tidyr", "stringr",
  "tidytext", "stopwords", "ggplot2",
  "wordcloud", "RColorBrewer"
))
```
### **Archivos necesarios**
COVID19.csv → dataset de tweets
nrc_espanol.csv → diccionario de emociones NRC en español

### **Cargar librerías y datatest**
``` {r}
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(stopwords)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

datos <- read_csv(""C:/Users/MI PC/OneDrive/Desktop/COVID19.csv"")
```
# Filtrar solo tweets de España
``` {r}
datos_esp <- datos %>%
  filter(country_code == "ES" & !is.na(country_code))
```

# Definir palabras clave
``` {r}
palabras_ansiedad <- c("ansiedad", "ansiosa", "ansioso", "nervios", "nerviosa", "nervioso", "ataque", "pánico")
palabras_depresion <- c("depresión", "triste", "tristeza", "vacío", "llorar", "llanto", "desesperanza", "infeliz", "abatido", "deprimido", "deprimida")
palabras_clave <- c(palabras_ansiedad, palabras_depresion)
```
# Filtrar tweets que contienen esas palabras
``` {r}
tweets_relacionados <- datos_esp %>%
  filter(str_detect(str_to_lower(text), str_c(palabras_clave, collapse = "|"))) %>%
  mutate(fecha = as.Date(created_at))
```
## Visualización temporal

```{r}
frecuencia_diaria <- tweets_relacionados %>%
  count(fecha)
```
### Cuenta cuántos tweets relacionados con salud mental se publicaron cada día.
```{r}
ggplot(frecuencia_diaria, aes(x = fecha, y = n)) +
  geom_line(color = "steelblue") +
  labs(title = "Evolución diaria de tweets sobre ansiedad y depresión",
       x = "Fecha", y = "Cantidad de tweets")
```
###Muestra un gráfico de línea con la evolución de esos tweets a lo largo del tiempo.

# Nube de palabras
```{r}
tokens_emocionales <- tweets_relacionados %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords("es")) %>%
  filter(str_detect(word, "[a-z]"))
```
###Separa los tweets en palabras individuales (tokens), elimina stopwords y caracteres no alfabéticos.
```{r}
frecuencias <- tokens_emocionales %>%
  count(word, sort = TRUE)
###Cuenta cuántas veces aparece cada palabra.
wordcloud(words = frecuencias$word,
          freq = frecuencias$n,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
```
### Genera una nube de palabras donde el tamaño refleja la frecuencia.
# **Análisis de emociones (con NRC)**
```{r}
nrc <- read.csv("C:/TU/RUTA/nrc_espanol.csv", encoding = "UTF-8") %>%
  filter(valor == 1) %>%
  select(palabra, sentimiento)
```
###Carga el diccionario de emociones NRC y selecciona las palabras activas.
```{r}
sentimientos <- tokens_emocionales %>%
  inner_join(nrc, by = c("word" = "palabra"))
```
###Relaciona las palabras de los tweets con emociones específicas.
```{r}
conteo_emociones <- sentimientos %>%
  count(sentimiento, sort = TRUE)
```
###Cuenta cuántas veces aparece cada emoción.
```{r}
ggplot(conteo_emociones, aes(x = reorder(sentimiento, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Emociones detectadas en los tweets relacionados con salud mental",
       x = "Emoción", y = "Frecuencia")
```
###Crea un gráfico de barras con las emociones predominantes detectadas (como tristeza, miedo, ira).
