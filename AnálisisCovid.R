library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(stopwords)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

# Cargar datos CSV
datos <- read_csv("C:/Users/MI PC/OneDrive/Desktop/COVID19.csv")  

# Filtrar tweets de España
datos_esp <- datos %>%
  filter(country_code == "ES" & !is.na(country_code))

# Definir palabras clave
palabras_ansiedad <- c("ansiedad", "ansiosa", "ansioso", "nervios", "nerviosa", "nervioso", "ataque", "pánico")
palabras_depresion <- c("depresión", "triste", "tristeza", "vacío", "llorar", "llanto", "desesperanza", "infeliz", "abatido", "deprimido", "deprimida")
palabras_clave <- c(palabras_ansiedad, palabras_depresion)

# Filtrar tweets relevantes
tweets_relacionados <- datos_esp %>%
  filter(str_detect(str_to_lower(text), str_c(palabras_clave, collapse = "|"))) %>%
  mutate(fecha = as.Date(created_at))  # Asegúrate de que 'created_at' es tu columna de fecha

# Evolución temporal
frecuencia_diaria <- tweets_relacionados %>%
  count(fecha)

ggplot(frecuencia_diaria, aes(x = fecha, y = n)) +
  geom_line(color = "steelblue") +
  labs(title = "Evolución de tweets sobre ansiedad y depresión", x = "Fecha", y = "Cantidad")

# Nube de palabras
tokens_emocionales <- tweets_relacionados %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords("es")) %>%
  filter(str_detect(word, "[a-z]"))

frecuencias <- tokens_emocionales %>%
  count(word, sort = TRUE)

wordcloud(words = frecuencias$word,
          freq = frecuencias$n,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

nrc <- read.csv("C:/Users/MI PC/Music/DATAFINAL/nrc_espanol.csv", encoding = "UTF-8")

nrc <- nrc %>%
  filter(valor == 1) %>%
  select(palabra, sentimiento)

# Cruzamos los tokens con las emociones del diccionario
sentimientos <- tokens_emocionales %>%
  inner_join(nrc, by = c("word" = "palabra"))

#Contamos las emociones encontradas

conteo_emociones <- sentimientos %>%
  count(sentimiento, sort = TRUE)

print(conteo_emociones)

#Visualizador en gráfico 
library(ggplot2)

ggplot(conteo_emociones, aes(x = reorder(sentimiento, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Emociones detectadas en los tweets relacionados con salud mental",
       x = "Emoción",
       y = "Frecuencia")


