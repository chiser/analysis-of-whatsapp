
chat_file <- "../ChatWhatsApp.txt"

library("rwhatsapp")
chat <- rwa_read(chat_file)
chat

## Remove messages without author or weird author
library("dplyr")
chat <- chat %>% 
  filter(!is.na(author)) # remove messages without author
chat <- chat %>% 
  filter(author != "13 - Sonia Rohrsen")
chat

## Messaging over time
library("ggplot2"); theme_set(theme_minimal())
library("lubridate")
chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")

## Amount of messages for each user
chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages")

## Amount of multimedia sent from each user
chat %>%
  mutate(day = date(time)) %>%
  filter(text == "<Multimedia omitido>") %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of multimedia shared")


## Emojis
library("tidyr")
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis")


## More emojis
library("ggimage")
emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))

chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used emojis") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## Often words
library("tidytext")
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")

library("stopwords")
to_remove <- c(stopwords( "es"),
               stopwords( "en"),
               stopwords( "de"),
               "multimedia",
               "omitido",
               "k",
               "x",
               "si",
               "d",
               "ok",
               "cn",
               "x",
               "u",
               "q",
               "xk",
               "llamada perdida",
               "luisa",
               "sobrina",
               "mama",
               "sonia",
               "kaj",
               "hermana",
               "rohrsen",
               "christian",
               "lulu",
               "ruth",
               "selina",
               "anja",
               "<U+07E1>",
               "<U+0C8E>",
               "<U+0C90>",
               "<U+3050>",
               "<U+0C8F>",
               "<U+0C8E>",
               "<U+0C93>",
               "alemania"
)#Define all the words which are not required

chat$text<-gsub("[^\x01-\x7F]", "", chat$text)
chat$text <- tolower(chat$text) #Turn the data into lower case
chat$text <- removeWords(chat$text, to_remove)
chat$text <- removePunctuation(chat$text)
chat$text <- removeNumbers(chat$text)

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 10, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 5, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")


## with td-idf
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  mutate(word = gsub(".com", "", word)) %>%
  mutate(word = gsub("^gag", "9gag", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 10) %>%
  group_by(author) %>%
  top_n(n = 6, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 5, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Important words using tf–idf by author")

## lexical diversity

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("unique words") +
  xlab("") +
  ggtitle("Lexical Diversity") +
  coord_flip()

## Check lexicon by user
quien<-"Chis"
quien<-"Sonia Rohrsen"

o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != quien) %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == quien) %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 20, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle(paste0("Unique words of ",quien))

for (i in unique(chat$author)){
  
  quien<-i
  
  o_words <- chat %>%
    unnest_tokens(input = text,
                  output = word) %>%
    filter(author != quien) %>% 
    count(word, sort = TRUE) 
  
  print(
  chat %>%
    unnest_tokens(input = text,
                  output = word) %>%
    filter(author == quien) %>% 
    count(word, sort = TRUE) %>% 
    filter(!word %in% o_words$word) %>% # only select words nobody else uses
    top_n(n = 20, n) %>%
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_col(show.legend = FALSE) +
    ylab("") + xlab("") +
    coord_flip() +
    ggtitle(paste0("Unique words of ",quien))
  )
  
}

##########################################################################################################
######################   Sentiment analysis                 #########################################
#####################################################################################################

# Read and load text file in R

# library(readtext) #Load Required package >setwd("/Users/Desktop/RDirectory")
# TextData <- readtext(chat_file)
# TextData <- as.data.frame(TextData)

# Remove punctuation, Numbers, special characters and other unwanted things and stem all the words.


library(tm)
mystopwords <- c("", "","","", "", "", stopwords("en")) #Define all the words which are not required

CleanData <- tolower(chat$text) #Turn the data into lower case
CleanData <- removeWords(CleanData, mystopwords)
CleanData <- removePunctuation(CleanData)
CleanData <- removeNumbers(CleanData)
# CleanData <- stemmer(CleanData, rm.bracket = TRUE)

# Make a word-cloud with according to the frequency of the word used

library(wordcloud)
# library(qdap)

# TextFrequency <- freq_terms(CleanData, at.least = 1)

freq_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>% 
  count(word, sort = TRUE) 

wordcloud(freq_words$word, freq_words$n, colors = freq_words$n, max.words = 200)

# Sentiment Analysis: Calls the NRC sentiment dictionary to calculate the presence of eight different emotions and their corresponding valence in a text file.
library(syuzhet)

Sentiments <- get_nrc_sentiment(o_words$word)

Sentiments <- cbind("Words" = o_words$word, Sentiments)

SentimentsScore <- data.frame("Score" = colSums(Sentiments[2:11]))

TotalSentiments <- cbind("Sentiments" = rownames(SentimentsScore), SentimentsScore)

rownames(TotalSentiments) <- NULL

# Visualisation of the sentiments extracted from the texts

library(ggplot2)
ggplot(data = TotalSentiments, aes(x = Sentiments, y = Score)) + geom_bar(stat = "identity", aes(fill = Sentiments))

################# En espaniol #######################
# Para este análisis de sentimiento usaremos el léxico Afinn. Este es un conjunto de palabras, puntuadas de acuerdo a qué tan 
# positivamente o negativamente son percibidas. Las palabras que son percibidas de manera positiva tienen puntuaciones de -4 
# a -1; y las positivas de 1 a 4.
# La versión que usaremos es una traducción automática, de inglés a español, de la versión del léxico presente en el conjunto 
# de datos sentiments de tidytext, con algunas correcciones manuales. Por supuesto, esto quiere decir que este léxico tendrá 
# algunos defectos, pero será suficiente para nuestro análisis.
# Descargamos este léxico de la siguiente dirección:
# download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
#               "lexico_afinn.en.es.csv")
# De nuevo usamos la función read.csv() para importar los datos.

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

chat_afinn <- 
  chat %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 

# Obtenemos también una puntuación por tuit, usando group_by() y summarise() de dplyr, y la agregamos chat para usarla 
# después. Tambien asignamos a los chat sin puntuación positiva o negativa un valor de 0, que indica neutralidad. 

chat2 <-
  chat_afinn %>%
  group_by(author) %>%
  summarise(Puntuacion_chat = mean(Puntuacion)) %>%
  left_join(chat, ., by = "author") %>% 
  mutate(Puntuacion_chat = ifelse(is.na(Puntuacion_chat), 0, Puntuacion_chat))


# Explorando los datos, medias por día
# Empecemos revisando cuántas palabras en total y cuantas palabras únicas ha usado cada autor
# Total de palabras utilizadas reconocidas por la lista de sentimiento
# Únicas
chat_afinn %>% 
  group_by(author) %>% 
  distinct(Palabra) %>% 
  count()

# Y veamos también las palabras positivas y negativas más usadas por cada uno de ellos.
library(purrr)

map(c("Positiva", "Negativa"), function(sentimiento) {
  chat_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(author) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = author) +
    geom_col() +
    facet_wrap("author", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento)
})

# Como deseamos observar tendencias, vamos a obtener la media de sentimientos por día de cada autor
# format(chat_afinn$time,format='%Y%m%d')

chat_afinn_fecha <-
  chat_afinn %>%
  group_by(author) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(author, time) %>%
  summarise(Media = mean(Puntuacion))

chat_afinn_fecha %>%
  ggplot() +
  aes(time, Media, color = author) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(author~.) +
  theme(legend.position = "none")

chat_afinn_fecha %>%
  ggplot() +
  aes(time, Media, color = author) +
  geom_smooth(method = "loess", fill = NA) 

chat_afinn %>%
  ggplot() +
  aes(time, Puntuacion, color = author) +
  geom_smooth(method = "loess", fill = NA)

library(scales)

chat_afinn %>%
  count(author, Tipo) %>%
  group_by(author) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(author, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  theme(legend.position = "top")

chat_afinn %>%
  group_by(author, time) %>%
  count(Tipo) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Fecha, Proporcion, fill = Tipo) +
  geom_col(width = 1) +
  facet_grid(author~.) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(expand = c(0, 0)) +
  theme(legend.position = "top")

chat2 %>%
  ggplot() +
  aes(Puntuacion_chat, color = author) +
  geom_density() +
  facet_wrap(~author) 


####################### Tomado de analisis de uber

library(tm)
# chat$text <- tm_map(chat$text, removeWords, stopwords(kind = "es"))
# Warning in tm_map.SimpleCorpus(uber_corpus_clean, removeWords,
# stopwords(kind = "es")): transformation drops documents
chat_txt <- iconv(chat$text, 'UTF-8', 'ASCII')
chat_corpus <- Corpus(VectorSource(chat_txt))
inspect(chat_corpus[1:10])
test <- tm_map(chat_corpus, stemDocument)
# Warning in tm_map.SimpleCorpus(uber_corpus_clean, stemDocument):
# transformation drops documents
test <- tm_map(test, stemCompletion, dictionary = test)

# library(Rstem)
# remotes::install_github("abhy/sentiment")
# library(sentiment)
chat_class_emo <- classify_emotion(test, algorithm="bayes", prior=1.0)
emotion <- uber_class_emo[, 7]
emotion[is.na(emotion)] <- "unknown"
table(emotion, useNA = "ifany")
library(ggplot2)
library(gridExtra)
uber_pie <- ggplot(as.data.frame(uber_class_emo), aes(x = factor(1), fill = factor(BEST_FIT))) + geom_bar(width = 1)

uber_pie + coord_polar(theta = "y") + ggtitle("Sentimiento Uber en España", subtitle = "Datos España") + ylab("Y") + xlab("X") + theme(plot.title = element_text(size=12, face='bold'))

library(ggplot2)
library(gridExtra)
uber_pie <- ggplot(as.data.frame(uber_class_emo), aes(x = factor(1), fill = factor(BEST_FIT)))  + geom_bar(width = 1)

uber_pie + coord_polar(theta = "y") + ggtitle("Sentimiento Uber en España", subtitle = "Datos España") + ylab("Y") + xlab("X") + scale_fill_brewer(palette = "RdYlGn") + theme(plot.title = element_text(size=12, face='bold'))

uber_class_pol <- classify_polarity(uber_txt, algorithm="bayes")

polarity <- uber_class_pol[, 4]

uber_sentiment_dataframe <- data.frame(text=uber_txt, emotion=emotion, 
                                       polarity=polarity, stringsAsFactors=FALSE)

ggplot(uber_sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Análisis de sentimiento de Uber en Twitter", subtitle = "Datos Uber España") +
  theme(legend.position="right", plot.title = element_text(size=12, face='bold')) + ylab("Número de Tweets") + xlab("Tipos de emoción")

ggplot(uber_sentiment_dataframe, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdYlBu") +
  ggtitle("Análisis de sentimiento Uber en Twitter", subtitle = "Datos España") +
  theme(legend.position="bottom", plot.title = element_text(size=12, face='bold')) + ylab("Número de Tweets") + xlab("Tipos de polaridad")

uber_sentiment_df = data.frame(text=uber_txt, emotion=emotion,
                               polarity=polarity, stringsAsFactors=FALSE)

# Separamos el texto segun las emociones
emotion_uber = levels(factor(uber_sentiment_df$emotion))
emotion_length = length(emotion_uber)
emotion_uber.docs = rep('', emotion_length)
for (i in 1: emotion_length) 
{
  tmp = uber_txt[emotion == emotion_uber[i]]
  emotion_uber.docs[i] = paste(tmp, collapse=' ')
}

# Eliminamos las Stopwords
emotion_uber.docs = removeWords(emotion_uber.docs, stopwords('es'))

# Creamos el corpus
corpus = Corpus(VectorSource(emotion_uber.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emotion_uber

# Dibujamos una Comparison Wordcloud
comparison.cloud(tdm, colors = brewer.pal(emotion_length, 'Dark2'),
                 scale = c(1,.2), max.words = 400,random.order = FALSE, title.size = 1.5)