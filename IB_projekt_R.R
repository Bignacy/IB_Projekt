
#' ---
#' title: "Analiza albumów The Strokes ze względu na teksty piosenek"
#' author: "Ignacy Brzoza"
#' date: "Czerwiec 2025"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: kate # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: hide    # Kod domyślnie zwinięty
#'     number_sections: false # Numeruje nagłówki
#'     css: "custom.css"     # Możliwość stworzenia własnego stylowania
#' ---

library(tm)                # Przetwarzanie tekstu
library(tidyverse)         # Wizualizacja danych
library(tidytext)          # Obróbka tekstu
library(ggplot2)           # Wykresy
library(ggthemes)          # Motywy graficzne
library(wordcloud)         # Chmury słów
library(RColorBrewer)      # Kolory
library(SentimentAnalysis) # Analiza sentymentu
library(SnowballC)         # Stemming
library(cluster)           # Klastrowanie
library(factoextra)        # Wizualizacje klastrów
library(dplyr)             # Przetwarzanie danych
library(ggrepel)           # Dodawania etykiet w wykresach
library(DT)                # Interaktywne tabele

#' # Funkcja do przetwarzania tekstu ----
# Funkcja do przetwarzania tekstu ----
process_text <- function(file_path) {
  # Wczytanie tekstu z pliku
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  # Usunięcie znaków interpunkcyjnych i cyfr oraz wybranych stop słów
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  custom_stopwords <- c("—", "–", "’s", "’re", "yeah", "verse", "chorus", "prechorus", "hey",
                        "oh", "just", "youre", "dont",
                        "postchorus", "bridge", "guitar", "solo", "instrumental", "break", "im")
  
  # Usunięcie stop słów angielskich
  all_stopwords <- c(stopwords("en"), custom_stopwords)
  text <- removeWords(text, all_stopwords)
  # Podział tekstu na słowa
  words <- unlist(strsplit(text, "\\s+"))
  # Usunięcie pustych elementów
  words <- words[words != ""]
  
  return(words)
}

# Funkcja do obliczania częstości występowania słów ----
word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}

# Funkcja do tworzenia chmury słów ----
plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 8,
            colors = brewer.pal(8, color_palette))
  title(title)
}

# Lista plików do wczytania
file_paths <- c("Is_This_It.txt", "Room_On_Fire.txt", "First_Impressions_Of_Earth.txt", "Angles.txt", "Comedown_Machine.txt", "The_New_Abnormal.txt")

custom_stopwords <- c("—", "–", "’s", "’re", "yeah", "verse", "chorus", "prechorus", "hey",
                      "oh", "just", "youre", "dont",
                      "postchorus", "bridge", "guitar", "solo", "instrumental", "break", "im")

# Przetwarzanie każdego pliku osobno
for (file_path in file_paths) {
  words <- process_text(file_path)
  
  # Usunięcie dodatkowych stop słów z przetworzonego tekstu
  words <- words[!words %in% custom_stopwords]
  
  # Obliczenie częstości występowania słów
  freq_df <- word_frequency(words)

  #' # Tworzenie chmury słów  
  # Tworzenie chmury słów
  plot_wordcloud(freq_df, paste("Chmura słów -", file_path), "Dark2")
  
  # Wyświetlenie 10 najczęściej występujących słów
  cat("Najczęściej występujące słowa w pliku", file_path, ":\n")
  print(head(freq_df, 10))
  cat("\n")
}

# Wczytanie słowników z plików csv ----
afinn <- read.csv("afinn.csv", stringsAsFactors = FALSE)
bing <- read.csv("bing.csv", stringsAsFactors = FALSE)
nrc <- read.csv("nrc.csv", stringsAsFactors = FALSE)

# Wczytanie danych tekstowych
text <- readLines("All_Albums.txt", encoding="UTF-8")

docs <- VCorpus(VectorSource(text))
tdm <- TermDocumentMatrix(docs)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)

tokeny <- data.frame(Review = names(v), freq = v, stringsAsFactors = F)
tokeny_data <- as_tibble(tokeny)

#' # Przed analizą sentymentu ----
# Przed analizą sentymentu ----

#' # Tokenizacja tekstu
# Tokenizacja tekstu

# M.in. usunięcie interpunkcji, zamiana tekstu na małe litery oraz usunięcie stop słów

tidy_tokeny <- tokeny_data %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words)

head(tidy_tokeny, 10)

#' # Analiza sentymentu ----
# Analiza sentymentu ----

# Analiza sentymentu przy użyciu słownika NRC ----

# Zliczanie sentymentu
sentiment_review_nrc <- tidy_tokeny %>%
  inner_join(nrc, relationship = "many-to-many")

sentiment_review_nrc %>%
  count(sentiment)

# Zliczanie najczęstszych słów dla danego sentymentu
sentiment_review_nrc %>%
  count(word, sentiment) %>%
  arrange(desc(n))

# Pozostawienie tylko słów o znaczeniu  pozytywnym lub negatywnym
sentiment_review_nrc2 <- sentiment_review_nrc %>%
  filter(sentiment %in% c("positive", "negative"))

word_counts_nrc2 <- sentiment_review_nrc2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

#' # Wizualizacja sentymentu
# Wizualizacja sentymentu
ggplot(word_counts_nrc2, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (NRC)")

# Analiza sentymentu przy użyciu słownika Bing ----

# Zliczanie sentymentu
sentiment_review_bing <- tidy_tokeny %>%
  inner_join(bing)

sentiment_review_bing %>%
  count(sentiment)

# Zliczanie najczęstszych słów dla danego sentymentu
sentiment_review_bing %>%
  count(word, sentiment) %>%
  arrange(desc(n))

# Pozostawienie tylko słów o znaczeniu  pozytywnym lub negatywnym

sentiment_review_bing2 <- sentiment_review_bing %>%
  filter(sentiment %in% c("positive", "negative"))

word_counts_bing2 <- sentiment_review_bing2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

# Wizualizacja sentymentu
ggplot(word_counts_bing2, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (Bing)") +
  scale_fill_manual(values = c("dodgerblue4", "goldenrod1"))

# Analiza sentymentu przy użyciu słownika Afinn ----

# Zliczanie sentymentu
sentiment_review_afinn <- tidy_tokeny %>%
  inner_join(afinn)

sentiment_review_afinn %>%
  count(value)

# Zliczanie najczęstszych słów dla danego sentymentu
sentiment_review_afinn %>%
  count(word, value) %>%
  arrange(desc(n))

# Pozostawienie tylko słów o wartości w zakresie od -5 do 5 w zakresie negatywne do pozytywnych

sentiment_review_afinn3 <- sentiment_review_afinn %>%
  filter(value %in% c("3", "-3" , "4", "-4", "5", "-5"))

word_counts_afinn3 <- sentiment_review_afinn3 %>%
  count(word, value) %>%
  group_by(value) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

#' # Wizualizacja sentymentu
# Wizualizacja sentymentu
ggplot(word_counts_afinn3, aes(x=word2, y=n, fill=value)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~value, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (AFINN)")

# Analiza sentymentu przy użyciu pakietu SentimentAnalysis ----
sentiment <- analyzeSentiment(text)

# Słownik GI (General Inquirer) ----

# Wczytanie słownika GI
data(DictionaryGI)
summary(DictionaryGI)

# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem GI
sentimentGI <- convertToDirection(sentiment$SentimentGI)

# Wykres skumulowanego sentymentu kierunkowego
plot(sentimentGI)

# Wykres skumulowanego sentymentu kierunkowego w ggplot2
df_GI <- data.frame(index = seq_along(sentimentGI), value = sentimentGI, Dictionary = "GI")

# Usunięcie ewentualnych wierszy, które zawierają NA
df_GI <- na.omit(df_GI)

ggplot(df_GI, aes(x = value)) +
  geom_bar(fill = "green", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (GI)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()

# Słownik QDAP (Quantitative Discourse Analysis Package) ----

# Wczytanie słownika QDAP
qdap <- loadDictionaryQDAP()
summary(qdap)

# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem QDAP
sentimentQDAP <- convertToDirection(sentiment$SentimentQDAP)

# Wykres skumulowanego sentymentu kierunkowego
plot(sentimentQDAP)

# Wykres skumulowanego sentymentu kierunkowego w ggplot2
df_QDAP <- data.frame(index = seq_along(sentimentQDAP), value = sentimentQDAP, Dictionary = "QDAP")

# Usunięcie wierszy, które zawierają NA
df_QDAP <- na.omit(df_QDAP)

ggplot(df_QDAP, aes(x = value)) +
  geom_bar(fill = "red", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (QDAP)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()

#' # Porównanie sentymentu na podstawie różnych słowników ----
# Porównanie sentymentu na podstawie różnych słowników ----

# Połączenie poszczególnych ramek w jedną ramkę
df_all <- bind_rows(df_GI, df_QDAP)

# Tworzenie wykresu z podziałem na słowniki
ggplot(df_all, aes(x = value, fill = Dictionary)) +
  geom_bar(alpha = 0.7) + 
  labs(title = "Skumulowany sentyment według słowników",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw() +
  facet_wrap(~Dictionary) + 
  scale_fill_manual(values = c("GI" = "green", 
                               "LM" = "orange",
                               "QDAP" = "red" ))


#' # Agregowanie sentymentu z różnych słowników w czasie ----
# Agregowanie sentymentu z różnych słowników w czasie ----

# Sprawdzenie ilości obserwacji
length(sentiment[,1])

# Ramki danych
df_all <- data.frame(sentence=1:length(sentiment[,1]),
                     GI=sentiment$SentimentGI,
                     LM=sentiment$SentimentLM,
                     QDAP=sentiment$SentimentQDAP)

# Usunięcie wartości NA
puste <- df_all[!complete.cases(df_all), ]

# Usunięcie pustych obserwacji
df_all <- df_all[!is.na(df_all$QDAP), ]

#' # Wykresy przedstawiające ewolucję sentymentu w czasie ----
# Wykresy przedstawiające ewolucję sentymentu w czasie ----

ggplot(df_all, aes(x=sentence, y=QDAP)) +
  geom_line(color="red", size=1) +
  geom_line(aes(x=sentence, y=GI), color="green", size=1) +
  geom_line(aes(x=sentence, y=LM), color="orange", size=1) +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")

ggplot(df_all, aes(x=sentence, y=QDAP)) + 
  geom_smooth(color="red") +
  geom_smooth(aes(x=sentence, y=GI), color="green") +
  geom_smooth(aes(x=sentence, y=LM), color="orange") +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")

#' # Dane tekstowe ----
# Dane tekstowe ----

docs <- DirSource("textfolder")

# Utwórzenie korpusu dokumentów tekstowych
corpus <- VCorpus(docs)

# Korpus
inspect(corpus)

# Przetwarzanie i oczyszczanie tekstu ----

# Normalizacja i usunięcie zbędnych znaków ----

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# Usunięcie zbędnych słów
corpus <- tm_map(corpus, removeWords, c("—", "–", "’s", "’re", "yeah", "verse", "chorus", "prechorus", "hey",
                                        "oh", "just", "youre", "dont",
                                        "postchorus", "bridge", "guitar", "solo", "instrumental", "break", "im"))
corpus <- tm_map(corpus, stripWhitespace)

#' # Stemming ----
# Stemming ----

# Kopia korpusu
corpus_copy <- corpus

# Wykonanie stemmingu w korpusie
corpus_stemmed <- tm_map(corpus, stemDocument)

# Uzupełnienie rdzeni słów po stemmingu ----

complete_stems <- content_transformer(function(x, dict) {
  x <- unlist(strsplit(x, " "))                
  x <- stemCompletion(x, dictionary = corpus_copy, type="longest") 
  paste(x, collapse = " ")                       
})

# StemCompletion do każdego dokumentu w korpusie
corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)

# usunięcie NA
corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
corpus_completed <- tm_map(corpus_completed, stripWhitespace)

#' # Tokenizacja ----
# Tokenizacja ----

# Macierze częstości TDM i DTM ----

# Funkcja TermDocumentMatrix() ----
# tokeny = wiersze, dokumenty = kolumny
tdm <- TermDocumentMatrix(corpus_completed)
tdm
inspect(tdm)

tdm_m <- as.matrix(tdm)

tdm_m[1:5, 1:5]

# Funkcja DocumentTermMatrix() ----
# dokumenty = wiersze, tokeny = kolumny
dtm <- DocumentTermMatrix(corpus_completed)
dtm
inspect(dtm)

dtm_m <- as.matrix(dtm)

dtm_m[1:5, 1:5]

#' # Zliczanie częstości słów ----
# Zliczanie częstości słów ----

v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

v2 <- sort(colSums(dtm_m), decreasing = TRUE)
dtm_df <- data.frame(word = names(v2), freq = v2)
head(dtm_df, 10)

#' # Eksploracyjna analiza danych ----
# Eksploracyjna analiza danych ----

# Globalna chmura słów
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 15, 
          colors = brewer.pal(8, "Dark2"))

# Wyświetlenie top 10
print(head(tdm_df, 10))

# Inżynieria cech w modelu Bag of Words: ----
# Reprezentacja słów i dokumentów w przestrzeni wektorowej ----

dtm
inspect(dtm)
dtm_m[1:5, 1:5]

#' # UCZENIE MASZYNOWE NIENADZOROWANE ----
# UCZENIE MASZYNOWE NIENADZOROWANE ----

#' # Klastrowanie k-średnich (k-means) ----
# Klastrowanie k-średnich (k-means) ----

# Dobór liczby klastrów metodą sylwetki
fviz_nbclust(t(dtm_m), kmeans, method = "silhouette") +
  labs(title = "Dobór liczby klastrów", subtitle = "Metoda sylwetki")

set.seed(123) # ziarno losowe dla replikacji wyników

# Ustawienie liczby klastrów k = 2 ----
k <- 2

klastrowanie <- kmeans(dtm_m, centers = k)

# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")

# Table z przypisaniem dokumentów i top 5 słów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_słów = top_words,
    stringsAsFactors = FALSE
  )
})

# Ramka danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentów z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dołączenie danych z podsumowania
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Tabela z pełnym podsumowaniem

datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczęstsze słowa i liczność klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))

# Chmury słów dla każdego klastra
for (i in 1:k) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 20, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura słów - Klaster", i))
}

# Przypisanie dokumentów do klastrów ----
document_names <- names(corpus)  
clusters <- klastrowanie$cluster  

# Ramka danych
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

print(documents_clusters)


# Wizualizacja przypisania dokumentów do klastrów ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentów do klastrów",
       x = "Dokument",
       y = "Liczba wystąpień (powinna wynosić 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)

# Ustawienie liczby klastrów k = 3 ----
k <- 3

klastrowanie <- kmeans(dtm_m, centers = k)

# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")


# Tabela z przypisaniem dokumentów i top 5 słów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_słów = top_words,
    stringsAsFactors = FALSE
  )
})

# Ramka danych
cluster_info_df <- do.call(rbind, cluster_info)

document_names <- names(corpus)

# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dołączamy dane z podsumowania
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Tabela z pełnym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczęstsze słowa i liczność klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))

# Chmury słów dla każdego klastra
for (i in 1:k) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 20, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura słów - Klaster", i))
}

# Przypisanie dokumentów do klastrów ----
document_names <- names(corpus)  
clusters <- klastrowanie$cluster 

# Ramka danych
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgląd
print(documents_clusters)

# Wizualizacja przypisania dokumentów do klastrów ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentów do klastrów",
       x = "Dokument",
       y = "Liczba wystąpień (powinna wynosić 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)

# Ustawienie liczby klastrów k = 4 ----
k <- 4

klastrowanie <- kmeans(dtm_m, centers = k)

# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")

# Tabela z przypisaniem dokumentów i top 5 słów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_słów = top_words,
    stringsAsFactors = FALSE
  )
})

# Ramka danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentów z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Dołączenie dane z podsumowania 
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Tabela z pełnym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczęstsze słowa i liczność klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))

# Chmury słów dla każdego klastra
for (i in 1:k) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 20, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura słów - Klaster", i))
}

# Przypisanie dokumentów do klastrów ----
document_names <- names(corpus) 
clusters <- klastrowanie$cluster

# Ramka danych
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgląd
print(documents_clusters)

# Wizualizacja przypisania dokumentów do klastrów ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentów do klastrów",
       x = "Dokument",
       y = "Liczba wystąpień (powinna wynosić 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)

