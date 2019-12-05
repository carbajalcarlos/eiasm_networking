# ===== Initialisation =====
# Loading required libraries
require(quanteda)
require(tidytext)
require(dplyr)
require(ggplot2)

require(tidyr)
#require(topicmodels)



# Loading required datasets
abstracts <- read.csv(file = "1_data/IPDMC_2019_Abstracts.csv", header = TRUE, stringsAsFactors = FALSE)
temp <- colnames(abstracts); temp[1] <- "AUTHOR.1"
colnames(abstracts) <- temp
abstracts$local.id <- 1:nrow(abstracts)
abstracts$ABSTRACT <- try(iconv(abstracts$ABSTRACT, "latin1", "ASCII//TRANSLIT"))

# Generation of tokens table
abstracts.dfm.article <- dfm(x = abstracts$ABSTRACT, tolower = TRUE, groups = abstracts$local.id,
                           stem = FALSE, remove = stopwords("english"),
                           remove_punct = TRUE, remove_numbers = TRUE, 
                           remove_symbols = TRUE, remove_hyphens = TRUE)
abstracts.dfm.author <- dfm(x = abstracts$ABSTRACT, tolower = TRUE, groups = abstracts$AUTHOR.1,
                           stem = FALSE, remove = stopwords("english"),
                           remove_punct = TRUE, remove_numbers = TRUE, 
                           remove_symbols = TRUE, remove_hyphens = TRUE)
abstracts.dfm.track <- dfm(x = abstracts$ABSTRACT, tolower = TRUE, groups = abstracts$X.1,
                           stem = FALSE, remove = stopwords("english"),
                           remove_punct = TRUE, remove_numbers = TRUE, 
                           remove_symbols = TRUE, remove_hyphens = TRUE)

# Tidy objects
abs.td.article <- tidy(abstracts.dfm.article)
abs.td.author <- tidy(abstracts.dfm.author)
abs.td.track <- tidy(abstracts.dfm.track)

# Converting into tf_idf matrix
abs.tf_idf.track <- bind_tf_idf(tbl = abs.td.track, term = term, document = document, n = count)

# Arranging entries by track and tf_idf 
abs.tf_idf.track <- abs.tf_idf.track %>%
  select(-tf) %>%
  arrange(document, desc(tf_idf))

# Plotting unique most used words per track 
abs.tf_idf.track %>%
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term)))) %>% 
  group_by(document) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~document, ncol = 4, scales = "free") +
  coord_flip()

abs.sentiments <- abs.td.track %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))


# Sentriment analysis
abs.sentiments <- abs.sentiments %>%
  arrange(document, desc(sentiment), desc(count))

abs.sentiments$document <- as.factor(abs.sentiments$document)

library(ggplot2)
abs.sen.1 <- subset(abs.sentiments, subset = document == "01. Creativity in New Product Development")

abs.sen.1 %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 3) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

abs.sen.2 <- subset(abs.sentiments, subset = document == "02. Design Issues and Innovation by Design")

abs.sen.2 %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 5) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

# ===== LDA model =====
# Casting tidy file into a matrix
abs.dtm.track <- abs.td.track %>%
  cast_dtm(document, term, count)

abs.dfm.track <- abs.td.track %>%
  cast_dfm(document, term, count)

# Creation of LDA model searching for two topics (k=5)
abs.track.lda <- LDA(abstracts.dfm.track, k = 5, control = list(seed = 359))
abs.track.lda

# Interpretation of the model
require(tidytext)
abs.track.lda.topics <- tidy(abs.track.lda, matrix = "beta")
abs.track.lda.topics

# Plotting resutls
require(ggplot2)
require(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()