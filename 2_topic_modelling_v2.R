# ===== Initialisation =====
# Loading required libraries
require(quanteda)      # Construction of Document-Term Matrix 
require(topicmodels)   # Fitting LDA model
require(tidytext)      # Tiding LDA topics

# Loading required datasets
abstracts <- read.csv(file = "1_data/IPDMC_2019_Abstracts.csv", header = TRUE, stringsAsFactors = FALSE)

# ===== Generation of LDA model ===== 
# Cleaing input dataset
temp <- colnames(abstracts); temp[1] <- "AUTHOR.1"
colnames(abstracts) <- temp
abstracts$local.id <- 1:nrow(abstracts)
abstracts$ABSTRACT <- try(iconv(abstracts$ABSTRACT, "latin1", "ASCII//TRANSLIT"))

# Generating to Document Feature Matrix
abs.dfm.article <- dfm(x = abstracts$ABSTRACT, tolower = TRUE, groups = abstracts$local.id,
                       stem = FALSE, remove = stopwords("english"),
                       remove_punct = TRUE, remove_numbers = TRUE, 
                       remove_symbols = TRUE, remove_hyphens = TRUE)
abs.dfm.author <- dfm(x = abstracts$ABSTRACT, tolower = TRUE, groups = abstracts$AUTHOR.1,
                      stem = FALSE, remove = stopwords("english"),
                      remove_punct = TRUE, remove_numbers = TRUE, 
                      remove_symbols = TRUE, remove_hyphens = TRUE)
abs.dfm.track <- dfm(x = abstracts$ABSTRACT, tolower = TRUE, groups = abstracts$X.1,
                     stem = FALSE, remove = stopwords("english"),
                     remove_punct = TRUE, remove_numbers = TRUE, 
                     remove_symbols = TRUE, remove_hyphens = TRUE)

# Generating to Document-Term Matrix
abs.td.track <- tidy(abs.dfm.track)
abs.dtm.track <- abs.td.track %>%
  cast_dtm(document, term, count)

# Fitting LDA model
abs.track.lda <- LDA(abs.dtm.track, k = 3, control = list(seed = 359))
#abs.track.lda

# Tiding LDA topics
abs.track.topics <- tidy(abs.track.lda, matrix = "beta")
#abs.track.topics

# ===== Interpretation of LDA topics =====
# Loading required libraries 
require(ggplot2)
require(dplyr)

# Substracting the top 10 terms for each topic
top.terms <- abs.track.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Contrast spread
library(tidyr)

beta.spread <- abs.track.topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))


abs.track <- tidy(abs.track.lda, matrix = "gamma")
abs.track

top.tracks <- abs.track %>%
  group_by(topic) %>%
  top_n(10, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

top.tracks %>%
  mutate(document = reorder_within(document, gamma, topic)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
