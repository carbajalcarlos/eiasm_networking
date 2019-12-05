# ===== Initialisation =====
# Loading required libraries
require(quanteda)
require(tidytext)
require(dplyr)
require(ggplot2)

require(tidyr)
require(topicmodels)



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

