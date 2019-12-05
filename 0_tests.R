# Loading required libraries
require(topicmodels)

data("AssociatedPress")
class(AssociatedPress)

# Creation of LDA model searching for two topics (k=2)
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 359))
ap_lda

# Interpretation of the model
require(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

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

# Visualisation of topic contrast
require(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

# topic probabilities
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

