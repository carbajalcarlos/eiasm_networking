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

# Convertion to Document-Term Matrix
abs.dtm.article <- dfm(x = abstracts$ABSTRACT, tolower = TRUE, groups = abstracts$local.id,
                       stem = FALSE, remove = stopwords("english"),
                       remove_punct = TRUE, remove_numbers = TRUE, 
                       remove_symbols = TRUE, remove_hyphens = TRUE)
abs.dtm.author <- dfm(x = abstracts$ABSTRACT, tolower = TRUE, groups = abstracts$AUTHOR.1,
                      stem = FALSE, remove = stopwords("english"),
                      remove_punct = TRUE, remove_numbers = TRUE, 
                      remove_symbols = TRUE, remove_hyphens = TRUE)
abs.dtm.track <- dfm(x = abstracts$ABSTRACT, tolower = TRUE, groups = abstracts$X.1,
                     stem = FALSE, remove = stopwords("english"),
                     remove_punct = TRUE, remove_numbers = TRUE, 
                     remove_symbols = TRUE, remove_hyphens = TRUE)

# Fitting LDA model
abs.track.lda <- LDA(abs.dtm.track, k = 5, control = list(seed = 359))
#abs.track.lda

# Tiding LDA topics
abs.track.lda.topics <- tidy(abs.track.lda, matrix = "beta")
#abs.track.lda.topics

# ===== Interpretation of LDA topics =====