# ----- Initialization -----
# Loading required libraries
require(rscopus)
require(dplyr)
require(tidyr)
require(curl)

# Loading authors dataset
authors <- read.csv(file = "0_data/IPDMC_2019_Authors.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(authors) <- c("id", "full.name.raw", "surname", "name", "affiliation.raw")

# ===== Cleaing authors names
# Transliteration of names
authors$surname.clean <- try(iconv(authors$surname, "latin1", "ASCII//TRANSLIT"))
authors$name.clean <- try(iconv(authors$name, "latin1", "ASCII//TRANSLIT")) 
# Trimming extra spaces
authors$surname.clean <- trimws(x = authors$surname.clean)
authors$name.clean <- trimws(x = authors$name.clean)
# Removing puntuation signs
index <- grep(pattern = "[[:punct:]]", x = authors$surname.clean)
authors$surname.clean[index] <- gsub(pattern = "[[:punct:]]", replacement = "",
                                     x = authors$surname.clean[index])
index <- grep(pattern = "[[:punct:]]", x = authors$name.clean)
authors$name.clean[index] <- gsub(pattern = "[[:punct:]]", replacement = "",
                                  x = authors$name.clean[index])

# ===== Setting up API key
set_api_key(api_key = "199a37378d4d5af0155ff632ada3652f")
if (have_api_key()) {
  print("API Key accepted")
  res = author_df(last_name = "acur", first_name = "nuran", verbose = FALSE, general = FALSE)
  names(res)
  head(res[, c("title", "journal", "description")])
  unique(res$au_id)
  unique(as.character(res$affilname_1))
  
  all_dat = author_data(last_name = "acur", 
                        first_name = "nuran", verbose = FALSE, general = TRUE)
  res2 = all_dat$df
  res2 = res2 %>% 
    rename(journal = `prism:publicationName`,
           title = `dc:title`,
           description = `dc:description`)
  head(res[, c("title", "journal", "description")])
}