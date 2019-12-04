# ----- Initialization -----
# Loading required libraries
require(rscopus)
require(dplyr)
require(tidyr)
require(curl)

# Loading authors dataset
authors <- read.csv(file = "1_data/IPDMC_2019_Authors.csv", header = TRUE, stringsAsFactors = FALSE)
names(authors) <- c("local.id", "full.name", "surname", "name", "affiliation.raw")

# ===== Cleaing authors names
# Transliteration of names
authors$surname.clean <- try(iconv(authors$surname, "latin1", "ASCII//TRANSLIT"))
authors$name.clean <- try(iconv(authors$name, "latin1", "ASCII//TRANSLIT")) 
# Trimming extra spaces
authors$surname.clean <- trimws(x = authors$surname.clean)
authors$name.clean <- trimws(x = authors$name.clean)
# To lowercase
authors$surname.clean <- tolower(x = authors$surname.clean)
authors$name.clean <- tolower(x = authors$name.clean)
# Removing puntuation signs
temp <- grep(pattern = "[[:punct:]]", x = authors$surname.clean)
authors$surname.clean[temp] <- gsub(pattern = "[[:punct:]]", replacement = "",
                                     x = authors$surname.clean[temp])
temp <- grep(pattern = "[[:punct:]]", x = authors$name.clean)
authors$name.clean[temp] <- gsub(pattern = "[[:punct:]]", replacement = "",
                                  x = authors$name.clean[temp])
names(authors) <- c("local.id", "full.name", "surname", "name", "affiliation.raw")

# ===== Setting up API key
set_api_key(api_key = "199a37378d4d5af0155ff632ada3652f")
if (have_api_key()) { print("API Key accepted") }

# Quering routine
scopus.data <- list()
missing.data <- NA
for (i in 1:nrow(authors)) {
  # Progress update
  if (i%%as.integer(nrow(authors)/20) == 0) { 
    print(paste(c(round(i/nrow(authors)*100,0), "% completed"), collapse = ""))
  }
  # Quering Scopus
  temp <- try(author_df(last_name = authors$surname.clean[i], 
                             first_name = authors$name.clean[i], 
                             verbose = FALSE, general = TRUE),
                   silent = TRUE)
  # Skiping errors
  if (class(temp) == "try-error") {
    missing.data <- c(missing.data, i)
    next
  }
  # Binding search to general dataset
  temp$local.id <- i
  scopus.data[[i]] <- temp
}

# ===== Creation of workable dataframes =====
# Binding authors data dataframe
scopus.data.flat <- bind_rows(scopus.data)
# Adding orginal data
temp <- subset(x = authors, select = c("local.id", "full.name"))
scopus.data.flat <- merge(x = scopus.data.flat, y = temp, by = "local.id")

# Storing data froma authors not found.
not.found <- as.data.frame(as.integer(missing.data[-1]))
names(not.found) <- "local.id"
not.found <- merge(x = not.found, y = authors, by = "local.id")

# Storing external files
save(list = c("authors", "not.found", "scopus.data", "scopus.data.flat"),
     file = "3_output/0_scopus_query.RData")
write.csv(x = scopus.data.flat, file = "3_output/1_scopus_query.csv")
write.csv(x = not.found, file = "3_output/2_authors_not_found.csv")
