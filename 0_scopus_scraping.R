# ----- Initialization -----
# Loading required libraries
require(rvest)
require(pingr)
require(rscopu)

install.packages("rscopus")

install.packages("pingr")

# Loading authors dataset
authors <- read.csv(file = "0_data/IPDMC_2019_Authors.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(authors) <- c("id", "full.name.raw", "surname", "name", "affiliation.raw")
# Cleaning names
authors$surname.clean <- try(iconv(authors$surname, "latin1", "ASCII//TRANSLIT"))
authors$name.clean <- try(iconv(authors$name, "latin1", "ASCII//TRANSLIT")) 
authors$surname.clean <- trimws(x = authors$surname.clean)
authors$name.clean <- trimws(x = authors$name.clean)
# Removing puntuation signs
index <- grep(pattern = "[[:punct:]]", x = authors$surname.clean)
authors$surname.clean[index] <- gsub(pattern = "[[:punct:]]",
                                     replacement = "",
                                     x = authors$surname.clean[index])
index <- grep(pattern = "[[:punct:]]", x = authors$name.clean)
authors$name.clean[index] <- gsub(pattern = "[[:punct:]]",
                                     replacement = "",
                                     x = authors$name.clean[index])

# ----- Webscraping subrutine -----
# Creation of objects required 
scopus <- data.frame()

# scraping cycle
for (i in 1:nrow(authors)) {
  # Creating url
  the.url <- paste(c("https://www.scopus.com/results/authorNamesList.uri?sort=count-f&src=al&sid=a217d747f53f39396233cbd979c4d9f4&sot=al&sdt=al&sl=39&st1=",
                     authors$surname.clean[i], "&st2=", authors$name.clean[i]), collapse = "")
  pagina <- try(read_html(the.url))
  print(.Last.value)
  
  node <- ".noMarginBottom"
  test <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                   warning = function(w) {print("Fail in 1"); return(NA)})
  if(length(test)==1){ next }
  
  # Scraping data
  id <- authors$id[i]
  
  node <- ".docTitle"
  name <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                   warning = function(w) {print("Fail in 1"); return(NA)})
  if (length(name)==0) { name <- NA }
  
  node <- ".txtSmaller"
  name.abbr <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                        warning = function(w) {print("Fail in 1"); return(NA)})
  if (length(name.abbr)==0) { name.abbr <- NA }
  
  node <- "#resultsDocumentsCol1 a"
  n.documents <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                          warning = function(w) {print("Fail in 1"); return(NA)})
  if (length(n.documents)==0) { n.documents <- NA }
  
  node <- "#resultDataRow1 .dataCol4"
  h.index <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                      warning = function(w) {print("Fail in 1"); return(NA)})
  if (length(h.index)==0) { h.index <- NA }
  
  node <- "#resultDataRow1 .anchorText"
  affiliation <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                          warning = function(w) {print("Fail in 1"); return(NA)})
  if (length(affiliation)==0) { affiliation <- NA }
  
  node <- "#resultDataRow1 .dataCol6"
  city <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                   warning = function(w) {print("Fail in 1"); return(NA)})
  if (length(city)==0) { city <- NA }
  
  node <- "#resultDataRow1 .dataCol7"
  country <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                      warning = function(w) {print("Fail in 1"); return(NA)})
  if (length(country)==0) { country <- NA }
  
  node <- ".docTitle"
  hyperlink <- tryCatch(pagina %>% html_nodes(node) %>% html_attr("href"),
                        warning = function(w) {print("Fail in 1"); return(NA)})
  if (length(hyperlink)==0) { hyperlink <- NA }
  
  subset <- cbind(id, name, name.abbr, n.documents, h.index, affiliation, city, country, hyperlink)
  subset <- as.data.frame(subset, stringsAsFactors = FALSE)
  
  scopus <- rbind(scopus, subset)
  
}

