# ----- Initialization -----
# Loading required libraries
require(rvest)

# Loading authors dataset
authors <- read.csv(file = "0_data/IPDMC_2019_Authors.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(authors) <- c("id", "full.name.raw", "surname", "name", "affiliation.raw")
# Cleaning names
authors$surname <- trimws(x = authors$surname)
authors$name <- trimws(x = authors$name)


# Creation of objects required 
scopus <- data.frame()

for (i in 1:1) {
  url <- paste(c("https://www.scopus.com/results/authorNamesList.uri?sort=count-f&src=al&sid=a217d747f53f39396233cbd979c4d9f4&sot=al&sdt=al&sl=39&s=AUTHLASTNAME%28",
                 authors$surname[i],
                 "%29+AND+AUTHFIRST%28",
                 authors$name[i],
                 

                 https://www.euractiv.com/page/", i, "/?s=democra+eu+reform&year=2014&orderby=post_date&order=ASC"), collapse = "")
  
}

ACURNURAN%29&st1=ACUR&st2=NURAN"
# webscraping subrutine
pagina <- try(read_html(url))


euractiv <- data.frame()
end <- FALSE
i <- 1