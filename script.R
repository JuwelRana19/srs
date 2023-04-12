library(httr)
library(rjson)
library(rvest)
library(stringr)


cat("Rechercher dans Usito : ")
query <- readLines("stdin", n = 1)

usito <- handle("https://usito.usherbrooke.ca/")

response <- GET(handle = usito, path = "v2/documents", query = list(contient = query))
json <- content(response, as = "text", encoding = "UTF-8")
documents <- fromJSON(json)
ids <- sapply(documents$données$items, function(x) sub("(\\.([a-zA-Z]|[à-ü]|[À-Ü]){3,})?\\.[a-zA-Z]{2,}$", "", x$clé$id))

response <- GET(handle = usito, path = "v2/mots/suggestions", query = list(caractères = query))
json <- content(response, as = "text", encoding = "UTF-8")
suggestions <- fromJSON(json)

rm(usito)

if (length(ids) > 10) {
  ids <- ids[1:10]
}
if (length(suggestions) > 10) {
  suggestions <- suggestions[1:10]
}
choices <- unique(unlist(c(ids, suggestions)))

print(choices)
cat("Sélectionner : ")
idx <- as.integer(readLines("stdin", n = 1))
choice <- choices[idx]

if (is.na(choice)) {
  cat("Pas de sélection\n")
} else {
  page <- read_html(paste0("https://usito.usherbrooke.ca/définitions/", choice))
  definitions <- page %>%
    html_elements(xpath = "//span[@class='definition_entree-style']|//span[not(@class='definition_entree-style')]/span[@class='def_sous_entree-style']") %>%
    html_text() %>%
    gsub("'", "’", ., fixed = TRUE) %>%
    str_squish()

  if (!length(definitions)) {
    cat("Pas de définition\n")
  } else {
    print(definitions)
    cat("Sélectionner : ")
    idx <- as.integer(readLines("stdin", n = 1))
    definition <- definitions[idx]

    if (is.na(definition)) {
      cat("Pas de sélection\n")
    } else {
      etymologie <- page %>%
        html_elements(xpath = "//span[@class='etymologie-information']/span[@class='etym-style']/node()[not(@style)]") %>%
        html_text() %>%
        paste(collapse = "") %>%
        str_squish()
      out <- file("vocab.csv", "a")
      cat(sprintf('"%s","%s","%s"\n', choice, definition, etymologie), file = out)
      close(out)
      cat(sprintf('"%s","%s","%s"\n', choice, definition, etymologie))
    }
  }
}
