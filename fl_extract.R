

library(pdftools)

#fl <- pdf_ocr_text("/home/frousseu/Downloads/fl.pdf", pages = 406)
fl <- pdf_fonts("/home/frousseu/Downloads/fl.pdf")
fl <- pdf_data("/home/frousseu/Downloads/fl.pdf")
fl <- pdf_text("/home/frousseu/Downloads/fl.pdf")
x <- strsplit(fl[111:859],"\n") |> unlist()


rempatterns <- list(
  figure =  "\\s{4,}Figure\\s{1+}\\d+",
  page = "\\[\\s\\d+\\s\\]",
  fl = "^\\s{8,}FLORE LAURENTIENNE"
)
l <- lapply(rempatterns, function(i) {
  grep(i, x)
}) |> unlist()
x <- x[-l]


getpatterns <- list(
  species = "^\\s{4,}\\d+\\.",
  genus = "^\\s{30,}\\d+\\.",
  family = "^\\s{4,}Fam\\. ",
  specieskey = "^\\s{4,}CLEF DES ESPÈCES",
  genuskey =  "^\\s{4,}CLEF DES GENRES"
)



l <- lapply(getpatterns, function(i) {
  grep(i, x)
}) 
l$species <- l$species[!l$species %in% l$genus]

l <- l |> unlist() |> sort()

e <- c(diff(l), 30)
lapply(seq_along(l), function(i) {
  x[l[i]:(l[i]+e[i]-1)]
})

x[g]


x1 <- strsplit(fl[[406]],"\n")[[1]] 
x2 <- strsplit(pdf_ocr_text("/home/frousseu/Downloads/fl.pdf", pages = 406, language = "fra"),"\n")[[1]] 

gsub("\\. =", "  ", x) |>
  gsub("_-\\.", "  ", x = _) |>
  gsub("^\\|\\s{1,}", "    ", x = _)  |>
  gsub(" \\|", "", x = _)  |>
  gsub(", —", ". —", x = _)  |>
  gsub(", \\|", ".  ", x = _)  |>
  gsub("ÉÊ", "É", x = _) |>
  gsub("\\$", "S", x = _)

gsub("[\\x00-\\x7F]+","",x)
gsub("[[:alnum:]]","",x)

sapply(x1, function(i) {
  pattern <- gsub("[[:punct:][:blank:]]", "", i)
  to <- gsub("[[:punct:][:blank:]]", "", x2)
  if( pattern != ""){
    agrep(pattern, to)
  } else {
    NA
  }
})

