
library(jsonlite)

#x <- sptax
#l <- lapply(names(x)[!names(x) %in% c("class", "subclass", "superorder", "order", "variety", "subspecies")], function(i){
#  data.frame(level = i, taxa = sort(unique(x[[i]])))
#}) |> do.call("rbind", args = _)

x <- d
l <- lapply(names(x)[names(x) %in% c("family", "subfamily", "tribe", "subtribe", "genus", "subgenus", "section", "subsection", "series", "species")], function(i){
  data.frame(level = i, taxa = sort(unique(x[[i]])))
}) |> do.call("rbind", args = _)

taxoen <- c("class", "subclass", "superorder", "order", "family", "subfamily", "tribe", "subtribe", "genus", "subgenus", "section", "subsection", "series", "species", "subspecies", "variety")

taxofr <- c("classe", "sous-classe", "super-ordre", "ordre", "famille", "sous-famille", "tribu", "sous-tribu", "genre", "sous-genre", "section", "sous-section", "série", "espèce", "sous-espèce", "variété")

l$level <- taxofr[match(l$level, taxoen)]


#keep <- c("famille", "genre", "section", "espèce")
keep <- c("famille", "sous-famille", "tribu", "sous-tribu", "genre", "sous-genre", "section", "sous-section", "série", "espèce")


l <- l[l$level %in% keep, ]
#l <- l[!l$level %in% c("classe", "sous-classe", "super-ordre", "ordre"), ]

#l <- l[taxon$taxonomicStatus[match(l$taxa, taxon$sp)] == "accepted", ]

### needs to keep what is found in Quebec at all taxonomic levels and this can be found in the early d!!!

#sortorder <- c("famille", "genre", "espèce", "section")
#sortorder <- taxofr
sortorder <- c("classe", "sous-classe", "super-ordre", "ordre", "famille", "genre", "espèce", "sous-famille", "tribu", "sous-tribu", "sous-genre", "section", "sous-section", "série", "sous-espèce", "variété")
l <- l[order(match(l$level, sortorder), l$taxa), ]

#l$taxa <- sapply(strsplit(l$taxa, " "), function(i){
#  if(length(i) == 3){
#    i[3]
#  } else {
#    paste(i, collapse = " ")
#  }
#})

l <- l[l$taxa != "", ]
l$taxa <- gsub("_", " ", l$taxa)

values1 <- unique(d[["nom"]]) # this and vernacularFR are the same thing I think, remove one
values2 <- unique(d[["vernacularFR"]])
values2 <- unique(sapply(strsplit(values2," "), "[", 1))
values <- sort(unique(c(values1, values2)))
values <- values[values != ""]
nom <- data.frame(level = "nom", taxa = values)
l <- rbind(l, nom)

paste0("const taxa = ", toJSON(l), ";") |> write("taxa.js", append = FALSE)
system("cp /home/frousseu/Documents/github/flore.quebec/data/taxa.js /home/frousseu/Documents/github/flore.quebec/flore.quebec/taxa.js")
