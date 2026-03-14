


source("/home/frousseu/Documents/github/flore.quebec/data/functions.R")

#fwrite(d,"/home/frousseu/Documents/github/flore.quebec/data/plants.csv")
#d<-fread("/home/frousseu/Documents/github/flore.quebec/data/plants2.csv")
d <- fread("/home/frousseu/Documents/github/flore.quebec/data/plants2024-12-18.csv")
#d[, inatID := ifelse(basename(inatID) == "NA", NA, inatID)]
#setdiff(dd$species, d$species)
#setdiff(d$species, dd$species)

random_photos <- fread("random_photos.csv", fill = TRUE)
random_photos$idobs <- as.integer(random_photos$idobs)
random_photos <- random_photos[!is.na(when), ]
random_photos$species <- d$species[match(random_photos$idtaxa,d$idtaxa)]

commits <- latest_species_commits(40, species = FALSE, keys = FALSE)
commits2 <- latest_species_commits(15, species = FALSE, keys = TRUE) # functions needs to stop if fewer than n

commits <- rbind(commits, commits2)

w <- which(commits$login == "Sckende")
if(any(w)){
  commits$login[w] <- "frousseu"
  commits$author[w] <- "François Rousseu"
  commits$name[w] <- "François Rousseu"
}

w <- which(commits$login == "MarcAureleVallee") # temp for marc aurèle
if(any(w)){
  commits$login[w] <- "MarcAureleVallee"
  commits$author[w] <- "Marc-Aurèle Vallée"
  commits$name[w] <- "Marc-Aurèle Vallée"
}

w <- which(commits$login == "AnneMarieBlanchette") # temp for marc aurèle
if(any(w)){
  commits$login[w] <- "AnneMarieBlanchette"
  commits$author[w] <- "Anne-Marie Blanchette"
  commits$name[w] <- "Anne-Marie Blanchette"
}

w <- which(commits$login == "Simon1Pesant") # temp for marc aurèle
if(any(w)){
  commits$login[w] <- "Simon1Pesant"
  commits$author[w] <- "Simon Pesant"
  commits$name[w] <- "Simon Pesant"
}

w <- which(commits$login == "malap2025") # temp for marc aurèle
if(any(w)){
  commits$login[w] <- "malap2025"
  commits$author[w] <- "Martine Lapointe"
  commits$name[w] <- "Martine Lapointe"
}

w <- which(commits$login == "andree-theriault") # temp for marc aurèle
if(any(w)){
  commits$login[w] <- "andree-theriault"
  commits$author[w] <- "Andrée Thériault"
  commits$name[w] <- "Andrée Thériault"
}

w <- which(commits$login == "geoffreyhall") # temp for marc aurèle
if(any(w)){
  commits$login[w] <- "geoffreyhall"
  commits$author[w] <- "Geoffrey Hall"
  commits$name[w] <- "Geoffrey Hall"
}

commits <- commits[!commits$file %in% c("Espèces/Acanthaceae/Justicia/Test_test.md", "Espèces/Acanthaceae/Justicia/Test_test2.md"), ]
commits <- commits[grepl("Espèces/|clés/", commits$file), ]

species_modified <- unique(commits$file[grepl("Espèces/", commits$file)])
selected_photos <- get_species_photos(species_modified)
setDT(selected_photos)
#fwrite(selected_photos, "selected_photos.csv")
sphotos <- fread("selected_photos.csv")
selected_photos <- unique(rbind(selected_photos, sphotos), by = c("species", "rank"))
fwrite(selected_photos, "selected_photos.csv", append = FALSE)

#fwrite(commits, "commits.csv", append = FALSE)
setDT(commits)
old_commits <- fread("commits.csv", colClasses = "character") # bug with date formats
#old_commits <- commits[!grepl("malap2025", commits$login),]
#fwrite(old_commits[,1:11], "commits.csv", append = FALSE) # bug with date formats
#repo <- ifelse(grepl("_clés.md|_taxon.md", old_commits$file), "keys", "species")
#old_commits <- cbind(repo = repo, old_commits)
#fwrite(old_commits, "commits.csv", append = FALSE)
nums <- c("additions","deletions","changes")
old_commits[, (nums) := lapply(.SD, as.integer), .SDcols = nums]
commits <- rbind(commits, old_commits)

# find author name if missing from local repo push??
w <- which(commits$name == "" | is.na(commits$name))
if(any(w)){
 nl <- commits[login %in% commits$login[w] & name != "", .(name, login)] 
 commits$name[w] <- nl$name[match(commits$login[w], nl$login)]
}  

commits <- unique(commits)#, by = c("sha", "file"))
commits <- commits[order(file, date), ]

fwrite(commits, "commits.csv", append = FALSE)

selected_photos<-fread("selected_photos.csv")
selected_photos$idtaxa <- d$idtaxa[match(selected_photos$species,d$species)]

photos <- merge(selected_photos, random_photos, all = TRUE)
photos <- photos[order(species, rank), ]
photos <- split(photos, photos$species) |>
  lapply(function(i){i[1:min(c(nrow(i),8)), ]}) |>
  rbindlist()

spcontrib <- list_contributions(commits[repo == "species", ]) #!!!!!!!!!!!!!!!!!!!!!!!!!! get con fro keys
commits[ , species := trimws(gsub("_|\\.md"," ",basename(file)))]
hccontrib <- list_hc_contributions("/home/frousseu/Documents/github/flore.quebec/species/Espèces")
spcontrib <- spcontrib[!spcontrib$species %in% hccontrib$species, ] |> rbind(hccontrib)
setDT(spcontrib)

d <- merge(d, spcontrib, all.x = TRUE)
d[ , date := ifelse(is.na(date), "1970-01-01T00:00:00Z", date)]

#commits[ , species := trimws(gsub("_|\\.md"," ",basename(file)))]

### get latest modifications
#x <- commits[repo == "species", ]
#x <- x[order(-date),]
#g <- grep("Merge pull request|Merge branch", x$message)
#if(any(g)){ # do not count merges for changes or species contributions
#  x <- x[-g, ]
#} 
#d[ , date := "1970-01-01T00:00:00Z"]
#latest <- x$date[match(d$species, x$species)]
#d[ , date := ifelse(!is.na(latest), latest, d$date)]


#pics<-data.frame(id=ids,pics=photos)
#photos<-merge(photos,d,by.x="idtaxa",by.y="idtaxa",all.x=TRUE)

table(duplicated(d$idtaxa))
d[ duplicated(d$idtaxa), ]

#d <- d[!is.na(idtaxa), ] # ???????????? Why!!!!!
photos <- merge(photos, d, by.x = "species", by.y = "species", all = TRUE) # Also keep what does not have photo !!!!!!
names(photos) <- gsub("\\.x|i\\.","", names(photos)) # ?????????
photos <- photos[order(species,rank)]
#pics<-photos[order(species),]

pics <- split(photos, photos$species) #|>
#lapply(function(i){i[1:min(c(nrow(i),8)), ]})

hccontrib <- rbind(
  list_hc_contributions("/home/frousseu/Documents/github/flore.quebec/species/Espèces"),
  list_hc_contributions("/home/frousseu/Documents/github/flore.quebec/keys/clés")
)

hc <- split(hccontrib, hccontrib$file) |>
  lapply(function(i){
    ini <- unlist(strsplit(i$initiated, ", "))
    edi <- unlist(strsplit(i$edited, ", "))
    data.frame(species = i$species[1], file = i$file[1], cont = c(rep("initiated", length(ini)), rep("edited", length(edi))), name = c(ini, edi), data = i$date[1])
  }) |>
  do.call("rbind", args = _) |>
  as.data.table()


hcc <- list(
  # nb de changements 
  data.table(name = character(), nbchanges = integer()), 
  # nb de commits
  data.table(name = character(), nbcommits = integer()), 
  # nb d'espèces initiées
  hc[grepl("Espèces", file) & cont == "initiated", ][, .(nbspinitiated = .N), by = .(name)], 
  hc[grepl("_clé.md", file) & cont == "initiated", ][, .(nbkeyinitiated = .N), by = .(name)], 
  hc[grepl("_taxon.md", file) & cont == "initiated", ][, .(nbtaxoninitiated = .N), by = .(name)],
  # nb d'espèces modifiées
  hc[grepl("Espèces", file) & cont == "edited", ][, .(nbspmodified = .N), by = .(name)], 
  hc[grepl("_clé.md", file) & cont == "edited", ][, .(nbkeymodified = .N), by = .(name)], 
  hc[grepl("_taxon.md", file) & cont == "edited", ][, .(nbtaxonmodified = .N), by = .(name)]
)
x <- Reduce(function(df1, df2) merge(df1, df2, by = "name", all = TRUE), hcc)
x[is.na(x)] <- 0
#x <- merge(x, unique(commits[name != "", c("name", "login")]), all.x = TRUE)
xhc <- x[order(-nbspinitiated, -nbspmodified), ]



############################################
### Compute contributor contributions
x <- commits[date > "2024-01-28T18:17:06Z", ]
g <- grep("Merge branch", x$message)
if(any(g)){ # do not count merge branch for commits, but count merge pull requests
  y <- x[-g, ]
} else {
  y <- x
}
g <- grep("Merge pull request|Merge branch", x$message)
if(any(g)){ # do not count merges for changes or species contributions
  z <- x[-g, ]
} else {
  z <- x
}
zz <- z[!z$file %in% hc$file, ] # remove files were credits are hard coded

xl <- list(
  # nb de changements 
  z[ , .(nbchanges = sum(changes)), by = .(name)], 
  # nb de commits
  y[ , .(nbcommits = .N), by = .(name)], 
  # nb d'espèces initiées
  unique(zz[repo == "species", ], by = c("species"))[ , .(nbspinitiated = .N), by = .(name)], 
  unique(zz[grepl("_clé.md", file), ], by = c("species"))[ , .(nbkeyinitiated = .N), by = .(name)], 
  unique(zz[grepl("_taxon.md", file), ], by = c("species"))[ , .(nbtaxoninitiated = .N), by = .(name)],
  # nb d'espèces modifiées
  #unique(z[duplicated(z, by = c("species")), ] , by = c("name", "species"))[ , .(nbspmodified = .N), by = .(name)], 
  zz |>
    unique(by = c("name", "species")) |>
    _[order(species, date), ] |>
    _[duplicated(species), ] |>
    _[ , .(nbspmodified = .N), by = .(name)],
  zz[grepl("_clé.md", file), ] |>
    unique(by = c("name", "species")) |>
    _[order(species, date), ] |>
    _[duplicated(species), ] |>
    _[ , .(nbkeymodified = .N), by = .(name)],
  zz[grepl("_taxon.md", file), ] |>
    unique(by = c("name", "species")) |>
    _[order(species, date), ] |>
    _[duplicated(species), ] |>
    _[ , .(nbtaxonmodified = .N), by = .(name)] 
)
x <- Reduce(function(df1, df2) merge(df1, df2, by = "name", all = TRUE), xl)
x[is.na(x)] <- 0
#x <- merge(x, unique(commits[name != "", c("login", "name")]), all.x = TRUE)
x <- x[order(-nbspinitiated, -nbspmodified), ]

x <- rbind(x, xhc)[, lapply(.SD, sum), by = .(name)]

ma <- match(x$name, commits$name)
x[, login := ifelse(is.na(ma), NA, commits$name[ma])]


### get user photo
if(FALSE){
  token <- readLines("/home/frousseu/.ssh/github_token")
  x$picurl <- unlist(lapply(x$login, function(i){
    if(is.na(i)){
      ""
    } else {
      githubapi <- paste0("https://frousseu:", token, "@api.github.com/users/", i)
      x <- fromJSON(githubapi)
      x$avatar_url
    }
  }))
}


lc <- split(x, x$name)
lc <-lc[x$name] # keep original ordering
lc <- lapply(lc, function(i){
  arr <- paste0(names(i), ": \"", unlist(as.vector(i[1, ])), "\"") |>
    paste(collapse = ", ")
  paste0("{", arr, "}") 
})

#pics<-pics[!duplicated(species),]
#pics<-pics[!is.na(pics$url),]


#fwrite(pics,"C:/Users/franc/Downloads/pics.csv")
#fread("C:/Users/franc/Downloads/pics.csv")

#image_container<-function(species,url){
#  cat("/014")
#  invisible(lapply(seq_along(species),function(i){
#    cat(paste0("
#      <div class=/"image-container/">
#        <img class=/"image/" src=/"",url[i],"/" alt=/"Additional Image 8/">
#        <div class=/"image-title/">",species[i],"</div>
#      </div>
#    "))
#  }))
#}
#image_container(pics$species,pics$url)

contrib <- list_hc_contributions("/home/frousseu/Documents/github/flore.quebec/keys/clés")

keycontrib <- list_contributions(commits[grep("_clé.md", file), ])
hckeycontrib <- contrib[grepl("clé.md", contrib$file),]
setDT(hckeycontrib)
keycontrib <- keycontrib[!keycontrib$species %in% hckeycontrib$species, ] |> rbind(hckeycontrib)

taxoncontrib <- list_contributions(commits[grep("_taxon.md", file), ])
hctaxoncontrib <- contrib[grepl("taxon.md", contrib$file),]
setDT(hctaxoncontrib)
taxoncontrib <- taxoncontrib[!taxoncontrib$species %in% hctaxoncontrib$species, ] |> rbind(hctaxoncontrib)

keys <- rbind(
  keycontrib,
  taxoncontrib
) |> setDT()
keys[, taxon := gsub(" clé| taxon", "", species)]
#keys[, file := basename(file)]
keys <- keys[, .(file, initiated, edited, date)]
keys <- as.data.frame(keys)
keys$initiated <- lapply(strsplit(keys$initiated, ", "), I)
keys$edited <- lapply(strsplit(keys$edited, ", "), I)
#keys[, initiated := paste0("[\"", gsub("\\, ", "\", \"", initiated),"\"]")]
#keys[, edited := paste0("[\"", gsub("\\, ", "\", \"", edited),"\"]")]





image_array<-function(){
  #cat("/014")
  l<-sapply(pics,function(i){
    tags<-c("src","alt","famille","genre","section","espèce","fna","inat","vascan","gbif","powo","herbierqc","class","ordre","nobs","vernaculaire","vernacularFRalt","vernacularEN","botanic","alternatif","status","protection","taxonomic_order","LOIEMV","COSEWIC","SARASTATUS","GRANK","NRANK","SRANK","contribution","date", "'sous-famille'", "'sous-genre'", "tribu", "section", "'sous-tribu'", "série", "'sous-section'")
    tagnames<-c("url","species","family","genus","section","species","fna","inat","vascan","gbif","powo","herbierqc","class","order","nobs","vernacularFR","vernacularFRalt","vernacularEN","botanic","alternatif","Québec","protection","taxonomic_order","LOIEMV","COSEWIC","SARASTATUS","GRANK","NRANK","SRANK","contribution","date", "subfamily", "subgenus", "tribe", "section", "subtribe", "series", "subsection")
    info<-unlist(as.vector(i[1,..tagnames]))
    info<-unname(sapply(info,function(x){paste0("\"",x,"\"")}))

    urls<-i$url[1:8]
    urls<-paste0("[ \"",paste(urls,collapse="\", \""),"\" ]")
    tags<-c(tags,"images")
    info<-c(info,urls)
    
    initiators <- strsplit(i$initiated, ", ")[[1]]
    initiators <- paste0("[ \"", paste(initiators, collapse = "\", \""), "\" ]")
    tags <- c(tags, "initiated")
    info <- c(info, initiators)
    
    editors <- strsplit(i$edited, ", ")[[1]]
    editors <- paste0("[ \"", paste(editors, collapse = "\", \""), "\" ]")
    tags <- c(tags, "edited")
    info <- c(info, editors)
    
    types <- strsplit(i$type, ", ")[[1]]
    types <- paste0("[ \"", paste(types, collapse = "\", \""), "\" ]")
    tags <- c(tags, "type")
    info <- c(info, types)

    urls<-i$attribution[1:8]
    ww<-which(urls=="no rights reserved")
    if(any(ww)){
      name<-ifelse(is.na(i$name),i$login,i$name)
      name<-ifelse(name=="",i$login,name)
      urls[ww]<-paste0("(c) ",name[1:8][ww],", no rights reserved (CC0)")
    }
    urls<-paste0("[ \"",paste(urls,collapse="\", \""),"\" ]")
    tags<-c(tags,"credit")
    info<-c(info,urls)
    
    urls<-i$idobs[1:8]
    urls<-paste0("https://www.inaturalist.org/observations/",urls)
    urls<-paste0("[ \"",paste(urls,collapse="\", \""),"\" ]")
    tags<-c(tags,"link")
    info<-c(info,urls)

    arr<-paste("{",paste0(paste0(tags,": ",info),collapse=", "),"},",collapse="")
    #arr<-gsub("/"{","{",arr)
    #arr<-gsub("}/"","}",arr)
    #cat(arr,"/n")
    arr
    #write(paste("const = [",arr,"];"),file="data.js",append=TRUE)
  })
  arr<-paste(l,collapse=" ")
  write(paste("const data = [",arr,"];"),file="data.js",append=TRUE)
}

option_values<-function(x, tag, append = TRUE){
  #cat("/014")
  values <- sort(unique(x[[tag]]))
  write(paste0("const ", paste0(tag, "_values"), " = [\"", paste0(values, collapse = "\", \""),"\"];"), file= "data.js", append = append)
}

all_values<-function(x,append=TRUE){
  values1<-unique(x[["nom"]])
  values2<-unique(x[["vernacularFR"]])
  values2<-unique(sapply(strsplit(values2," "),"[",1))
  #values3<-unique(x[["vernacularEN"]])
  #values3<-unique(sapply(strsplit(values3," "),"[",1))
  values<-sort(unique(c(values1,values2)))
  values<-values[values!=""]
  write(paste0("const ",paste0("nom","_values")," = [\"",paste0(values,collapse="\", \""),"\"];"),file="data.js",append=append)
  m<-lapply(values,function(i){
    paste("{",paste0(gsub("-|'|’| ","_",i),": [",paste(sort(unique(c(grep(i,x[["nom"]]),grep(i,x[["vernacularFR"]])))-1),collapse=", "),"]"),"}")
  })
  arr<-paste(unlist(m),collapse=", ")
  write(paste("const common_names = [",arr,"];"),file="data.js",append=append)
}

# dim(d) and length(pics) that's the probelm with the indexing

write("", file= "data.js", append = FALSE)
#option_values(rbindlist(pics),tag="family", append=FALSE) # the option values are probably not useful anymore
#option_values(rbindlist(pics),tag="genus")
#option_values(rbindlist(pics),tag="section")
#option_values(rbindlist(pics),tag="species")
all_values(d)
#write("", file= "data.js", append = FALSE)
image_array()
write(paste("const contributions = [", paste(lc, collapse = ", ") ,"];"), file = "data.js", append = TRUE)
write(paste0("const keys = ", toJSON(keys, auto_unbox = TRUE), ";"), file = "data.js", append = TRUE)
system("cp /home/frousseu/Documents/github/flore.quebec/data/data.js /home/frousseu/Documents/github/flore.quebec/flore.quebec/data.js")
#file.show("/home/frousseu/Documents/github/floreqc/flora.html")


#x<-x[grep("QC//, Canada|Québec//, Canada",x$place_guess),]


#library(wordcloud2)

#df<-as.data.frame(table(d[d$family!="As",]$genus))
#df<-df[rev(order(df$Freq)),]
#row.names(df)<-df[,1]
#wordcloud2(data=df, size=0.75, shape="square",color='random-dark',minSize=1)





