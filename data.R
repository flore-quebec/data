
library(data.table)
library(jsonlite)
library(future)
library(future.apply)
library(RCurl)
library(magick)
library(taxize)
library(foreach)
library(doParallel)
library(httr)
library(rgbif)
library(Hmisc)
library(sf)


### VASCAN request when updatingS
# https://data.canadensys.net/vascan/checklist?lang=fr&habit=all&taxon=0&combination=anyof&province=QC&status=native&status=introduced&status=ephemeral&rank=class&rank=order&rank=family&rank=genus&rank=species&nolimit=false&sort=taxonomically&criteria_panel=selection

###https://data.canadensys.net/vascan/checklist?lang=fr&habit=all&taxon=0&combination=anyof&province=QC&status=native&status=introduced&status=ephemeral&rank=class&rank=subclass&rank=superorder&rank=order&rank=family&rank=subfamily&rank=tribe&rank=subtribe&rank=genus&rank=subgenus&rank=section&rank=subsection&rank=series&rank=species&rank=subspecies&rank=variety&nolimit=false&sort=taxonomically&criteria_panel=selection ## 2024-11-10

d <- fread("/home/frousseu/Documents/github/flore.quebec/data/vascan/DWC-2c9d816e-9ff9-4c1f-8bb5-f3047e05f50a/TXT-76b0e2bc-7f67-46c2-8d78-1e3687b59e26.txt",header=TRUE,encoding="UTF-8")
d$species <- d$`Nom scientifique`

taxon <- fread("/home/frousseu/Documents/github/flore.quebec/data/vascan/DWC-2c9d816e-9ff9-4c1f-8bb5-f3047e05f50a/taxon.txt")

### First get full taxonomy
taxo <- c("Classe", "Sous classe", "Super ordre", "Ordre", "Famille", "Sous famille", "Tribu", "Sous tribu", "Genre", "Sous genre", "Section", "Sous section", "Série", "Espèce", "Sous espèce", "Variété")
setdiff(taxo, names(table(d$Rang)))



source("functions.R")
#path <- "vascan"
path <- "vascan/DWC-2c9d816e-9ff9-4c1f-8bb5-f3047e05f50a"
lf <- list.files(path, full = TRUE, pattern = ".txt")
d <- fread(grep("TXT-", lf, value = TRUE), header = TRUE, encoding = "UTF-8")
gbif<-fread("gbif/0039190-240321170329656.csv")



taxon <- fread(grep("taxon", lf, value = TRUE))
### Hydroctyle Hydrocotyle scientificName has a problem with the author
taxon[, scientificNameAuthorship := gsub("Hydrocotyle", "", scientificNameAuthorship)]
taxon[, scientificName := gsub("Hydrocotyle Hydrocotyle", "Hydrocotyle", scientificName)]

sp <- sapply(1:nrow(taxon), function(i){ # the following removes authors from the name and it relies on the author text to be the same in both
  authors <- taxon$scientificNameAuthorship[i]
  if(authors != ""){
    gsub(authors, "", taxon$scientificName[i], fixed = TRUE) |>
      gsub("\\s+", " ", x = _) |>
      trimws()
  } else {
    taxon$scientificName[i]
  }
})
taxon[, sp := sp]



sptax <- invisible(sapply(1:nrow(taxon), function(i){
  print(i)
  parents <- NULL
  ma <- match(taxon$parentNameUsageID[i], taxon$taxonID) 
  while(!is.na(ma)){
    parents <- c(parents, taxon$taxonID[ma])
    ma <- match(taxon$parentNameUsageID[ma], taxon$taxonID) 
  }
  matches <- match(c(taxon$taxonID[i], parents), taxon$taxonID)
  res <- taxon$sp[matches]
  names(res) <- taxon$taxonRank[matches]
  res |> as.list() |> as.data.table()
})) |> rbindlist(fill = TRUE)


sptaxssp <- sptax[taxon$taxonomicStatus == "accepted", ]
l <- split(sptaxssp, sptaxssp$species)
fulltaxon <- do.call("rbind", lapply(l, function(i){
  s <- i[is.na(i$variety) & is.na(subspecies), ]  
  if(nrow(i) >=2){
    if(nrow(i) == 2){
      s$sub <- sort(c(i$subspecies, i$variety))
    } else {
      s$sub <- paste(sort(unique(c(i$subspecies, i$variety))), collapse = ", ") 
    }
  } else {
    s$sub <- NA
  }
  s
}))





#spnames<-unique(taxon$scientificName)
#sp<-sapply(strsplit(spnames," "),function(i){
#  g <- match(c("var.", "subsp."), i)
#  if(all(is.na(g))){
#    paste(i[1:2],collapse=" ")
#  }else{
#    if(all(!is.na(g))){
#      paste(i[1:6],collapse=" ")
#      print("double")
#    }else{
#      paste(i[1:4],collapse=" ")
#    }
#  }
#})
#spnames<-data.table(scientificName=spnames,sp=sp)
#taxon<-taxon[spnames, on = "scientificName"]


distribution<-fread(grep("distribution",lf,value=TRUE))
distribution<-distribution[locality=="Québec",]

vernacular<-fread(grep("vernacularname",lf,value=TRUE))
vernacular<-vernacular[language=="FR" & isPreferredName,]


d<-d[Rang=="Espèce",]
d$species<-d$"Nom scientifique"
#d<-d[-grep("×",d$species),] # removes hybrid
d[,taxonID:=as.integer(basename(URL))]

# d<-d[taxon,on="taxonID", nomatch=NULL]
d <- merge(d, taxon, all.x = TRUE)
d <- merge(d, fulltaxon[, c("species", setdiff(names(fulltaxon), names(d))), with = FALSE], by = "species", all.x = TRUE)


taxon2<-taxon[taxonRank=="species" & taxonomicStatus=="synonym",]
taxon2[,taxonID:=as.integer(acceptedNameUsageID)]
taxon2[,species:=sapply(strsplit(acceptedNameUsage," "),function(i){paste(i[1:2],collapse=" ")})]
taxon2[,species_alt:=paste(genus,specificEpithet)]
taxon2<-taxon2[,.(taxonID,species,species_alt)]
taxon2<-taxon2[!is.na(taxonID),]
taxon2<-taxon2[order(species),]
nbobs <- sapply(taxon2$species_alt,function(i){
  length(which(gbif$species == i))
})
taxon2[,nbobs:=nbobs]
taxon2[species == "Koeleria spicata",.(species, species_alt, nbobs)]

#x<-fromJSON("https://api.inaturalist.org/v1/observations/91205262%2C80182732")





d<-d[taxon2,on="taxonID",species_alt:=i.species_alt]

### add sections
#ma <- match(d$taxonID, taxon$taxonID)
#section <- sapply(strsplit(taxon$higherClassification[ma], ";"), function(i){
#  g <- grep(" sect\\. ", i)
#  if(any(g)){
#    sapply(strsplit(i[g]," "), "[", 3)
#  }else{
#    NA
#  }
#})
#d$section <- section




#d2<-merge(d,taxon2,all.x=TRUE)

#d<-d[family%in%c("Poaceae","Cyperaceae","Juncaceae","Lamiaceae","Asteraceae"),]

d<-merge(d,distribution,all.x=TRUE,by="taxonID")
#d<-d[distribution,,on="taxonID", nomatch=NA] # bug here!!!!!!!!
#d<-d[vernacular,,on="taxonID", nomatch=NA]
d<-merge(d,vernacular,all.x=TRUE,by="taxonID")


### add plant type
desc <- fread(file.path(path, "description.txt"))
d <- merge(d, desc, all.x = TRUE)
d$type <- d$description
d$type <- gsub("herb", "herbe", d$type)
d$type <- gsub("vine", "vigne", d$type)
d$type <- gsub("tree", "arbre", d$type)
d$type <- gsub("shrub", "arbuste", d$type)
d$type <- ifelse(d$order %in% "Lycopodiales", "lycopode", d$type)
d$type <- ifelse(d$order %in% "Equisetales", "prêle", d$type)
d$type <- ifelse(d$order %in% c("Ophioglossales", "Osmundales", "Polypodiales", "Schizaeales"), "fougère", d$type)
d$type[d$type == ""] <- "herbe"
d$type <- gsub(",", ", ", d$type)

#i<-"Equisetum pratense"
#x<-fromJSON(paste0("https://api.inaturalist.org/v1/taxa?q=/"",gsub(" ","%20",i),"/""))$results$id[1]

### gnr
sp <- d$species#[1:200]

#d$inatID <- translate2inat(sp)
d$inatID <- translate2inat2(sp)

table(is.na((d$inatID)))

#xx <- d[is.na(as.integer(basename(d$inatID))),.(species,species_alt,inatID)]

#translate2inat2(xx$species_alt)

### get inat ids from checklistbank.org
#url<-"https://api.checklistbank.org/dataset/2012/nameusage/"
#species<-unique(d$taxonID)#[1:100]
#l<-lapply(species,function(i){
#  print(match(i,species))
#  x<-fromJSON(paste0(url,d$taxonID[match(i,d$taxonID)],"/related?datasetKey=139831"))
#  if(length(x)>0){
#    k<-x$status=="accepted"
#    if(any(k)){
#      sp<-unique(x$name[k,])
#    }else{
#      sp<-unique(x$name)
#    }
#    g<-grep(" × ",sp$scientificName)
#    if(length(g)>0 && nrow(sp)>1){
#      sp<-sp[-g,]
#    }
#    sp$id
#  }else{
#    NA
#  }
#})
#names(l)<-species
#species[sapply(l,length)==0]
#species[sapply(l,length)==2]


#inatnames<-data.table(taxonID=species,inatID=unlist(l,use.names=FALSE))
#d<-merge(d,inatnames,all.x=TRUE)
#inat$cbnm<-inat$taxref
#inat$cbnm<-gsub("var. |subsp. ","",inat$cbnm)


### FNA links
d$fna<-paste0("http://floranorthamerica.org/",gsub(" ","_",d$species))
links<-unique(d$fna)
plan(multisession,workers=8)
ex<-future_lapply(links,url.exists)
plan(sequential)
d$fna<-ifelse(unlist(ex)[match(d$fna,links)],d$fna,NA)


### POWO links maybe use only accepted = TRUE
sp<-d$species#[1:2]
powo <- do.call("rbind", lapply(sp, function(i){
  Sys.sleep(0.25) # kew api rate limit is apparently 5/sec see https://github.com/ropensci/taxize/issues/836
  get_pow_(i, ask = FALSE, accepted = FALSE, rank_filter = "species") # need to correct
})) 
#powourl<-data.frame(sp=sp,powo=attributes(powo)$uri)
powourl<-data.frame(sp=sp,powo=sapply(powo,function(i){paste0("http://powo.science.kew.org/",i$url[1])}))
d$powo<-powourl$powo[match(d$species,powourl$sp)]


#
#powo <- get_pow_(sp, ask = FALSE, accepted = FALSE, rank_filter = "species") 
#powonames <- sapply(powo,function(i){if(is.null(i)){NA}else{i[i$accepted & i$rank=="Species", ]$name[1]}})


sp <-d[is.na(inatID), .(species)]$species
q <- lapply(sp, function(i){
  print(i)
  x <- fromJSON(paste0("https://api.inaturalist.org/v1/search?q=",gsub(" ","%20",i)))
  x <- x$results$record
  x <- x[x$rank != "hybrid" & x$iconic_taxon_name == "Plantae" & x$is_active, c("name", "id")]
  Sys.sleep(0.5)
  x
})

ans <-do.call("rbind", lapply(q, function(i){
  if(is.null(i)){
    data.frame(name = NA, id = NA)
  } else {
    i[1, ]
  }
}))
ans$sp <- sp
setDT(ans)
d[is.na(inatID), inatID := ans[.SD, on=.(sp), x.id]]




#res <- data.frame(sp = sp, powonames = powonames)
#res <- cbind(res, ans)
#row.names(res) <- NULL

species_alt <- taxon2[species %in% sp, ]$species_alt
ans <- translate2inat2(species_alt)
ans <- data.table(taxon2[species %in% sp, ], inatID = ans)
ans <- ans[ !is.na(inatID), ]
ans <- ans[order(species, -nbobs)]
ans <- ans[!duplicated(species), ]

#test <- merge(d, ans[, .(species, inatID)])


#d[, inatID := ifelse(basename(inatID) == "NA", NA, inatID)]
d[is.na(inatID), inatID := ans[.SD, on=.(species), x.inatID]]
sp <- d[is.na(inatID), .(species,powo)]$species
powo <- get_pow_(sp, ask = FALSE, accepted = FALSE, rank_filter = "species") 
powonames <- lapply(powo,function(i){if(is.null(i)){NA}else{i[i$accepted & i$rank=="Species", ]$name[1]}})
powonames <- powonames[!sapply(powonames,is.na)]
ans <- translate2inat(gsub(" × ", " ", unname(unlist(powonames))))
ans <- data.table(species = names(powonames), inatID = unname(unlist(ans)))
d[is.na(inatID), inatID := ans[.SD, on=.(species), x.inatID]]
sp <- d[is.na(inatID), .(species,powo)]$species
powo <- get_pow_(sp, ask = FALSE, accepted = FALSE, rank_filter = "species") 
powonames <- lapply(powo,function(i){if(is.null(i)){NA}else{i[i$accepted & i$rank%in%c("Subspecies","Variety"), ]$name[1]}})
powonames <- powonames[!sapply(powonames,is.na)]
ans <- translate2inat(gsub(" × ", " ", unname(unlist(powonames))))
ans <- data.table(species = names(powonames), inatID = unname(unlist(ans)))
d[is.na(inatID), inatID := ans[.SD, on=.(species), x.inatID]]
sp <- d[is.na(inatID), .(species,powo)]$species

### Manual add-ons
man <- list(
  #"Chenopodium strictum" = 784685,
  #"Crataegus lemingtonensis" = 1393474,
  #"Hackelia americana" = 209198,
  #"Ligusticum scoticum" = 1446175,
  #"Pentanema britannicum" = 1320685,
  #"Salix elaeagnos" = 338098
  #"Crataegus knieskerniana" = 
  "Palustricodon aparinoides" = 127957
)

ans <- data.table(species = "Palustricodon aparinoides", inatID = 127957)
d[is.na(inatID), inatID := ans[.SD, on=.(species), x.inatID]]
sp <- d[is.na(inatID), .(species,powo)]$species

### iNat links
inat<-basename(d$inatID)
d$inat<-ifelse(is.na(inat), NA, paste0("https://www.inaturalist.org/observations?subview=grid&place_id=13336&taxon_id=", inat))




### VASCAN links
#im<-image_read("https://layout.canadensys.net/common/images/favicon.ico")
#image_write(image_trim(im[6]),"/home/frousseu/Documents/github/floreqc/vascanlogo.jpg")
d$vascan<-d$references


### GBIF links
sp<-d$species
if(length(sp)){
  registerDoParallel(detectCores())
  keys<-foreach(i=sp,.packages=c("rgbif")) %dopar% {
    #sptab<-rev(sort(table(as.data.frame(occ_search(scientificName=i,limit=200)$data)$scientificName)))
    #spfull<-names(sptab)[1]
    #key<-as.data.frame(name_suggest(q=spfull)$data)$key[1]
    key<-as.data.frame(name_backbone(name=i, rank='species', kingdom='plants'))$usageKey[1]
    file.path("https://www.gbif.org/fr/species",key)
  }
  gbifurl<-data.frame(sp=sp,gbif=unlist(keys))
  d$gbif<-gbifurl$gbif[match(d$species,gbifurl$sp)]
}


### N obs
#gbif<-fread("/home/frousseu/Documents/github/floreqc/gbif/0021817-231002084531237.csv",select=c("species","eventDate","decimalLatitude",""))
gbif<-gbif[!is.na(decimalLatitude),]
gbif<-gbif[!is.na(eventDate),]
counts<-gbif[,.(nobs=.N),by=.(species)]
d<-merge(d,counts,by="species",all.x=TRUE)
d<-d[,nobs:=fifelse(is.na(nobs),0,nobs),]

d[,vernacularFR:=.(`Nom vernaculaire fr`)]
d[,vernacularFR:=capitalize(vernacularFR)]
d[,vernacularEN:=.(`Nom vernaculaire en`)]
d[,vernacularEN:=tools::toTitleCase(vernacularEN)]
d[,botanic:=.(`acceptedNameUsage`)]


### Herbier du Québec links
nomvern<-tolower(gsub(" |'","-",d$vernacularFR))
nomvern<-iconv(nomvern,to="ASCII//TRANSLIT")
d$herbierqc<-paste0("https://herbierduquebec.gouv.qc.ca/plante/",nomvern)
links<-unique(d$herbierqc)
plan(multisession,workers=8) # parallel too fast for website
ex<-lapply(links, function(i){
  print(i)
  #Sys.sleep(runif(1,0.5,2))
  url.exists(i)
})
plan(sequential)
d$herbierqc<-ifelse(unlist(ex)[match(d$herbierqc,links)],d$herbierqc,NA)

### statuts
d$status<-"Statut (introduit, indigène, etc.)"
d$protection<-"Protection (menacée, vulnérable, etc.)"
d$alternatif<-NA
d$vernacularFRalt<-NA


d[,idtaxa:=as.integer(basename(inatID))]
#d<-d[!is.na(idtaxa),]
d[,species:=d$"Nom scientifique"]
d[,nom:=vernacularFR]
ma<-match(d$species,sapply(strsplit(taxon$acceptedNameUsage," "),function(i){paste(i[1:2],collapse=" ")}))
d[,taxonomic_order:=ma]


### Some ssp have several statuses that should be combined
s<-st_read("/home/frousseu/Documents/github/flore.quebec/data/emvs_dq.gpkg") |> as.data.table()
s<-s[GROUPE=="Plantes" & GGROUPE!="Invasculaires",]
s[,species:=sapply(strsplit(SNAME," "),function(i){paste(i[1:2],collapse=" ")})]
s<-s[,c("species","LOIEMV","COSEWIC","SARASTATUS","GRANK","NRANK","SRANK"),with=FALSE] |> unique()
s<-s[species%in%d$species,]
s<-s[!duplicated(s$species),]
d<-s[d,on=.(species)]


#fwrite(d,"/home/frousseu/Documents/github/flore.quebec/data/plants2024-11-23.csv")
d<-fread("/home/frousseu/Documents/github/flore.quebec/data/plants2.csv")
#d[, inatID := ifelse(basename(inatID) == "NA", NA, inatID)]
#setdiff(dd$species, d$species)

oldsp <- setdiff(d$species, d2$species)
newsp <- setdiff(d2$species, d$species)

oldsp <- lapply(oldsp, function(i){
  ma <- match(i, taxon$sp)
  if(!is.na(ma)){
    sp <- taxon$acceptedNameUsage[ma]
    res <- taxon$sp[match(sp, taxon$scientificName)]
  } else {
    g <- grep(i, taxon$sp)
    if(any(g)){
      sp <- unique(taxon$acceptedNameUsage[g])
      res <- taxon$sp[match(sp, taxon$scientificName)]
    } else {
      res <- NA
    }
  }
  data.frame(old = i, new = res)
}) |> do.call("rbind", args = _)
  
newnewsp <- setdiff(newsp, oldsp$new)






