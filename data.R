
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


#d<-read.table("https://data.canadensys.net/downloads/vascan/TXT-5bfd1205-26cd-4ee9-aada-af09bc88732a.txt",header=TRUE,encoding="UTF-8",sep="/t",nrows=10)

#d<-fread("https://data.canadensys.net/downloads/vascan/TXT-5bfd1205-26cd-4ee9-aada-af09bc88732a.txt",header=TRUE,encoding="UTF-8")


lf<-list.files("/home/frousseu/Documents/github/floreqc/vascan",full=TRUE,pattern=".txt")

taxon<-fread(grep("taxon",lf,value=TRUE))
spnames<-unique(taxon$scientificName)
sp<-sapply(strsplit(spnames," "),function(i){
  g<-match("var.",i)
  if(is.na(g)){
    paste(i[1:2],collapse=" ")
  }else{
    paste(i[1:4],collapse=" ")
  }
})
spnames<-data.table(scientificName=spnames,sp=sp)
taxon<-taxon[spnames,on="scientificName"]


distribution<-fread(grep("distribution",lf,value=TRUE))
distribution<-distribution[locality=="Québec",]

vernacular<-fread(grep("vernacularname",lf,value=TRUE))
vernacular<-vernacular[language=="FR" & isPreferredName,]


#d<-fread("/home/frousseu/Documents/github/floreqc/vascan.csv",header=TRUE,encoding="UTF-8")

d<-fread("/home/frousseu/Documents/github/floreqc/TXT-007361df-1554-4e21-9aa7-0271ef025e6f.txt",header=TRUE,encoding="UTF-8")

d<-d[Rang=="Espèce",]
d$species<-d$"Nom scientifique"
d<-d[-grep("×",d$species),] # removes hybrid
d[,taxonID:=as.integer(basename(URL))]

d<-d[taxon,on="taxonID", nomatch=NULL]

taxon2<-taxon[taxonRank=="species" & taxonomicStatus=="synonym",]
taxon2[,taxonID:=as.integer(acceptedNameUsageID)]
taxon2[,species_alt:=paste(genus,specificEpithet)]
taxon2<-taxon2[,.(taxonID,species_alt)]
taxon2<-taxon2[!is.na(taxonID),]

d<-d[taxon2,on="taxonID",species_alt:=i.species_alt]

#d2<-merge(d,taxon2,all.x=TRUE)

#d<-d[family%in%c("Poaceae","Cyperaceae","Juncaceae","Lamiaceae","Asteraceae"),]

d<-merge(d,distribution,all.x=TRUE,by="taxonID")
#d<-d[distribution,,on="taxonID", nomatch=NA] # bug here!!!!!!!!
#d<-d[vernacular,,on="taxonID", nomatch=NA]
d<-merge(d,vernacular,all.x=TRUE,by="taxonID")


#i<-"Equisetum pratense"
#x<-fromJSON(paste0("https://api.inaturalist.org/v1/taxa?q=/"",gsub(" ","%20",i),"/""))$results$id[1]

### gnr
sp<-d$species#[1:200]
lsp<-split(sp,ceiling(seq_along(sp)/200))
d$inatID<-unlist(lapply(lsp,function(i){
  api<-paste0("http://resolver.globalnames.org/name_resolvers.json?names=",paste(gsub(" ","+",i),collapse="|"),"&data_source_ids=147|180")
  req<-GET(api)
  json<-content(req,as="text")
  x<-fromJSON(json)$data$results
  id<-sapply(x,function(j){
    ma<-match("iNaturalist Taxonomy",j$data_source_title)
    if(is.na(ma)){
      NA
    }else{
      j$taxon_id[ma]
    }
  })
  paste0("https://www.inaturalist.org/taxa/",id)
}))

table(is.na(as.integer(basename(d$inatID))))

#d[is.na(as.integer(basename(d$inatID))),.(species,species_alt,inatID)]



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


### iNat links
inat<-basename(d$inatID)
d$inat<-ifelse(inat=="",NA,paste0("https://www.inaturalist.org/observations?subview=grid&place_id=13336&taxon_id=",inat))


### POWO links
sp<-d$species#[1:2]
powo<-get_pow_(sp,ask=FALSE,accepted=FALSE,rank_filter="species") # need to correct
#powourl<-data.frame(sp=sp,powo=attributes(powo)$uri)
powourl<-data.frame(sp=sp,powo=sapply(powo,function(i){paste0("http://powo.science.kew.org/",i$url[1])}))
d$powo<-powourl$powo[match(d$species,powourl$sp)]


### VASCAN links
im<-image_read("https://layout.canadensys.net/common/images/favicon.ico")
image_write(image_trim(im[6]),"/home/frousseu/Documents/github/floreqc/vascanlogo.jpg")
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
gbif<-fread("/home/frousseu/Documents/github/floreqc/gbif/0021817-231002084531237.csv",select=c("species","eventDate","decimalLatitude",""))
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
plan(multisession,workers=8)
ex<-future_lapply(links,url.exists)
plan(sequential)
d$herbierqc<-ifelse(unlist(ex)[match(d$herbierqc,links)],d$herbierqc,NA)

### statuts
d$status<-"Statut (introduit, indigène, etc.)"
d$protection<-"Protection (menacée, vulnérable, etc.)"
d$alternatif<-NA
d$vernacularFRalt<-NA


iders<-paste(c("frousseu","elacroix-carignan","lysandra","marc_aurele","elbourret","bickel","michael_oldham","wdvanhem","sedgequeen","hsteger","seanblaney","chasseurdeplantes","birds_bugs_botany","bachandy","paquette0747","brothernorbert","tsn","ludoleclerc","trscavo","ken_j_allison","alexandre_bergeron","johnklymko","charlie","mcusson","mhough","birddogger","ibarzabal_j","choess","m-bibittes","brucebennett","tiarelle","polemoniaceae"),collapse=",")

#iders<-paste(c("marc_aurele","frousseu","lysandra"),collapse=",")

get_random_photos<-function(id,license=c("cc0","cc-by","cc-by-nc"),iders=NULL,place=TRUE){
  cc<-paste(license,collapse=",")
  #x<-fromJSON(paste0("https://api.inaturalist.org/v1/observations?photo_license=",cc,"&taxon_id=",id,"&quality_grade=research&ident_user_id=",iders,"&order=desc&order_by=created_at"))
  api<-paste0("https://api.inaturalist.org/v1/observations?photo_license=",cc,"&taxon_id=",id,if(is.null(place)){""}else{"&place_id=13336"},if(is.null(iders)){""}else{paste0("&ident_user_id=",iders)},"&order=desc&order_by=created_at&per_page=200")
  x<-fromJSON(api)#$to
  if(x$total_results==0){
    return(NULL)
  }else{
    x<-x$results
  }
  users<-cbind(place_guess=x$place_guess,x$user[,c("login","name")])
  pics<-do.call("rbind",lapply(seq_along(x$observation_photos),function(i){
    res1<-x$observation_photos[[i]]$photo[,c("url","license_code","attribution")]
    res2<-x$observation_photos[[i]]$photo$original_dimensions[,c("width","height")]
    res<-cbind(res1,res2)
    #res<-res[which(res$width>205 & res$height>205),]
    #if(nrow(res)>0){
    res<-res[1,] # keep first one
    #}
    cbind(idobs=as.integer(x$id[i]),res,users[rep(i,nrow(res)),])
  }))
  showbobs<-paste0("https://www.inaturalist.org/observations/?id=",paste0(pics$idobs,collapse=","),"&place_id=any")
  #ids<-x$identifications[[1]][,c("taxon_id","current","user")]
  pics$url<-gsub("/square","/medium",pics$url)
  pics<-pics[which(pics$width>205 & pics$height>205),]
  pics
}


#df<-get_photos(id=169114,iders=iders,place=TRUE)[0,]

set.seed(1234)
ids<-basename(d$inatID)#[1:10]
random_photos<-lapply(seq_along(ids),function(i){
  #cat(paste(i,"/r"));flush.console()
  print(i)
  if(is.na(ids[i])){return(NULL)}
  x<-get_random_photos(id=ids[i],iders=iders,place=TRUE)
  if(!is.null(x)){x<-x[sample(1:nrow(x)),]}
  if(is.null(x) || nrow(x)<8){
    x<-get_random_photos(id=ids[i],iders=iders,place=NULL)
    if(!is.null(x)){x<-x[sample(1:nrow(x)),]}
    if(is.null(x) || nrow(x)<8){
      x<-get_random_photos(id=ids[i],iders=NULL,place=TRUE)
      if(!is.null(x)){x<-x[sample(1:nrow(x)),]}
      if(is.null(x) || nrow(x)<8){
        x<-get_random_photos(id=ids[i],iders=NULL,place=NULL)
        if(!is.null(x)){x<-x[sample(1:nrow(x)),]}
      }else{
        x<-NULL
      }
    }
  }
  if(!is.null(x)){
    pic<-x[1:min(c(8,nrow(x))),]
    Sys.sleep(0.5)
    pic$when<-as.character(Sys.time())
    pic$idtaxa<-ids[i]
    pic$selected <- 0
    pic$rank <- NA
    fwrite(pic,"random_photos.csv",append=TRUE)
  }
})
#photos<-lapply(seq_along(photos),function(i){
#  if(is.null(photos[[i]])){
#    cbind(idtaxa=ids[[i]],photos[[which(!sapply(photos,is.null))[1]]][0,][1,])
#  }else{
#    cbind(idtaxa=ids[[i]],photos[[i]])
#  }
#})
#photos<-rbindlist(photos)

d[,idtaxa:=as.integer(basename(inatID))]
#d<-d[!is.na(idtaxa),]
d[,species:=d$"Nom scientifique"]
d[,nom:=vernacularFR]
ma<-match(d$species,sapply(strsplit(taxon$acceptedNameUsage," "),function(i){paste(i[1:2],collapse=" ")}))
d[,taxonomic_order:=ma]


### Some ssp have several statuses that should be combined
s<-st_read("/home/frousseu/Documents/github/floreqc/emvs_dq.gpkg") |> as.data.table()
s<-s[GROUPE=="Plantes" & GGROUPE!="Invasculaires",]
s[,species:=sapply(strsplit(SNAME," "),function(i){paste(i[1:2],collapse=" ")})]
s<-s[,c("species","LOIEMV","COSEWIC","SARASTATUS","GRANK","NRANK","SRANK"),with=FALSE] |> unique()
s<-s[species%in%d$species,]
s<-s[!duplicated(s$species),]
d<-s[d,on=.(species)]


#fwrite(d,"/home/frousseu/Documents/github/floreqc/plants.csv")
d<-fread("/home/frousseu/Documents/github/floreqc/plants.csv")




random_photos <- fread("random_photos.csv", fill = TRUE)
random_photos$idobs <- as.integer(random_photos$idobs)
random_photos <- random_photos[!is.na(when), ]
random_photos$species <- d$species[match(random_photos$idtaxa,d$idtaxa)]


commits <- latest_species_commits(10, species = TRUE)
selected_photos <- get_species_photos(commits)
setDT(selected_photos)
#fwrite(selected_photos, "selected_photos.csv")
sphotos <- fread("selected_photos.csv")
selected_photos <- unique(rbind(selected_photos, sphotos), by = c("species", "rank"))
fwrite(selected_photos, "selected_photos.csv", append = FALSE)


selected_photos<-fread("selected_photos.csv")
selected_photos$idtaxa <- d$idtaxa[match(selected_photos$species,d$species)]


photos <- merge(selected_photos, random_photos, all = TRUE)
photos <- photos[order(species, rank), ]
photos <- split(photos, photos$species) |>
            lapply(function(i){i[1:min(c(nrow(i),8)), ]}) |>
            rbindlist()










#pics<-data.frame(id=ids,pics=photos)
#photos<-merge(photos,d,by.x="idtaxa",by.y="idtaxa",all.x=TRUE)

d <- d[!is.na(idtaxa), ] # ????????????
photos <- merge(photos, d, by.x="idtaxa", by.y="idtaxa", all=TRUE)
names(photos) <- gsub("\\.x|i\\.","",names(photos)) # ?????????
photos <- photos[order(species,rank)]
#pics<-photos[order(species),]



pics <- split(photos, photos$species) #|>
          #lapply(function(i){i[1:min(c(nrow(i),8)), ]})







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

#pics<-pics[1:2]

image_array<-function(){
  #cat("/014")
  l<-sapply(pics,function(i){
    tags<-c("src","alt","famille","genre","espèce","fna","inat","vascan","gbif","powo","herbierqc","class","ordre","nobs","vernaculaire","vernacularFRalt","vernacularEN","botanic","alternatif","status","protection","taxonomic_order","LOIEMV","COSEWIC","SARASTATUS","GRANK","NRANK","SRANK")
    tagnames<-c("url","species","family","genus","species","fna","inat","vascan","gbif","powo","herbierqc","class","order","nobs","vernacularFR","vernacularFRalt","vernacularEN","botanic","alternatif","Québec","protection","taxonomic_order","LOIEMV","COSEWIC","SARASTATUS","GRANK","NRANK","SRANK")
    info<-unlist(as.vector(i[1,..tagnames]))
    info<-unname(sapply(info,function(x){paste0("\"",x,"\"")}))
    #w<-which(photos$species==i$species[1])
    urls<-i$url[1:8]
    urls<-paste0("[ \"",paste(urls,collapse="\", \""),"\" ]")
    tags<-c(tags,"images")
    info<-c(info,urls)

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

option_values<-function(x,tag,append=TRUE){
  #cat("/014")
  values<-sort(unique(x[[tag]]))
  write(paste0("const ",paste0(tag,"_values")," = [\"",paste0(values,collapse="\", \""),"\"];"),file="data.js",append=append)
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
  #m<-lapply(values,function(i){
  #  paste("{",paste0(gsub("-|'|’| ","_",values),": [",paste(sort(unique(c(grep(i,x[["nom"]]),grep(i,x[["vernacularFR"]])))-1),collapse=", "),"]"),"}")
  #})
  #arr<-paste(unlist(m),collapse=", ")
  #write(paste("const common_names = [",arr,"];"),file="data.js",append=append)
}


option_values(rbindlist(pics),tag="family", append=FALSE)
option_values(rbindlist(pics),tag="genus")
option_values(rbindlist(pics),tag="species")
all_values(d)
image_array()
#file.show("/home/frousseu/Documents/github/floreqc/flora.html")







#x<-x[grep("QC//, Canada|Québec//, Canada",x$place_guess),]


#library(wordcloud2)

#df<-as.data.frame(table(d[d$family!="As",]$genus))
#df<-df[rev(order(df$Freq)),]
#row.names(df)<-df[,1]
#wordcloud2(data=df, size=0.75, shape="square",color='random-dark',minSize=1)












# lapply(1:100,function(i){
#   print(i)
#   readLines("https://raw.githubusercontent.com/frousseu/floreqc/main/README.md")
# })
