

source("/home/frousseu/Documents/github/flore.quebec/data/functions.R")

d<-fread("/home/frousseu/Documents/github/floreqc/plants2.csv")
random_photos <- fread("random_photos.csv", fill = TRUE)


iders<-paste(c("frousseu","elacroix-carignan","lysandra","marc_aurele","elbourret","bickel","michael_oldham","wdvanhem","sedgequeen","hsteger","seanblaney","chasseurdeplantes","birds_bugs_botany","bachandy","paquette0747","brothernorbert","tsn","ludoleclerc","trscavo","ken_j_allison","alexandre_bergeron","johnklymko","charlie","mcusson","mhough","birddogger","ibarzabal_j","choess","m-bibittes","brucebennett","tiarelle","polemoniaceae"),collapse=",")

#iders<-paste(c("marc_aurele","frousseu","lysandra"),collapse=",")

#df<-get_photos(id=169114,iders=iders,place=TRUE)[0,]

set.seed(1234)
ids<-basename(d$inatID)
ids<-basename(d$inatID[!d$idtaxa %in% random_photos$idtaxa])
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
    Sys.sleep(1)
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