
library(data.table)
library(magick)
library(sf)
library(terra)
library(rmapshaper)
library(geodata)
library(magick)
library(berryFunctions)


###
gbif<-fread("/home/frousseu/Documents/Github/floreqc/gbif/0021817-231002084531237.csv")
gbif[,date:=eventDate]
gbif[,jul:=as.integer(format(as.Date(date),"%j"))]
gbif[,year:=substr(date,1,4)]

gbif[,speciesgbif:=species]
spnames<-unique(gbif$scientificName)
sp<-sapply(strsplit(spnames," "),function(i){
  g<-match("var.",i)
  if(is.na(g)){
    paste(i[1:2],collapse=" ")
  }else{
    paste(i[1:4],collapse=" ")
  }
})
spnames<-data.table(scientificName=spnames,sp=sp)
gbif<-gbif[spnames,on="scientificName"]
#gbif[,species:=sp]

gbif[,species:=d$species[match(taxon$acceptedNameUsageID[match(gbif$sp,taxon$sp)],d$taxonID)],]
gbif[is.na(species),species:=speciesgbif]

#setdiff(d$species,gbif$species)
#table(gbif$species[grep("Adiantum",gbif$species)])
#table(d$species[grep("Adiantum",d$species)])
#unique(gbif[grep("Elymus",gbif$sp),c("speciesgbif","sp","species"),with=TRUE])


dates<-format(as.Date(1:365,origin="1970-01-01")-1,"%Y-%m-%d")
brks<-as.integer(format(as.Date(dates[c(which(substr(dates,9,10)%in%c("01")),365)]),"%j"))
brks<-sort(c(brks,brks[-length(brks)]+diff(brks)/2))


spnames<-unique(gbif$scientificName)
alt_names<-sapply(strsplit(spnames," "),function(i){
  g<-match("var.",i)
  if(is.na(g)){
    paste(i[1:2],collapse=" ")
  }else{
    paste(i[1:4],collapse=" ")
  }
})




sp<-unique(d$species[d$species%in%gbif$species])#[1]

for(i in sp){
  print(i)
  x<-gbif[species==i,"jul",]
  if(nrow(x)==0){next}
  h<-hist(x$jul[x$jul<366],breaks=brks,plot=FALSE)
  #plot(density(d$jul,na.rm=TRUE))
  pheno<-density(gbif$jul,na.rm=TRUE,adjust=3,from=1,to=365)
  #h<-lapply(h,"[",c(5:25))
  labs<-gsub("\\.","",format(as.Date(h$mids,origin="1970-01-01"),"%b"))

  path<-paste0("/home/frousseu/Documents/Github/floreqc/images/",gsub(" ","_",i),"_pheno.png")
  png(path,units="cm",width=9,height=6,res=300)
  par(mar=c(3,2,3,0.5),bg="white")

  plot(0,0,xlim=c(60,335),ylim=c(0,max(c(5,h$counts))*1.01),type="n",xaxt="n",yaxt="n",xaxs = 'i',yaxs = 'i',axes=FALSE)
  abline(v=brks,lty=3,col="grey80")
  abline(h=pretty(c(h$counts,5)),lty=3,col="grey80")
  invisible(lapply(seq_along(h$counts),function(i){
    offset<-0.75
    roundedRect(xleft=brks[i]+offset,xright=brks[i+1]-offset,ybottom=0,ytop=h$counts[i],col="forestgreen",border=NA,corners=c(2,3),rounding=0.0,bothsame=FALSE,devcorrect=TRUE)
  }))
  alongx<-pheno$x
  alongy<-(pheno$y/max(pheno$y))*max(c(h$counts,5))
  polygon(c(alongx,rev(alongx)),c(alongy,rep(0,length(alongy))),col=adjustcolor("darkgreen",0.15),border=NA)
  lines(alongx,alongy,col=adjustcolor("darkgreen",0.35))
  #invisible(lapply(seq_along(h$counts),function(i){
  #  offset<-0.75
  #  roundedRect(xleft=brks[i]+offset,xright=brks[i+1]-offset,ybottom=0,ytop=h$counts[i],col=adjustcolor("forestgreen",0.25),border=NA,corners=c(2,3),rounding=0.15)
  #}))
  months<-brks[seq(2,length(brks),by=2)]
  minus<--c(1,2,length(months))
  months<-months[minus]
  monthlims<-brks[-seq(2,length(brks),by=2)]
  monthlims<-monthlims[-c(1:2)]
  text(months,rep(0,length(months)),label=unique(labs)[minus],srt=0,adj=c(0.5,1.5),cex=0.65,xpd=TRUE)
  axis(1,at=monthlims,labels=rep("",length(monthlims)),tcl=-0.7,lwd=0,lwd.ticks=1.0,col.ticks="grey80",xpd=TRUE)
  mtext(side=2,line=1,font=2,text="Nb d'observations",cex=0.75)
  axis(2,at=pretty(c(h$counts,5))[-1],mgp=c(2,0.05,0),tcl=0,las=2,col="grey50",cex.axis=0.5,lwd=0)
  legend("top",legend=c("Observations","Toutes les espèces"),ncol=2,pch=22,pt.cex=1.25,col=c("forestgreen",adjustcolor("darkgreen",0.35)),pt.bg=c("forestgreen",adjustcolor("darkgreen",0.15)),cex=0.65,bty="n",inset=c(0.05,-0.20),xpd=TRUE,pt.lwd=1.25)

  dev.off()
  image_read(path) |> image_trim() |> image_border("10x10",color="white") |> image_write(path)
}
file.show(list.files(dirname(path),full=TRUE)[1])

