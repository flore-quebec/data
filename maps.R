
library(geodata)
library(sf)
library(rmapshaper)
library(rnaturalearth)
library(magick)
library(foreach)
library(doParallel)
library(data.table)
library(future)
library(future.apply)

d<-fread("/home/frousseu/Documents/github/floreqc/plants.csv")

gbif<-fread("/home/frousseu/Documents/github/floreqc/gbif/0021817-231002084531237.csv")

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

gbif<-st_as_sf(gbif,coords=c("decimalLongitude","decimalLatitude"),crs=4326)
gbif<-st_transform(gbif,32618)

can<-gadm("CAN",path="/home/frousseu/Documents/github/floreqc/qc.gpkg") |> st_as_sf()

# Downloads polygons using package geodata
#can<-gadm("CAN",level=1,path=getwd()) |> st_as_sf()
can<-st_transform(can,32618)

# keep Québec and bordering provinces/states as a buffer
region<-can[can$NAME_1%in%c("Québec"),]

# split NF into different polygons
labrador<-ms_explode(can[can$NAME_1%in%c("Newfoundland and Labrador"),])
labrador<-labrador[which.max(st_area(labrador)),] # keep Labarador
region<-rbind(region,labrador)

# Add it to the study region
region<-rbind(region,labrador)

# Simplify polygons to make things faster
region<-ms_simplify(region,0.02)
region<-st_union(region) |> st_as_sf()

# lakes
lakes<-ne_download(scale="large",type="lakes",destdir=getwd(),category="physical",returnclass="sf") |> st_transform(32618)
#lakes<-ne_download(scale="large",type="rivers_lake_centerlines",destdir=getwd(),category="physical",returnclass="sf") |> st_transform(32618)
lakes<-st_filter(lakes,region)
lakes<-ms_simplify(lakes,0.05)
lakes<-st_intersection(lakes,region)

lim<-c(min(st_bbox(region)[c(2,4)]),5700000)

bbox<-st_bbox(region)
bbox[4]<-5700000

south<-st_crop(st_intersection(region,lakes),bbox)
buff<-st_sym_difference(st_as_sf(st_as_sfc(st_bbox(st_buffer(region,150000)))),st_buffer(region,150000))
buffl<-st_buffer(region,150000)
sp<-unique(d$species[d$species%in%gbif$species])
#sp<-"Anthoxanthum nitens"

cl<-makeCluster(14)
registerDoParallel(cl)
foreach(i=sp,.packages=c("sf","terra","magick")) %dopar% {
#plan(multisession,workers=14)
#l<-future_lapply(sp,function(i){
  print(i)

  plotQC<-function(region,ylim=NULL,cex=0.25){ # plotting function for the study area
    par(mar=c(0,0,0,0),bg="white")
    #plot(st_geometry(qc),col="grey99",border=NA)
    plot(st_geometry(region),lwd=0.1,col="grey85",border="grey60",ylim=ylim)
    plot(st_geometry(lakes),col="grey99",border="grey75",lwd=0.05,add=TRUE)
    plot(st_geometry(x),pch=21,bg=adjustcolor("forestgreen",0.5),col=adjustcolor("forestgreen",0.9),lwd=0.3,cex=cex,add=TRUE)
  }


  x<-gbif[gbif$species==i,]
  path<-paste0("/home/frousseu/Documents/Github/floreqc/images/",gsub(" ","_",i),"_map.png")
  png(gsub("_map","_map1",path),units="cm",width=3,height=1.5,res=500)
  plotQC(region,ylim=lim)
  dev.off()
  image_read(gsub("_map","_map1",path)) |> image_trim() |> image_border("x20",color="white") |> image_write(gsub("_map","_map1",path))
  #file.show("C:/Users/God/Documents/floreqc/qcfloremap1.png")

  png(gsub("_map","_map2",path),units="cm",width=3,height=1.5,res=500)
  plotQC(region,ylim=NULL,cex=0.3)
  dev.off()
  image_read(gsub("_map","_map2",path)) |> image_trim() |> image_write(gsub("_map","_map2",path))

  im1<-image_read(gsub("_map","_map1",path))
  im2<-image_read(gsub("_map","_map2",path))
  im<-image_composite(im1,image_scale(im2,"x140"),offset="+10+0",gravity="southeast")
  im |> image_trim() |> image_write(path)

  png(gsub("_map","_minimap",path),units="cm",width=3,height=1.5,res=500)
  par(mar=c(0,0,0,0))
  plot(st_geometry(region),lwd=0.1,col="grey80",border=NA)
  plot(buff,col="red",border=NA,add=TRUE)
  #plot(buffl,border="forestgreen",lwd=0.5,add=TRUE)
  plot(st_geometry(region),lwd=0.1,col=NA,border="grey65",add=TRUE)
  plot(st_geometry(lakes),col="grey99",border="grey75",lwd=0.05,add=TRUE)
  plot(st_geometry(x),pch=21,bg=adjustcolor("forestgreen",0.75),col=adjustcolor("forestgreen",0.9),lwd=0.3,cex=0.35,add=TRUE)
  dev.off()
  image_read(gsub("_map","_minimap",path)) |> image_transparent(color="red",fuzz=5) |> image_trim() |> image_write(gsub("_map","_minimap",path))

}
stopCluster(cl)
#plan(sequential)
file.show(list.files(dirname(path),full=TRUE,pattern="_map.png")[1])



herbier<-image_read("/home/frousseu/Documents/Github/floreqc/herbierqc.jpg")

plot(herbier)

png("/home/frousseu/Documents/Github/floreqc/overlayqc.png",res=300,units="in",width=3,height=3)
par(mar=c(0,0,0,0),bg="transparent")
plot(st_geometry(region),lwd=0.1,col="#ffffff",border=NA)
plot(st_geometry(lakes),col="#ffffffff",border=NA,lwd=0.05,add=TRUE)
dev.off()

qc<-image_read("/home/frousseu/Documents/Github/floreqc/overlayqc.png")
qc<-image_scale(qc,image_info(herbier)$height)

im<-image_composite(herbier,qc,operator="blend",compose_args="90")
image_write(im,"/home/frousseu/Documents/Github/floreqc/herbier.png")


library(sf)
library(rmapshaper)
library(FRutils)

l <- st_layers("/home/frousseu/Downloads/CLASSI_ECO_QC_GDB/CLASSI_ECO_QC.gdb")
bio <- st_read("/home/frousseu/Downloads/CLASSI_ECO_QC_GDB/CLASSI_ECO_QC.gdb", layer="N3_DOM_BIO")
bio <- ms_simplify(bio,0.001)


cols<-colo.scale(1:10,c("gold2","gold3","palegreen","yellowgreen","forestgreen","darkgreen","darkseagreen","navajowhite4","grey80"))
cols <- adjustcolor(cols,0.65)

par(mar=c(0,0,0,0))
plot(st_geometry(bio),col=cols,border=NA)
plot(st_transform(st_geometry(lakes),st_crs(bio)),col="grey99",border="grey75",lwd=0.05,add=TRUE)
plot(st_geometry(st_transform(gbif[gbif$species=="Elymus virginicus",],st_crs(bio))),cex=3,pch=21,col="black",lwd=4,bg=adjustcolor("deepskyblue",0.5),add=TRUE)
plot(st_geometry(bio),border="black",lwd=0.1,add=TRUE)


