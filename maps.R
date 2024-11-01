
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

#un <- c("species","verbatimScientificName","scientificName")
#keep <- c("family", "genus", un)
#gbif[genus %in% c("Elymus"), .(.N), by = keep][order(family, genus, species, verbatimScientificName), ] |> View()
# split by species/scientificName and find all matches     
     
d <- fread("/home/frousseu/Documents/github/flore.quebec/data/plants2.csv")

gbif <- fread("/home/frousseu/Documents/github/flore.quebec/data/gbif/0039190-240321170329656.csv")
other <- sapply(strsplit(gbif$scientificName, " "), \(i) {paste(i[1:2], collapse = " ")})
gbif[ , other := other]
gbif[, origspecies := species]

###






nomatch <- is.na(match(d$species, gbif$species)) & is.na(match(d$species_alt, gbif$species)) & is.na(match(d$species, gbif$other)) & is.na(match(d$species_alt, gbif$other))
table(nomatch)
d$species[nomatch]

gbif[, vascan := NA]
ma <- match(gbif$other, d$species)
gbif[, vascan := ifelse(is.na(vascan) & !is.na(ma), d$species[ma], vascan)]
ma <- match(gbif$species, d$species)
gbif[, vascan := ifelse(is.na(vascan) & !is.na(ma), d$species[ma], vascan)]
ma <- match(gbif$other, d$species_alt)
gbif[, vascan := ifelse(is.na(vascan) & !is.na(ma), d$species[ma], vascan)]
ma <- match(gbif$species, d$species_alt)
gbif[, vascan := ifelse(is.na(vascan) & !is.na(ma), d$species[ma], vascan)]

nomatch <- is.na(match(d$species, gbif$vascan))
d$species[nomatch]



#### testing
#x <- gbif[gbif$genus %in% "Eragrostis" & gbif$species != "", .(species, decimalLongitude, decimalLatitude)] |>
#       as.data.frame() |>
#       st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
#x <- split(x, x$species)

#path <- "/home/frousseu/Documents/github/flore.quebec/data/temp"
#lapply(names(x), function(i) {
#  filename <- paste0(gsub(" ","_",i),".geojson")
#  st_write(x[[i]], file.path(path, filename), append = FALSE)  
#  #cmd <- paste("tippecanoe -o", gsub(".geojson", ".pmtiles", file.path(path, filename)),"--force -r1 --cluster-distance=5", file.path(path, filename))
#  cmd <- paste("tippecanoe -o", gsub(".geojson", ".pmtiles", file.path(path, filename)),"--force -B 0", file.path(path, filename))
#  system(cmd)
#  unlink(file.path(path, filename))
#}) |> invisible()





# inatnames <- gbif[institutionCode == "iNaturalist", .(species, infraspecificEpithet, scientificName, occurrenceID)] 
# inatnames <- inatnames[!duplicated(scientificName),]
# inatnames[, id := as.integer(basename(occurrenceID))]
# ids <- split(inatnames$id, ceiling(1:nrow(inatnames)/30))
# 
# res <- lapply(ids, function(i){
#   Sys.sleep(1)
#   x <- fromJSON(paste0("https://api.inaturalist.org/v1/observations/", paste(i, collapse = ",")))
#   data.table(id = x$results$id, nameinat = x$results$community_taxon$name)
# })
# res <- rbindlist(res)
# inatnames <- merge(inatnames, res, all.x = TRUE)




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

gbif[, species := d$species[match(taxon$acceptedNameUsageID[match(gbif$sp,taxon$sp)],d$taxonID)],]
gbif[is.na(species),species := speciesgbif]

gbif[, longitude := decimalLongitude]
gbif[, latitude := decimalLatitude]
gbif[, coordinateUncertaintyInMeters := ifelse(is.na(coordinateUncertaintyInMeters), "nd", coordinateUncertaintyInMeters)]

# table(gbif$datasetKey)
datasets <- list(
 'c4a5efee-4c61-45d8-bd7a-6275b2388308' = 'Herbier du Québec (QUE) - Collection de plantes vasculaires',
 '50c9509d-22c7-4a22-a47d-8c48425ef4a7' = 'iNaturalist - Research-grade Observations',
 '0348540a-e644-4496-89d3-c257da9ad776' = 'Marie-Victorin Herbarium (MT) - Plantes vasculaires',
 '2fd02649-fc08-4957-9ac5-2830e072c097' = 'Herbier Louis-Marie (QFA) - Collection de plantes vasculaires'
)

gbif[, datasetName := unname(unlist(datasets[match(gbif$datasetKey, names(datasets))]))]
gbif[, link := ifelse(datasetName == 'iNaturalist - Research-grade Observations', occurrenceID, paste0("https://www.gbif.org/fr/occurrence/", gbifID))]

gbif<-st_as_sf(gbif,coords=c("decimalLongitude","decimalLatitude"),crs=4326)
gbif<-st_transform(gbif,32618)

can<-gadm("CAN",path="/home/frousseu/Documents/github/flore.quebec/data/qc.gpkg") |> st_as_sf()

# Downloads polygons using package geodata
#can<-gadm("CAN",level=1,path=getwd()) |> st_as_sf()
can<-st_transform(can,32618)

# keep Québec and bordering provinces/states as a buffer
region<-can[can$NAME_1%in%c("Québec"),]

# split NF into different polygons
labrador<-ms_explode(can[can$NAME_1%in%c("Newfoundland and Labrador"),])
labrador<-labrador[which.max(st_area(labrador)),] # keep Labarador
#region<-rbind(region,labrador)

# Add it to the study region
#region<-rbind(region,labrador)

# Simplify polygons to make things faster
region<-ms_simplify(region,0.05)
region<-st_union(region) |> st_as_sf()

# lakes
lakes<-ne_download(scale="large",type="lakes",destdir=getwd(),category="physical",returnclass="sf") |> st_transform(32618)
#lakes<-ne_download(scale="large",type="rivers_lake_centerlines",destdir=getwd(),category="physical",returnclass="sf") |> st_transform(32618)
lakes<-st_filter(lakes,region)
lakes<-ms_simplify(lakes,0.15)
lakes<-st_intersection(lakes,region)

lim<-c(min(st_bbox(region)[c(2,4)]),5700000)

bbox<-st_bbox(region)
bbox[4]<-5700000

south<-st_crop(st_intersection(region,lakes),bbox)
buff<-st_sym_difference(st_as_sf(st_as_sfc(st_bbox(st_buffer(region,100000)))),st_buffer(region,100000))
#buffl<-st_buffer(region,150000)
sp<-unique(d$species[d$species%in%gbif$species])
#sp<-"Anthoxanthum nitens"



cl<-makeCluster(8)
registerDoParallel(cl)
foreach(i=sp,.packages=c("sf","terra","magick")) %dopar% {
#plan(multisession,workers=14)
#l<-future_lapply(sp,function(i){
  print(i)

  plotQC<-function(region,ylim=NULL,cex=0.55){ # plotting function for the study area
    par(mar=c(0,0,0,0),bg="white")
    #plot(st_geometry(qc),col="grey99",border=NA)
    plot(st_geometry(region),lwd=0.1,col="grey85",border="grey60",ylim=ylim)
    plot(st_geometry(lakes),col="grey99",border="grey75",lwd=0.05,add=TRUE)
    plot(st_geometry(x),pch=21,bg=adjustcolor("#3FA34D",0.95),col=adjustcolor("black",0.95),lwd=0.1,cex=cex,add=TRUE)
  }

  x<-gbif[gbif$species==i,]
  path<-paste0("/home/frousseu/Documents/github/flore.quebec/data/images/",gsub(" ","_",i),"_map.png")
  png(gsub("_map","_map1",path),units="cm",width=10,height=5,res=500)
  plotQC(region,ylim=lim)
  dev.off()
  image_read(gsub("_map","_map1",path)) |> image_trim() |> image_border("x50",color="white") |> image_write(gsub("_map","_map1",path))
  #file.show("C:/Users/God/Documents/floreqc/qcfloremap1.png")

  png(gsub("_map","_map2",path),units="cm",width=10,height=5,res=500)
  plotQC(region,ylim=NULL,cex=0.85)
  dev.off()
  image_read(gsub("_map","_map2",path)) |> image_trim() |> image_write(gsub("_map","_map2",path))

  im1<-image_read(gsub("_map","_map1",path))
  im2<-image_read(gsub("_map","_map2",path))
  im<-image_composite(im1,image_scale(im2,"x420"),offset="+100+0",gravity="southeast")
  im |> image_trim() |> image_scale("x800") |> image_write(path)
  # image_quantize(max = 100, dither = TRUE) 

  png(gsub("_map","_minimap",path),units="cm",width=3,height=1.5,res=500)
  par(mar=c(0,0,0,0))
  plot(st_geometry(region),lwd=0.1,col="grey80",border=NA)
  plot(buff,col="red",border=NA,add=TRUE)
  #plot(buffl,border="forestgreen",lwd=0.5,add=TRUE)
  plot(st_geometry(region),lwd=0.1,col=NA,border="grey65",add=TRUE)
  plot(st_geometry(lakes),col="grey99",border="grey75",lwd=0.05,add=TRUE)
  plot(st_geometry(x),pch=21,bg=adjustcolor("#3FA34D",0.95),col=adjustcolor("black",0.9),lwd=0.1,cex=0.4,add=TRUE)
  dev.off()
  image_read(gsub("_map","_minimap",path)) |> image_transparent(color="red",fuzz=5) |> image_trim() |> image_write(gsub("_map","_minimap",path))
  keep <- c("longitude","latitude","datasetName","scientificName","eventDate","recordedBy","identifiedBy","coordinateUncertaintyInMeters", "link")
  
  xpmtiles <- st_transform(x[, keep], 4326)
  #rename <- c("Longitude", "Latitude", "datasetName", "Taxon", "Observateur(s)", "Date", "Précision", "link", "geometry")
  #names(xpmtiles) <- rename
  
  pathpmtiles <- "/home/frousseu/Documents/github/flore.quebec/data/temp"
  filename <- gsub("_map\\.png", "\\.geojson", basename(path)) #paste0(gsub(" ","_",i),".geojson")
  st_write(xpmtiles, file.path(pathpmtiles, filename), append = FALSE)  
  #cmd <- paste("tippecanoe -o", gsub(".geojson", ".pmtiles", file.path(path, filename)),"--force -r1 --cluster-distance=5", file.path(path, filename))
  cmd <- paste("tippecanoe -o", gsub(".geojson", ".pmtiles", file.path(pathpmtiles, filename)),"--force -B 0", file.path(pathpmtiles, filename))
  system(cmd)
  unlink(file.path(pathpmtiles, filename))
  
}
stopCluster(cl)
system("rm /home/frousseu/Documents/github/flore.quebec/data/images/*map1.png")
system("rm /home/frousseu/Documents/github/flore.quebec/data/images/*map2.png")
#plan(sequential)
#file.show(list.files(dirname(path),full=TRUE,pattern="_map.png")[1])


#paste("s3cmd del s3://flore-quebec/*.pmtiles") |> system()
paste("s3cmd put /home/frousseu/Documents/github/flore.quebec/data/temp/*.pmtiles s3://flore-quebec --acl-public") |> system()





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


#cols<-colo.scale(1:10,c("gold2","gold3","palegreen","yellowgreen","forestgreen","darkgreen","darkseagreen","navajowhite3","grey80"))
cols<-colo.scale(1:10,c("gold2","gold3","palegreen","yellowgreen","forestgreen","darkgreen",colo.scale(1:3,c("darkseagreen","grey80"))))
#cols<-colo.scale(1:10,c("darkgreen","forestgreen","grey80"))
cols <- adjustcolor(cols,0.75)

png("/home/frousseu/Downloads/biomap.png", height = 22, width = 22, res = 100, units = "in")
par(mar=c(0,0,0,0))
plot(st_geometry(bio),col=cols,border=NA)
plot(st_transform(st_geometry(lakes),st_crs(bio)),col="grey99",border="grey75",lwd=0.05,add=TRUE)
plot(st_geometry(st_transform(gbif[gbif$species=="Elymus virginicus",],st_crs(bio))),cex=3,pch=21,col="black",lwd=4,bg=adjustcolor("brown",0.5),add=TRUE)
plot(st_geometry(bio),border="black",lwd=0.1,add=TRUE)
dev.off()
system("xdg-open /home/frousseu/Downloads/biomap.png")


