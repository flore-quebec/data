

latest_species_commits <- function(n = 100, species = TRUE){
  token <- readLines("/home/frousseu/.ssh/github_token")
  githubapi <- paste0("https://frousseu:",token,"@api.github.com/repos/frousseu/floreduquebecsp/commits")
  x <- fromJSON(paste0(githubapi,"?per_page=",n,"&files=true"))
  sha <- x$sha
  l <- lapply(sha, function(i){
    print(i)
    x <- fromJSON(file.path(githubapi,i))
    file <- x$files$filename
    if(is.null(file)){
      return(NULL)
    }
    author <- x$commit$author$name
    login <- x$author$login
    date <- x$commit$author$date
    message <- x$commit$message
    #x$files$patch
    res <- data.frame(sha = i, file = file, author = author, login = login, date = date, message = message)
    res <- cbind(res, x$files[,c("additions","deletions","changes")])
    res
  })
  x <- do.call("rbind", l)
  x <- x[grep("_", x$file),]
  if(species){
    unique(x$file)
  }else{
    x
  }
}
# sps <- latest_species_commits(10)



get_species_photos <- function(paths){
  res <- lapply(paths,function(s){
      print(s)
      url <- file.path("https://raw.githubusercontent.com/frousseu/floreduquebecsp/main",s)
      x <- readLines(url)
      beg <- match("<!--", x)
      end <- match("-->", x)
      x <- x[(beg + 1):(end - 1)]
      x <- gsub(" ", "", x)
      x <- x[x != ""]
      if(length(x) == 0){
        return(NULL)
      }
      l <- lapply(strsplit(x, "-"),function(i){
        if(length(i) == 1){
          i <- c(1,i)
        }
        data.frame(species = gsub(".md","",gsub("_", " ", basename(url))), idobs = as.integer(basename(i[length(i)])), no = as.integer(i[1:(length(i) - 1)]))
      })
      x <- do.call("rbind", l)
      l <- split(x, x$idobs)
      l <- l[sort(as.character(unique(x$idobs)))] # split reorders output and iNat API orders output alphabetically
      j <- fromJSON(file.path("https://api.inaturalist.org/v1/observations",paste0(names(l),collapse=",")))
      l <- lapply(seq_along(l),function(i){
        res <- j$results$photos[[i]][l[[i]]$no, c("license_code", "url", "attribution")]
        res2 <- j$results$user[i, c("login", "name")]
        cbind(l[[i]], res, res2, uuid = j$results$uuid[i], taxon.name = j$results$taxon$name[i])
      })
      info <- do.call("rbind", l)
      info <- info[order(as.integer(row.names(info))), ]
      info$reject <- ifelse(info$license_code %in% c("cc0","c(c-by","cc-by-nc") & info$species == info$taxon.name, 0, 1)
      info$rank <- 1:nrow(info)
      info$url <- gsub("square.","medium.",info$url)
      info
  })
  do.call("rbind",res)
}

# sps_photos <- get_species_photos(sps)





species_paths <- function(){
  token <- readLines("/home/frousseu/.ssh/github_token")
  url <- paste0("https://frousseu:",token,"@api.github.com/repos/frousseu/floreduquebecqc/git/trees/main?recursive=1")
  x <- fromJSON(url)
  x <- x$tree$path
  x[grep("_",x)]
}


make_species_files <- function(){
  folders <- unique(d[ , c("family", "genus", "sp")]) |>
    apply(1, paste, collapse = "/") |>
    gsub(" ", "_", x = _) |>
    paste0(".md") |>
    file.path("Espèces",x = _) |>
    sort()
  lapply(folders,function(i){
    if(!dir.exists(dirname(i))){
      dir.create(dirname(i), recursive = TRUE)
    }
    if(!file.exists(i)){
      template <- "\n<!--\n\n\n\n\n-->\n\n## Traits distinctifs\n\n-\n\n## Espèces semblables\n\n-\n\n## Habitat\n\n-\n\n## Commentaires\n\n-\n\n"
      write(template,i)
    }
  })
}
#make_species_files()



#url <- paste0("https://frousseu:",token,"@api.github.com/repos/frousseu/floreqc/contents/Espèces/")
#x <- fromJSON(url)

# count_letters_changed <- function(diff) {
#   additions <- gsub("\\+([^\\n]+)", "\\1", regmatches(diff, gregexpr("\\+([^\\n]+)", diff)))
#   deletions <- gsub("\\-([^\\n]+)", "\\1", regmatches(diff, gregexpr("\\-([^\\n]+)", diff)))
#
#   additions_text <- paste(additions, collapse = "")
#   deletions_text <- paste(deletions, collapse = "")
#
#   letters_changed <- sum(utf8ToInt(additions_text) != utf8ToInt(deletions_text))
#   return(letters_changed)
# }
#
# count_letters_changed(x$files$patch)



