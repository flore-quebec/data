
token <- readLines("/home/frousseu/.ssh/github_token")
remsha <- c("0fe221835f4c22857bb9443434bdaedef7fb6584", "3cda8bf851afdde774cea6b32ae8d0f07a262953")

latest_species_commits <- function(n = 100, species = TRUE){
  token <- readLines("/home/frousseu/.ssh/github_token")
  githubapi <- paste0("https://frousseu:",token,"@api.github.com/repos/flore-quebec/species/commits")
  x <- fromJSON(paste0(githubapi,"?path=Esp%C3%A8ces&per_page=",n,"&files=true"))
  sha <- x$sha
  #get_all_local_commits(file)
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
    name <- author
    
    w <- which(author == login)
    if(any(w)){
      replace <- sapply(w, function(i){
        a <- unique(author[which(login == author[i])])
        a[which(a != author[i])[1]]
      })
      name[w] <- replace
    }
    ### with too few commits the name cannot be inferred with the author
    res <- data.frame(sha = i, file = file, author = author, login = login, name = name, date = date, message = message)
    res <- cbind(res, x$files[,c("additions","deletions","changes")])
    res
  })
  x <- do.call("rbind", l)
  x <- x[grep("_", x$file),]
  rem <- c("0fe221835f4c22857bb9443434bdaedef7fb6584", "3cda8bf851afdde774cea6b32ae8d0f07a262953", "13d610c3ef114bd3b9e0d872a020c2763fac2c00")
  x <- x[!(x$sha %in% rem), ] # remove .md inits
  
  g <- grep("Merge pull request|Merge branch", x$message)
  if(any(g)){ # do not count merges for changes or species contributions
    x <- x[-g, ]
  } 
  if(species){
    unique(x$file)
  }else{
    x
  }
}
# sps <- latest_species_commits(10, species = TRUE)

get_sha <- function(sha, token){
  #sha <- sha[!sha %in% remsha]
  githubapi <- paste0("https://frousseu:",token,"@api.github.com/repos/flore-quebec/species/commits")
  l <- lapply(sha, function(i){
    print(i)
    req<-GET(file.path(githubapi,i), add_headers("link")) # get nb of pages
    header <- req$all_headers[[1]]$headers$link
    if(!is.null(header)){
      pn <- gregexpr("page=([0-9]+)", header) |>
        regmatches(header, m = _) |>
        _[[1]] |>
        sub("page=", "", x = _) |>
        as.integer()
    } else {
      pn <- 1
    }
    lx <- lapply(1:max(pn), function(j){ # page through pages
      x <- fromJSON(file.path(githubapi,paste0(i, "?page=", j)))
      file <- x$files$filename
      if(is.null(file)){
        return(NULL)
      }
      author <- x$commit$author$name
      login <- x$author$login
      date <- x$commit$author$date
      message <- x$commit$message
      name <- author
      w <- which(author == login)
      if(any(w)){
        replace <- sapply(w, function(i){
          a <- unique(author[which(login == author[i])])
          a[which(a != author[i])[1]]
        })
        name[w] <- replace
      }
      ### with too few commits the name cannot be inferred with the author
      res <- data.frame(sha = i, file = file, author = author, login = login, name = name, date = date, message = message)
      res <- cbind(res, x$files[,c("additions","deletions","changes")])
      res
    })
    res <- do.call("rbind", lx)
    res
  })
  x <- do.call("rbind", l)
  x
}

#co <- get_sha(sha = sha, token = token)


















#spfiles <- latest_species_commits(10, species = TRUE)
#ll <- lapply(spfiles, get_all_local_commits)
#ll <- ll[sapply(ll, length) > 0] |> unlist() |> unique()


#token <- readLines("/home/frousseu/.ssh/github_token")
#githubapi <- paste0("https://frousseu:",token,"@api.github.com/repos/flore-quebec/species/commits")
#x <- fromJSON(paste0(githubapi,"?path=Esp%C3%A8ces&per_page=",n,"&files=true"))
#sha <- x$sha

#n <- 20
#githubapi <- paste0("https://frousseu:",token,"@api.github.com/repos/flore-quebec/species/commits?path=Esp%C3%A8ces/Acanthaceae/Justicia/Test_test2.md&per_page=",n,"&files=true&rename_history=true")
#x <- fromJSON(githubapi)
#x$sha

# git log --pretty=oneline --follow Esp%C3%A8ces/Acanthaceae/Justicia/Test_test.md

#https://github.com/flore-quebec/species/commits/41d1aef989a87e0d8db884b523581c6df58772c1/Esp%C3%A8ces/Acanthaceae/Justicia/Test_test.md?browsing_rename_history=true&new_path=Esp%C3%A8ces/Acanthaceae/Justicia/Test_test2.md&original_branch=main



get_all_local_commits <- function(file){
  #out <- system(sprintf("git log --pretty=oneline --name-status --follow %s", file), intern = TRUE)
  out <- system(sprintf("git log --pretty=oneline --follow %s", file), intern = TRUE)
  strsplit(out, " ") |>
    sapply("[", 1)
}
#file <- "Espèces/Acanthaceae/Justicia/Test_test2.md"
#lf <- list.files("Espèces", pattern = ".md", full = TRUE, recursive = TRUE)
#lc <- lapply(lf, function(i){
  #print(i)
#  get_all_local_commits(i)
#}) |> unlist() |> unique()

#co <- get_sha(sha = lc, token = token)


#sha <- get_all_local_commits(file)

#old_commits <- fread("/home/frousseu/Documents/github/flore.quebec/data/commits.csv", colClasses = "character") # bug with date formats
#nums <- c("additions","deletions","changes")
#old_commits[, (nums) := lapply(.SD, as.integer), .SDcols = nums]


#out <- system(sprintf("git log --pretty=oneline --name-status --follow %s", file), intern = TRUE)
#strsplit(out, "\t") |>
#  sapply("[", 1)







get_species_photos <- function(paths){
  res <- lapply(paths,function(s){
      print(s)
      url <- file.path("https://raw.githubusercontent.com/flore-quebec/species/main",s)
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
  url <- paste0("https://frousseu:",token,"@api.github.com/repos/flore-quebec/species/git/trees/main?recursive=1")
  x <- fromJSON(url)
  x <- x$tree$path
  x[grep("_",x)]
}
#x <- species_paths()


make_species_files <- function(){
  folders <- unique(d[ , c("family", "genus", "species")]) |>
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

#x <- commits

list_contributions <- function(x){
  x$name <- x$author
  w <- which(x$author == x$login)
  if(any(w)){
    replace <- sapply(w, function(i){
      a <- unique(x$author[which(x$login == x$author[i])])
      a[which(a != x$author[i])[1]]
    })
    x$name[w] <- replace
  }
  x <- x[x$date > "2024-01-28T18:17:06Z", ] # removes the init files
  g <- grep("Merge pull request|Merge branch", x$message)
  if(any(g)){
    x <- x[-g, ]
  }
  l <- lapply(split(x, x$file), function(i){
    res <- unique(i$name) # matches login first cause author can change
    initiated <- res[1]
    if(length(res) == 1){
      contribution <- paste0("Initié par ",res,".")
      edited <- ""
    } else {
      contribution <- paste0(paste("Initié par", res[1]), paste(" et modifié par", paste(res[-1], collapse=", ") ),".")
      edited <- paste(res[-1], collapse=", ")
    }
    data.frame(contribution = contribution, initiated = initiated, edited = edited)
  })
  names(l) <- basename(names(l)) |> gsub("_", " ", x = _) |> gsub(".md", "", x = _)
  data.frame(species = names(l), do.call("rbind", l))
}


# returns a vector of inat ids
translate2inat <- function(sp){ # make sure order returned corresponds to sp order
  lsp<-split(sp,ceiling(seq_along(sp)/200))
  unlist(lapply(seq_along(lsp),function(j){
    i <- lsp[[j]]
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
    ifelse(is.na(id), NA, paste0("https://www.inaturalist.org/taxa/",id))
  }))
}



#api <- "https://verifier.globalnames.org/api/v1/verifications/Mutarda%20nigra|Carex%20lurida?data_sources=147%7C180&all_matches=true&capitalize=false&species_group=false&fuzzy_uninomial=false&stats=false&main_taxon_threshold=0.5"

# returns a vector of inat ids
translate2inat2 <- function(sp){ # make sure order returned corresponds to sp order
  lsp<-split(sp,ceiling(seq_along(sp)/200))
  unlist(lapply(seq_along(lsp),function(j){
    #print(j)
    i <- lsp[[j]]
    api<-paste0("https://verifier.globalnames.org/api/v1/verifications/",paste(gsub(" ","%20",i),collapse="|"),"?data_sources=147%7C180%7C167&all_matches=true&capitalize=false&species_group=false&fuzzy_uninomial=false&stats=false&main_taxon_threshold=0.5")
    req<-GET(api)
    json<-content(req,as="text", encoding = "UTF-8")
    x<-fromJSON(json)$names$results
    inatid<-sapply(x,function(j){
      ma<-match("iNaturalist",j$dataSourceTitleShort)
      if(is.na(ma)){
        NA
      }else{
        j$recordId[ma]
      }
    })
    ifelse(is.na(inatid), NA, paste0("https://www.inaturalist.org/taxa/",inatid))
  }))
}




get_random_photos<-function(id,license=c("cc0","cc-by","cc-by-nc"),iders=NULL,place=TRUE){
  cc<-paste(license,collapse=",")
  #x<-fromJSON(paste0("https://api.inaturalist.org/v1/observations?photo_license=",cc,"&taxon_id=",id,"&quality_grade=research&ident_user_id=",iders,"&order=desc&order_by=created_at"))
  api<-paste0("https://api.inaturalist.org/v1/observations?photo_license=",cc,"&taxon_id=",id,if(is.null(place)){""}else{"&place_id=13336"},if(is.null(iders)){""}else{paste0("&ident_user_id=",iders)},"&order=desc&order_by=created_at&per_page=200")
  print(api)
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

# GitHub API base URL
#github_api_url <- "https://api.github.com"

# Define your repository, file, and owner
#owner <- "flore-quebec"  # GitHub username or organization
#repo <- "species"  # Repository name
#file_path <- file  # Path to the file within the repository

# GitHub personal access token (optional, but recommended for rate limiting)
#token <- "your_personal_access_token"  # Optional, if you want to use token for higher rate limits

# Construct the API URL for the commit history of the file
#url <- sprintf("%s/repos/%s/%s/commits?path=%s", github_api_url, owner, repo, file_path)
#url <- "https://frousseu:secret@api.github.com/repos/flore-quebec/species/commits?path=Esp%C3%A8ces&per_page=300&files=true"
# If you have a token, use it in the request for authentication
#response <- GET(url, add_headers(Authorization = paste("token", token)))

#strsplit(out[seq(1, length(out), by = 2)], " ", fixed = FALSE)[[1]]
#paste(out[seq(1, length(out), by = 2)], out[seq(2, length(out), by = 2)], sep = "\t") |>
#  strsplit("\t")





