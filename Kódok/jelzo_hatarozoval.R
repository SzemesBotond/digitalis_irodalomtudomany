# magyarlnc által elemzett szövegek
input.dir <- "YOUR DIRECTORY"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:length(files.v)){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}



mn <- list()
mn11 <- list()
mn2<- list()
mn21<- list()
mn22<- list()
mn23<- list()
for(i in 1:length(my.corpus.l)){
  mn [[i]] <- grep("ADJ", my.corpus.l[[i]]$V3, ignore.case = T)
  #mn11 [[i]] <-grep("(Sing|Pres)$", my.corpus.l[[i]]$V4, ignore.case = T) #V5 ha emagyar és Sing a vége: $
  #mn[[i]] <- intersect(mn[[i]], mn11[[i]])
  mn2 [[i]] <-intersect(mn[[i]]-1, mn[[i]])
  mn21 [[i]] <- grep("(ul|ül|an|en)$", my.corpus.l[[i]]$V1, ignore.case = T)
  mn2 [[i]] <-intersect(mn2[[i]], mn21[[i]])
}


jelzett_jelzo <- szavak
for(i in 1:length(szavak)){
  jelzett_jelzo[[i]] <- list(szavak[[i]])
  for(j in 1:length(mn2[[i]])){
    if(length(mn2[[i]])>0)
      jelzett_jelzo[[i]][[j]] <- szavak[[i]][mn2[[i]][[j]]:(mn2[[i]][[j]]+1)]}
}


jelzett_jelzo2 <- jelzett_jelzo
for(i in 1:length(jelzett_jelzo)){
  jelzett_jelzo2[[i]] <- list(jelzett_jelzo[[i]])
  for(j in 1:length(jelzett_jelzo[[i]])){
    jelzett_jelzo2[[i]][[j]] <- jelzett_jelzo[[i]][[j]][!grepl("(teljesen|gyorsan|lassan|folyamatosan|erősen|túlságosan|biztosan|egyszerűen)", jelzett_jelzo[[i]][j])]
  }
}
for(i in 1:length(jelzett_jelzo2)){
  jelzett_jelzo2[[i]] <- jelzett_jelzo2[[i]][which(jelzett_jelzo2[[i]] != "character(0)")]
}


jelzett_jelzo_szama <- list()
jelzett_jelzo_aranya <- list()
for (i in 1:length(jelzett_jelzo2)){
  jelzett_jelzo_szama[[i]] <- length(jelzett_jelzo2[[i]])
  jelzett_jelzo_aranya[[i]] <- jelzett_jelzo_szama[[i]]/szavak_szama[[i]]*1000
}

unlist(jelzett_jelzo_aranya)
