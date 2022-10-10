# magyarlnc által elemzett szövegek
input.dir <- "YOUR DIRECTORY"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:length(files.v)){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}


birtok <- list()
birtokos1 <- list()
birtokos2 <- list()
birtokos3 <- list()
fn <- list()
fn2 <- list ()
fn3 <- list ()
mn <- list()
jelzo1 <- list()
jelzo2 <- list()
det <- list()
det1 <- list()
det2 <- list()
det3 <- list()
szavak <- list()
összbirt <- list()
for(i in 1:length(my.corpus.l)){
  #az emagyar "poss"
  birtok [[i]] <- grep("psor", my.corpus.l[[i]]$V4, ignore.case=T)
  fn [[i]] <- grep("NOUN", my.corpus.l[[i]]$V3, ignore.case = T)
  #fn2 [[i]] <- grep("Sing$", my.corpus.l[[i]]$V4, ignore.case = T)
  fn3 [[i]] <- grep("Acc|Nom|Ins", my.corpus.l[[i]]$V4, ignore.case = T)
  #fn [[i]] <- intersect(fn[[i]], fn2[[i]])
  fn [[i]] <- intersect(fn[[i]], fn3[[i]])
  mn [[i]] <- grep("ADJ", my.corpus.l[[i]]$V3, ignore.case = T)
  birtokos1[[i]] <- intersect(birtok [[i]]-1, fn[[i]])
  jelzo1[[i]] <- intersect(birtok [[i]]-1, mn[[i]])
  birtokos2[[i]] <- intersect(jelzo1[[i]]-1, fn[[i]])
  jelzo2[[i]] <- intersect(jelzo1[[i]]-1, mn[[i]])
  birtokos3[[i]] <- intersect(jelzo2[[i]]-1, fn[[i]])
  det[[i]]  <- grep("DET", my.corpus.l[[i]]$V3, ignore.case = T)
  det1[[i]] <- intersect(birtokos1[[i]]-1, det[[i]])
  det2[[i]] <- intersect(birtokos2[[i]]-1, det[[i]])
  det3[[i]] <-intersect(birtokos3[[i]]-1, det[[i]])
  birtokos1[[i]] <- det1[[i]]+1
  birtokos2[[i]] <- det2[[i]]+1
  birtokos3[[i]] <-det3[[i]]+1
  birtok[[i]] <- c(birtokos1[[i]]+1, birtokos2[[i]]+2, birtokos3[[i]]+3)
  birtok[[i]] <- unique(sort(birtok[[i]], decreasing = F))
  összbirt[[i]] <- unique(sort(c(birtokos1[[i]],birtokos2[[i]], birtokos3[[i]], birtok[[i]]), decreasing = F))
  szavak [[i]] <- my.corpus.l[[i]]$V2
  }


#a pozícióknak megfelelő szópárok

összbirt_szo  <- szavak
elvontbirt <- list()
for(i in 1:length(szavak)){
  összbirt_szo[[i]] <- list(szavak[[i]])
  for(j in 1:length(összbirt[[i]])){
    if(length(összbirt[[i]])>0){
      összbirt_szo[[i]][[j]] <- szavak[[i]][összbirt[[i]][[j]]]}}
  elvontbirt [[i]] <- grep("[a-zíéáűúőóüö]+(ság|ség|ás|és)$", összbirt_szo[[i]], ignore.case=T)
  }  

elvontbirt_szo <- összbirt_szo
for(i in 1:length(szavak)){
  elvontbirt_szo[[i]] <- list(összbirt_szo[[i]])
  for(j in 1:length(elvontbirt[[i]])){
    if(length(elvontbirt[[i]])>0){
      elvontbirt_szo[[i]][[j]] <- összbirt_szo[[i]][elvontbirt[[i]][[j]]]}}
}  


#szűréshez kiírás
elvontbirt_szo2 <- list()
for(i in 1:length(elvontbirt_szo1)){
  elvontbirt_szo2[[i]] <- unlist(elvontbirt_szo[[i]], recursive =  F)
}
kiiras <- unique(unlist(elvontbirt_szo2))
options(max.print=1000000)
capture.output(kiiras, file="elvontstop.txt")

# szűrés a kézi feldolgozás után
library(tidyverse)
library(dplyr)
stop <- scan(paste("YOUR DIRECTORY/elvontstop.txt", sep="/"),
             what="character", sep="\f", quote = "", encoding = "UTF8")
stop <- data_frame(stop)

elvontbirt_szox <- list()
elvontbirt_szo1 <- list()
for(i in 1:length(elvontbirt_szo)){
  elvontbirt_szo1[[i]] <- unlist(elvontbirt_szo[[i]], recursive= F)
  elvontbirt_szo1[[i]] <- data.frame(t(data.frame(elvontbirt_szo1[[i]])))
  colnames(elvontbirt_szo1[[i]]) <- "elvont"
  elvontbirt_szox[[i]] <- elvontbirt_szo1[[i]] %>%
    anti_join(stop, by = c("elvont"="stop"))
  
  }

#arány

elvbirtszam  <- list()
elvbirtarany <- list()
for(i in 1:length(elvontbirt_szox)){
  elvbirtszam [[i]] <- nrow(elvontbirt_szox[[i]])
  elvbirtarany [[i]] <- elvbirtszam[[i]]/szavak_szama[[i]]*1000
}

unlist(elvbirtarany)
