# magyarlnc által elemzett szövegek
input.dir <- "YOUR DIRECTORY"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:length(files.v)){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}

## használt függvények
intersect_lists <- function (x,y) {
  lapply(seq(length(my.corpus.l)),
         function(i) Reduce(intersect,lapply(list(x, y),"[[",i)))
}

grep_list_df <- function (x, y){
  lapply(my.corpus.l, 
         function(df) grep(x, df[,y], ignore.case = T))
}

prev_pos <- function (a){
  lapply(a, function(x) x-1)
}

next_pos <- function (a,b){
  lapply(a, function(x) x+b)
}

###
birtok <- grep_list_df("poss", 4) #vagy psor ha magyarlánc
fn <- grep_list_df("NOUN", 3)
#fn2 <- grep_list_df("Sing$", 5)
fn3 <- grep_list_df("Acc|Nom|Ins", 4)
fn <- intersect_lists(fn, fn3) #lehetne fn2 is
mn <- grep_list_df("ADJ", 3)
birtokos1 <- intersect_lists(prev_pos(birtok), fn)
jelzo1 <- intersect_lists(prev_pos(birtok), mn)
birtokos2 <- intersect_lists(prev_pos(jelzo1), fn)
jelzo2 <- intersect_lists(prev_pos(jelzo1), mn)
birtokos3 <- intersect_lists(prev_pos(jelzo2), fn) 
det <- grep_list_df("DET", 3)
det1 <- intersect_lists(prev_pos(birtokos1), det)  
det2 <- intersect_lists(prev_pos(birtokos2), det)
det3 <- intersect_lists(prev_pos(birtokos3), det)
birtokos1 <- next_pos(det1, 1)
birtokos2 <- next_pos(det2, 1)
birtokos3 <- next_pos(det3, 1)
birtok <- do.call(Map, c(c, list(
  next_pos(birtokos1, 1),
  next_pos(birtokos2, 2),
  next_pos(birtokos3, 3))))
birtok <- lapply(lapply(birtok, sort, decreasing = F), unique)
összbirt <- lapply(lapply(lapply(seq(length(my.corpus.l)),
                                 function(i) Reduce(c,lapply(
                                   list(birtokos1,
                                        birtokos2,
                                        birtokos3,
                                        birtok),"[[",i))),
                          sort, decreasing = F), unique)
szavak <- lapply(my.corpus.l, "[", , "V2")


#a pozícióknak megfelelő szópárok

összbirt_szo <- Map(`[`, szavak, összbirt)
elvontbirt <- lapply(összbirt_szo, 
       function(lt) grep("[a-zíéáűúőóüö]+(ság|ség|ás|és)$", lt, ignore.case = T))
elvontbirt_szo <- Map(`[`, összbirt_szo,elvontbirt)

#szűréshez kiírás
elvontbirt_szo2 <- sapply(elvontbirt_szo, unlist, recursive = F)
kiiras <- unique(unlist(elvontbirt_szo2))
options(max.print=1000000)
capture.output(kiiras, file="elvontstop.txt")

# szűrés a kézi feldolgozás után
library(tidyverse)
library(dplyr)
stop <- scan(paste("YOUR DIRECTORY/elvontstop.txt", sep="/"),
             what="character", sep="\f", quote = "", encoding = "UTF8")
stop <- data_frame(stop)

elvontbirt_szo1 <- lapply(lapply(elvontbirt_szo, unlist, recursive= F), tibble)
elvontbirt_szo1 <- lapply(elvontbirt_szo1, setNames, "elvont")
elvontnirt_szox <- lapply(seq(length(my.corpus.l)),
                          function(i) elvontbirt_szo1[[i]] %>%
                            anti_join(stop1, by = c("elvont"="stop")))
#arány

elvbirtszam <- lapply(elvontbirt_szox, nrow)
elvbirtarany <- sapply(mapply(FUN = `/`, elvbirtszam, szavak_szama, SIMPLIFY = FALSE), function(x) x*1000)
