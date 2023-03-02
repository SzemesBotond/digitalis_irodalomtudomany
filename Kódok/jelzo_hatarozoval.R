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

mn <- grep_list_df("ADJ", 3)
#mn11 <-grep_list_df("(Sing|Pres)$", V4) #V5 ha emagyar és Sing a vége: $
#mn <- intersect_lists(mn, mn11)
mn2 <-intersect_lists(prev_pos(mn), mn)
mn21_0 <- grep_list_df("(ul|ül|an|en)$", 1)
jelzo <-intersect_lists(mn2, mn21)
jelzett <- next_pos(jelzo, 1)


#a pozícióknak megfelelő szópárok
szavak <- lapply(my.corpus.l, "[", , "V1")
lemmák <- lapply(my.corpus.l, "[", , "V2")
jelzo_szo <- Map(`[`, szavak, jelzo )
jelzett_szo <- Map(`[`, szavak, jelzett)
jelzett_jelzo <- sapply(seq(length(jelzo_szo)),
                        function(i) lapply(
                          seq(length(jelzo_szo[[i]])),
                          function (j) c(
                            jelzo_szo[[i]][j],jelzett_szo[[i]][j], jelzett_lemma[[i]][j]
                          )))
#stopszó-szűrés
stop <- tibble(stop=c("teljesen",
                      "gyorsan",
                      "lassan",
                      "folyamatosan",
                      "erősen",
                      "túlságosan",
                      "biztosan",
                      "egyszerűen",
                      "amolyan"))

jelzett_jelzo_df <- lapply(seq(length(jelzo_szo)), 
                           function (i) tibble(határozó = jelzo_szo[[i]], jelző = jelzett_szo[[i]] ))
jelzett_jelzo_szurt <- lapply(seq(length(my.corpus.l)),
                              function(i) jelzett_elvont_df[[i]] %>%
                                anti_join(stop, by = c("határozó"="stop")))


jelzett_jelzo_szama <- lapply(jelzett_jelzo_szurt, nrow)
jelzett_jelzo_aranya <- sapply(mapply(FUN = `/`, jelzett_jelzo_szama, szavak_szama, SIMPLIFY = FALSE), function(x) x*1000)

