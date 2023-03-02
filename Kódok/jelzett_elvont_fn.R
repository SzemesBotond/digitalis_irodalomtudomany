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

union_lists <- function (x,y) {
  lapply(seq(length(my.corpus.l)),
         function(i) Reduce(union,lapply(list(x, y),"[[",i)))
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

#ság/ség, ás/és képző és a képzett melléknevek egymás után
elvont <- grep_list_df("[a-zíéáűúőóüö]+(ság|ség|ás|és)$", 2)
fn <- grep_list_df("NOUN", 3)
#fn2 <- grep_list_df("Sing$", 5)
fn3 <- grep_list_df("Dat|Acc|Nom|Ins", 4)
fn <- intersect_lists(fn, fn3) #lehetne fn2 is
mn <- grep_list_df("ADJ", 3)
mn2 <- grep_list_df("(i|as|es|ös|os|ú|ű|tlan|tlen|talan|telen|ó)$", 1)
mn3 <- grep_list_df("PartPast", 4)
mn21 <- union_lists(mn2, mn3)
mn <- intersect_lists(mn, mn21)
elvont_fn <- intersect_lists(elvont, fn)
#elvont_fn <- intersect_lists(elvont_fn, fn2)
elvont_fn <- intersect_lists(elvont_fn , fn3)
jelzo <- intersect_lists(prev_pos(elvont_fn), mn)
jelett <- next_pos(jelzo, 1)

#a pozícióknak megfelelő szópárok
szavak <- lapply(my.corpus.l, "[", , "V1")
lemmák <- lapply(my.corpus.l, "[", , "V2")
jelzo_szo <- Map(`[`, lemmák, jelzo )
jelzett_szo <- Map(`[`, szavak, jelzett)
jelzett_lemma <- Map(`[`, lemmák, jelzett)
jelzett_elvont <- sapply(seq(length(jelzo_szo)),
                         function(i) lapply(
                           seq(length(jelzo_szo[[i]])),
                           function (j) c(
                             jelzo_szo[[i]][j],jelzett_szo[[i]][j], jelzett_lemma[[i]][j]
                           )))

#egy-két főnév kiszűrése - először minden egyedi főnév kimentése
#elvont_fn_list <- unique(unlist(jelzett_szo))
#capture.output(elvont_fn_list, file="YOUR DIRECTORY/elvont_fn.txt")

# kézi meghatározás után a szavak kiszűrése
stop <- scan(paste("YOUR DIRECTORY/elvontstop.txt", sep="/"),
             what="character", sep="\f", quote = "", encoding = "UTF8")
stop <- data_frame(stop)

jelzett_elvont_df <- lapply(seq(length(jelzo_szo)), 
                            function (i) tibble(jelzo = jelzo_szo[[i]], fn = jelzett_szo[[i]], fnlemma = jelzett_lemma[[i]] ))
jelzett_elvont_szurt <- lapply(seq(length(my.corpus.l)),
                               function(i) jelzett_elvont_df[[i]] %>%
                                 anti_join(stop, by = c("fnlemma"="stop")))

#regények elnevezése, a szószerkezetek kimentése
files.v <- lapply(files.v, str_remove_all, "(.txt|out_)")
names(jelzett_elvont_szurt) <- files.v
options(dplyr.print_max = 1e9)
capture.output(jelzett_elvont_szurt, file="proba.txt")

#


jelzett_elvont_szama <- lapply(jelzett_elvont_szurt, nrow)
jelzett_elvont_aranya <- sapply(mapply(FUN = `/`, jelzett_elvont_szama, szavak_szama, SIMPLIFY = FALSE), function(x) x*1000)


#-val-vel

elvont_fnval <- lapply(seq(length(my.corpus.l)),
                       function(i) jelzett_elvont_szurt[[i]]%>% 
                         filter(grepl("[a-zíűáéúőóüö]+(ssal|ggal)", fn)))

jelzett_elvontval_szama <- lapply(elvont_fnval, nrow)
jelzett_elvontval_aranya <- sapply(mapply(FUN = `/`, jelzett_elvontval_szama, szavak_szama, SIMPLIFY = FALSE), function(x) x*1000)

