# magyarlnc által elemzett szövegek
input.dir <- "YOUR DIRECTORY"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:length(files.v)){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}

#ság/ség, ás/és képző és a képzett melléknevek egymás után
elvont <- list()
fn <- list()
fn2 <- list()
fn3 <- list ()
elvont_fn <-list()
mn <- list ()
mn2 <- list ()
mn3 <- list()
mn21 <- list()
jelzo <- list()
for(i in 1:length(my.corpus.l)){
  elvont [[i]] <- grep("[a-zíéáűúőóüö]+(ság|ség|ás|és)$", my.corpus.l[[i]]$V2, ignore.case=T)
  fn [[i]] <- grep("NOUN", my.corpus.l[[i]]$V3, ignore.case = T)
  #fn2 [[i]] <- grep("Sing$", my.corpus.l[[i]]$V4, ignore.case = T)
  fn3 [[i]] <- grep("Dat|Acc|Nom|Ins", my.corpus.l[[i]]$V4, ignore.case = T)
  mn [[i]] <- grep("ADJ", my.corpus.l[[i]]$V3, ignore.case = T)
  mn2 [[i]] <- grep("(i|as|es|ös|os|ú|ű|tlan|tlen|talan|telen|ó)$", my.corpus.l[[i]]$V1, ignore.case = T)
  mn3 [[i]] <- grep("PartPast", my.corpus.l[[i]]$V4)
  mn21 [[i]] <- union(mn2[[i]], mn3[[i]])
  mn [[i]] <- intersect(mn[[i]], mn21[[i]])
  elvont_fn[[i]] <- intersect(elvont [[i]], fn[[i]])
  #elvont_fn[[i]] <- intersect(elvont_fn [[i]], fn2[[i]])
  elvont_fn[[i]] <- intersect(elvont_fn [[i]], fn3[[i]])
  jelzo [[i]] <- intersect(elvont_fn [[i]]-1, mn[[i]])
  }

#a pozícióknak megfelelő szópárok
szavak <- list()
for(i in 1:length(my.corpus.l)){
  szavak [[i]] <- my.corpus.l[[i]]$V1
}
jelzett_elvont <- szavak
for(i in 1:length(szavak)){
  jelzett_elvont[[i]] <- list(szavak[[i]])
  for(j in 1:length(jelzo[[i]])){
    if(length(jelzo[[i]])>0)
    jelzett_elvont[[i]][[j]] <- szavak[[i]][jelzo[[i]][[j]]:(jelzo[[i]][[j]]+1)]}
    }

#egy-két főnév kiszűrése - először minden egyedi főnév kimentése
elvont_fn_list <- szavak
for(i in 1:length(jelzo)){
  elvont_fn_list[[i]] <- list(szavak[[i]])
  for(j in 1:length(jelzo[[i]])){
    elvont_fn_list[[i]][[j]] <- my.corpus.l[[i]]$V2[(jelzo[[i]][[j]]+1)]
  }
}
elvont_fn_list <- unique(unlist(elvont_fn_list))
capture.output(elvont_fn_list, file="YOUR DIRECTORY/elvont_fn.txt")
# kézi meghatározás után a szavak kiszűrése
jelzett_elvont1 <- jelzett_elvont
for(i in 1:length(jelzett_elvont)){
  jelzett_elvont1[[i]] <- list(jelzett_elvont[[i]])
  for(j in 1:length(jelzett_elvont[[i]])){
    jelzett_elvont1[[i]][[j]] <- jelzett_elvont[[i]][[j]][!grepl("(oriás|evező-csapás|idő-töltés|személyiség|igazságszolgáltatás|
    tevékenység|állás|nézés|kávés|meghivás|festés|lakás|könyvkereskedés|gondolázás|lépés|
    asztaltársaság|járás|kaszás|nyilás|feleség|fakultás|termés|ellenség|rendszerváltozás|vonás|ujság|fogás|
    írás|előadás|hatóság|Hollaki-társaság|
    asszonyság|fenség|közönség|czigányprimás|fogadás|kurjogatás|sikoltozás|
    dudás|prés|biróság|helyiség|állomás|óbégatás|bokréta-óriás|csárdás|sipkás|felvonás|
    pajtás|megjegyzés|irás|népség|óriás|fejbeverés|lámpás|válás|majorság|papipalás|vasútállomás|újság|rónaság|
    Kinder-tojás|pelenkaféleség|bundás|tisztás|sajtféleség|időjárás|
    alvás|vetés|fizetés|házasság|vallás|júdás|jeremiás|sírás|
    intés|rés|tűzrakás|ritkás|vízállás|helység|helyőrség|kés|
    földhányás|repedés|köpés|ütés|hitközség|fürdés|kiadás|sertés|
    lövés|testőrség|pattanás|muskétás|fűtés|Újság|munkás|segédmunkás|ifjúmunkás|rendőrkapitányság|
    rendőrség|téglás|gyűlés|hagymamártás|zárás|diákszövetség|villamoscsengetés|
    kutyaugatás|kiejtés|bizottság|tojás|Parcen-rokonság|kamillaborogatás|letartóztatás|
    irodahelyiség|kirándulás|telefonhívás|felhívás|lakosság|
    prolinépség|kézírás|könyökforgatás|szállítómunkás|
    lábcsoszogás|lábmosás|mértékegység|cserjés|kacsatojás|
    síkfutás|hímzés|ajtónyílás|költség|
    pohárcsörrenés|arzéngyilkosság|sarkantyúpengés|
    figyelmeztetésre|légycsípés|kocsmázás|ülés|röhögés|
    kereszteződés|valami|kézmosás|nagytakarítás|vendégszereplés|napfogyatkozás)", jelzett_elvont[[i]][j])]
      }
}

for(i in 1:length(jelzett_elvont1)){
  jelzett_elvont1[[i]] <- jelzett_elvont1[[i]][which(jelzett_elvont1[[i]] != "character(0)")]
}

#regények elnevezése, a szószerkezetek kimentése
library(stringr)
for (i in 1:length(files.v)) {
  files.v[[i]] <- str_remove_all(files.v[[i]], ".txt")
  files.v[[i]] <- str_remove_all(files.v[[i]], "out_")
}
names(jelzett_elvont1) <- files.v

capture.output(jelzett_elvont1, file="YOUR DIRECTORY/jelzett_elvont.txt")

# szavak_szama mondathossz


jelzett_elvont_szama <- list()
jelzett_elvont_aranya <- list()
for (i in 1:length(jelzett_elvont1)){
  jelzett_elvont_szama[[i]] <- length(jelzett_elvont1[[i]])
  jelzett_elvont_aranya[[i]] <- jelzett_elvont_szama[[i]]/szavak_szama[[i]]*1000
}

unlist(jelzett_elvont_aranya)

#-val-vel

elvont_fnval <- list()
for (i in 1:length(elvont_fn)){
  elvont_fnval [[i]] <- grep("[a-zíűáéúőóüö]+(ssal|ggal)", jelzett_elvont[[i]])
  elvont_fnval [[i]] <- jelzett_elvont[[i]][elvont_fnval [[i]]]
  }
  
jelzett_elvontval_szama <- list()
jelzett_elvontval_aranya <- list()
for (i in 1:length(elvont_fnval)){
  jelzett_elvontval_szama[[i]] <- length(elvont_fnval[[i]])
  jelzett_elvontval_aranya[[i]] <- jelzett_elvontval_szama[[i]]/szavak_szama[[i]]*1000
}

unlist(jelzett_elvontval_aranya)


