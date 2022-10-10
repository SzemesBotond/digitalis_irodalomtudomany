###az egész korpusz hasonlatainak kiszedése
library(dplyr)
input.dir <- "YOUR DIRECTORY"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:length(files.v)){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8" )
}
#az egeszet egy daataframe-be
regeny <- gsub("out_", "", files.v)
regeny <- gsub("\\.txt", "", regeny)
names(my.corpus.l) <- regeny
corpus <- bind_rows(my.corpus.l, .id = "regény")

#hasonlito kiszedese
idx3 <- grep("mint", corpus$V2)
idx4 <- grep("[.?!]", corpus$V2) #lehente vesszok is
#a mint-ektől a következő írásjelig tartó szakasz kszedése.
id <- c()
for (i in 1:length(idx3)) {
  if(i<length(idx3)){
    id[[i]] <- min(idx4[idx3[[i]]<idx4 & idx4<idx3[[i+1]]])
  }
  else if (i==length(idx3)){
    id[[i]] <- min(idx4[idx3[[i]]<idx4])  
  }
}
#van, hogy az irasjel nincs ket mint kozott, ezt "inf"-fel jeloli - az ehez legkozelebbi "mint" kiszedese
inf <- grep("Inf", id)
idx_hiba <- c ()
for (i in 1:length(inf)) {
  idx_hiba[[i]] <- idx3[inf[i]+1]  
}
idx_hiba <- unlist(idx_hiba)
idx3 <- idx3 [! idx3 %in% idx_hiba]
id <- id[which(id != "Inf")]
id <- unlist (id)
#hasonlatok, hasonlitasok
corpus_mondat <- c()
for (i in 1:length(idx3)) {
  corpus_mondat[[i]] <- corpus[idx3[[i]]:id[[i]],]  
}
corpus_hasonlat <- bind_rows(corpus_mondat)
#a hasonlatokat megszűrni
corpus_hasonlat_lemma <- corpus_hasonlat%>%
  filter(!grepl("\\d+", V2))%>%
  filter(!grepl("[[:punct:]]", V2))%>%
  filter(V3 != "PROPN")%>%
  anti_join(stop, by = c("V2"="stop"))%>%
  select(V2, regény)%>%
  rename(word = V2)

#regények szerinti rendezés és gyakorisági lista
corpus_hl <- corpus_hasonlat_lemma %>%
  group_by(regény)%>%
  count(word, sort = TRUE) 

#Két vh közötti regényekkel tfidf
deryhas_16 <- corpus_hl%>%
  filter(regény == "DeryTibor_ABefejezetlenMondat" |
          regény == "KrudyGyula_RezedaKazmerSzepElete" |
          regény == "FustMilan_FelesegemTortenete" |
          regény == "MaraiSandor_AGyertyakCsonkigEgnek" |
          regény == "Karinthy_UtazásAKoponyám" |
          regény == "SzerbAntal_UtasEsHoldvilag" |
          regény == "IllyesGyula_PusztakNepe" |
          regény == "TamasiAron_AbelARengetegben" |
          regény == "GelleriAndorEndre_ANagymosoda" |
          regény == "SzomoryDezso_AParizsiRegeny" |
           regény == "KosztolanyiDezso_EdesAnna" |
           regény == "KosztolanyiDezso_Aranysarkany" |
           regény == "TeresanszkyJozsiJeno_KakukkMarciIfjusaga" |
           regény == "KrudyGyula_HetBagoly" |
           regény == "SzaboDezsoCsodalatosELet" |
           regény == "SzepErno_LilaAkac" |
           regény == "BabitsMihaly_AGolyakalifa"
         )


dery_16_tfidf <- deryhas_vs16 %>%
  bind_tf_idf(word, regény, n) %>%
  arrange(desc(tf_idf))%>%
  top_n(50, tf_idf) %>%
  arrange(regény) 
deryhas_vs16 <- dery_16_tfidf%>%
  filter(regény == "DeryTibor_ABefejezetlenMondat")
dery_16_tfidf$word

