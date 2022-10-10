#stopszavak
stop <- scan(paste("YOUR DIRECTORY/FILE", sep="/"),
             what="character", sep="\f", quote = "", encoding = "UTF-8")
stop <- data_frame(stop)

####Dery A befejezetlen modnat hasonlatai####
library(dplyr)
input <- "YOUR DIRECTORY/DéryTibor_ABefejezetlenMondat.txt"
dery <- read.table(paste(input, sep=" "), header = FALSE, fill = TRUE, encoding = "UTF-8" )
dery <- dery%>%
  filter(V3 != "PROPN")%>%
  filter(V2 != "lőrinc")%>%
  filter(V2 != "vallesz")%>%
  filter(V2 != "kacsomilla")%>%
  filter(V2 != "reischné")%>%
  filter(V2 != "benedek")%>%
  filter(V2 != "ilma")%>%
  filter(V2 != "rózsa")

#kotetekre bontas
idx1 <- which(dery$V1 == "MÁSODIK")
idx2 <- which(dery$V1 == "HARMADIK")
d_1 <- dery[1:idx1,]
d_2 <- dery[idx1:idx2,]
d_3 <- dery[idx2:nrow(dery),]
kotet <- bind_rows(mutate(d_1, kötet = "első"),
                   mutate(d_2, kötet = "második"),
                   mutate(d_3, kötet = "harmadik"))%>%
  select(V2, kötet)

#ha csak az első és harmadik fejezet
#kotet <- kotet %>%
#  filter(kötet != "második")

#hasonlito kiszedese
idx3 <- grep("mint", kotet$V2)
idx4 <- grep("[.?!]", kotet$V2) #lehente vesszok is
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
kotet_mondat <- c()
for (i in 1:length(idx3)) {
  kotet_mondat[[i]] <- kotet[idx3[[i]]:id[[i]],]  
}
hasonlat_kotet <- bind_rows(kotet_mondat)

#stopszo-szűrés és a kötetek külön változóba mentése
hasonlat_kotet$V2 = gsub("[[:punct:]]", "", hasonlat_kotet$V2)
hasonlat_kotet<- hasonlat_kotet%>%
  anti_join(stop, by = c("V2"="stop"))%>%
  filter(V2 != "")

elso_kotet <- hasonlat_kotet%>%
          filter(kötet == "első")
kettoharom <- hasonlat_kotet%>%
  filter(kötet != "első")
harmadik_kotet <- hasonlat_kotet%>%
  filter(kötet == "harmadik")
masodik_kotet <- hasonlat_kotet%>%
  filter(kötet == "második")
egyketto <- hasonlat_kotet%>%
  filter(kötet != "harmadik")

### 1. Zeta
library(stylo)

zeta = oppose(primary.corpus = list(elso_kotet$V2,elso_kotet$V2), secondary.corpus = list(harmadik_kotet$V2,harmadik_kotet$V2))

zeta = oppose(primary.corpus = list(elso_kotet$V2,elso_kotet$V2), secondary.corpus = list(tobbi$word,tobbi$word))

#zeta = oppose(primary.corpus = list(meszolyhas$word,meszolyhas$word), secondary.corpus = list(deryhas$word,deryhas$word))



### 2. log likelihood
#először frekvencia-listák - stlyo-val

#igy egyszerűbb és csak indexelni aztán [1, ]
tokenized.texts = list(elso_kotet$V2, harmadik_kotet$V2)
freqlist = make.frequency.list(tokenized.texts)
freqs = make.table.of.frequencies(tokenized.texts, freqlist)

#vagy külön:
freqlist = make.frequency.list(elso_kotet$V2)
freqs1 = make.table.of.frequencies(elso_kotet$V2, freqlist)

freqlist3 = make.frequency.list(harmadik_kotet$V2)
freqs3 = make.table.of.frequencies(harmadik_kotet$V2, freqlist3)

#words = factor(union(names(freqs1), names(freqs2)))
words = union(names(freqs1), names(freqs3))
freqs1 = freqs1[words]
freqs3 = freqs3[words]
names(freqs1) = words
names(freqs3) = words
freqs1[is.na(freqs1)] = 0
freqs3[is.na(freqs3)] = 0

N1 = sum(freqs1) # az első szöveg teljes mérete
N2 = sum(freqs3) # második szöveg
E1 = N1 * (freqs1 + freqs3) / (N1 + N2) # expected values
E2 = N2 * (freqs1 + freqs3) / (N1 + N2)

O1 = freqs1 * log(freqs1 / E1) # observed values
O1[is.na(O1)] = 0
O2 = freqs3 * log(freqs3 / E2)
O2[is.na(O2)] = 0

G2 = 2 * (O1 + O2)

# a nem jellemzők -1-gyel szorzása, hogy fordított sorrendbe
G2[(freqs3 / N2) >= (freqs1 / N1)] = G2[(freqs3 / N2) >= (freqs1 / N1)] * -1

G2 = sort(unclass(G2), decreasing = TRUE)

G2[1:100]
capture.output(G2[1:100], file=paste("Dery_loglikelihood_harmadik_vs_egesz.txt",sep=""))



### cracovian
freqlist1 = make.frequency.list(elso_kotet$V2,  value = TRUE)
freqlist2 = make.frequency.list(harmadik_kotet$V2, value = TRUE)

freqlist1 = cumsum(freqlist1/100)
freqlist2 = cumsum(freqlist2/100)

freqs21 = freqlist2[names(freqlist1)]
freqs21[is.na(freqs21)] = 1
names(freqs21) = names(freqlist1)
freqs11 = freqlist1[names(freqlist2)]
freqs11[is.na(freqs11)] = 1
names(freqs11) = names(freqlist2)

x1 = (freqs21 - freqlist1)
x2 = (freqs11 - freqlist2)
x1 = x1[names(freqlist1)]
x2 = x2[names(freqlist2)]

sort(x1, decreasing = TRUE)[1:100]
sort(x2, decreasing = TRUE)[1:100]

capture.output(sort(x1, decreasing = TRUE)[1:100], file=paste("Dery_cracovian_elso_vs_harmadik.txt",sep=""))
capture.output(sort(x2, decreasing = TRUE)[1:100], file=paste("Dery_cracovian_harmadik_vs_elso.txt",sep=""))


