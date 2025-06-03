rm(list=ls())


library(tidyverse)
library(survey)
library(srvyr)
library(readxl)


# H: magyar teljes
load("C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/adatfile_valtozatok/EUSILCH20052023teljes.RData")
# E: EU27 502 hulláma, de csak kiválasztott változók
#load("C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/adatfile_valtozatok/EUSILCeu27.RData")
# medinokEU27 (ez évenként, országonként tartalmazza 
# a súlyozott mediánt: HX090median
# a küszöböt: AROP60tresh
# a HX090 eloszlásának felső 10%-át levágó határt: also90
load("C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/reszeredmenyek/medianokEU27.Rdata")

# empirikus eloszlás
emp <- read.csv2("C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/reszeredmenyek/HUempeloszlas.csv")
HUempELOSZLASegyfo <- read.csv2("C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/reszeredmenyek/HUempeloszlasEGYFO.csv")
# adminisztratív módon megállapított jövedelemösszegek
miniber <- read_excel(
  "C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/reszeredmenyek/minimalberek_beolvasasra_v2.xlsx",
  na = c("", "NA")
)




# 1. EU27 országok mindegyikére megvizsgál kérdések
# 
# ## 1.1. Mennyire tekinthető a bruttonetto problematika (HY010 \< HY020) is hungarikumnak? Megjelneik-e ez máshol, és ha igen, milyen mértékben?
# 
# E <- E %>% rename(hid = RX030,
#                   pid = RB030,
#                   y = HB010,
#                   weight=RB050,
#                   szulev=RB080,
#                   c=RB020)
# 
# E <- left_join(E,medianokEU27,by=c("c","y"))
# 
# 
# 
# 
# E$m050kornyek <- 0
# E$m060kornyek <- 0
# E$m070kornyek <- 0
# E$m100kornyek <- 0
# 
# E$m050kornyek[E$HX090>E$HX090median*0.48 & E$HX090<E$HX090median*0.52] <- 1
# E$m060kornyek[E$HX090>E$HX090median*0.58 & E$HX090<E$HX090median*0.62] <- 1
# E$m070kornyek[E$HX090>E$HX090median*0.68 & E$HX090<E$HX090median*0.72] <- 1
# E$m100kornyek[E$HX090>E$HX090median*0.98 & E$HX090<E$HX090median*1.02] <- 1
# 
# # EU27-ben mekkora a HY010 < HY020 arany
# 
# E$bruttonettobaj <- 0
# E$bruttonettobaj[E$HY010 < E$HY020] <- 1
# 
# table(E$bruttonettobaj)
# 
# 
# EkornyBruttoNetto <- E %>% 
#   group_by(c,y) %>% 
#   summarize(m050kornyek = mean(m050kornyek,na.rm=T,vartype=NULL),
#             m060kornyek = mean(m060kornyek,na.rm=T,vartype=NULL),
#             m070kornyek = mean(m070kornyek,na.rm=T,vartype=NULL),
#             m100kornyek = mean(m100kornyek,na.rm=T,vartype=NULL),
#             bruttonettobaj = mean(bruttonettobaj,na.rm=T,vartype=NULL))
# 
# hist(EkornyBruttoNetto$bruttonettobaj,breaks = 200)
# 
# 
# 
# EkornyBruttoNetto %>% 
#   filter(bruttonettobaj>0.1) %>% 
#   select(c,y,bruttonettobaj,m060kornyek) %>%
#   arrange(desc(bruttonettobaj))
# 
# 
# # 2. Csak magyar adatfilerész vizsgálatával

## 2.1. A magyar adatokon mikor és milyen mértékben jelennek meg ezekben változókban negatív értékek: HY120 HY130 HY140.

### 2.1.1. Nulladik lépés: melyik évben melyik változó használható?



H <- H %>% rename(hid = RX030,
                  pid = RB030,
                  y = HB010,
                  weight=RB050,
                  szulev=RB080,
                  c=RB020)
H$c <- "HU"

H <- left_join(H,medianokEU27,by=c("c","y"))

Hegyfo <- subset(H,HX060==5)

# bálna-ábra




# HY010   < HY020 aránya
H$bruttonettobaj <- 0
H$bruttonettobaj[H$HY010 < H$HY020] <- 1

#HY120 HY130 HY140 negatív

# HY120N és HY120G: mikor melyiket gyűjtik?

H$HY120N_NA <- is.na(H$HY120N)
H$HY130N_NA <- is.na(H$HY130N)
H$HY140N_NA <- is.na(H$HY140N)
H$HY120G_NA <- is.na(H$HY120G)
H$HY130G_NA <- is.na(H$HY130G)
H$HY140G_NA <- is.na(H$HY140G)


H$HY120Gneg <- 0
H$HY130Gneg <- 0
H$HY140Gneg <- 0

H$HY120Gneg[H$HY120G < 0] <- 1
H$HY130Gneg[H$HY130G < 0] <- 1
H$HY140Gneg[H$HY140G < 0] <- 1

# átutazók:
# akik a küszöb alól a küszöb fölé utaznak a HY140vonatán
H$utazo <- 0
H$utazo[H$HX090 > H$AROP60tresh & 
          H$HY140Gneg==1 & 
          H$HY140G < H$HX090 - H$AROP60tresh] <- 1



H120130140ugyek <- H %>% group_by(y) %>%
  summarize(bruttonettobaj = mean(bruttonettobaj,na.rm=T,vartype=NULL),
            HY120N_NA = mean(HY120N_NA,na.rm=T,vartype=NULL),
            HY130N_NA = mean(HY130N_NA,na.rm=T,vartype=NULL),
            HY140N_NA = mean(HY140N_NA,na.rm=T,vartype=NULL),
            HY120G_NA = mean(HY120G_NA,na.rm=T,vartype=NULL),
            HY130G_NA = mean(HY130G_NA,na.rm=T,vartype=NULL),
            HY140G_NA = mean(HY140G_NA,na.rm=T,vartype=NULL),
            HY120Gneg = mean(HY120Gneg,na.rm=T,vartype=NULL),
            HY130Gneg = mean(HY130Gneg,na.rm=T,vartype=NULL),
            HY140Gneg = mean(HY140Gneg,na.rm=T,vartype=NULL))


### 2.1.2. HY140G negatív

H120130140ugyek %>% select(y,bruttonettobaj,HY140Gneg)
barplot(H120130140ugyek$HY140Gneg,names.arg=2005:2023,cex.axis=1,cex.names=1)

#új bruttónettó háztartásonként

# 1. Háztartásonkénti egyedi rekord létrehozása
H_haztartas <- H %>%
  group_by(hid, y) %>%
  slice(1) %>%  # megtartjuk az első személyt háztartásonként
  ungroup()

# 2. Jelző változó háztartásszintű bruttó–nettó és HY140G problémára
H_haztartas <- H_haztartas %>%
  mutate(bruttonettobaj = as.integer(HY010 < HY020),
         HY140Gneg = as.integer(HY140G < 0))

# 3. Arány kiszámítása évenként
haztartasi_aranyok <- H_haztartas %>%
  group_by(y) %>%
  summarize(bruttonettobaj_arany = mean(bruttonettobaj, na.rm = TRUE),
            HY140Gneg = mean(HY140Gneg,na.rm=T,vartype=NULL))

haztartasi_aranyok

## 2.2. Egyenletprobléma: HY020 nem HY010-HY120G-HY130G-HY140G

# Definíciószerint  HY020 = HY010 – HY120G – HY130G – HY140G. 
# -   Regular taxes on wealth (HY120G)
# -   Regular inter-household cash transfers paid (HY130G); 
# -   Tax on income and social insurance contributions (HY140G);  
# 
# Itt az eredmények azt mutatják, hogy
# -   az egyes hullámokban mekkora arányban jelentkezik a probléma (esetszám, %)
# -   ha van egyenletprobélma, mekkora a különbség (kategorizálva)
# 

### 2.2.1. Az esetek hány százalékában nem teljesül az egyenlet?


H$egyenletproblema <- 0
H$egyenletproblema[H$HY020 != H$HY010 - H$HY120G - H$HY130G - H$HY140G] <- 1
H$egyenletproblemaDIFF <- 0
H$egyenletproblemaDIFF[H$egyenletproblema==1] <- H$HY020[H$egyenletproblema==1] - 
  (H$HY010[H$egyenletproblema==1] - H$HY120G[H$egyenletproblema==1] - 
     H$HY130G[H$egyenletproblema==1] - H$HY140G[H$egyenletproblema==1])

summary(H$egyenletproblemaDIFF)

table(H$y,H$egyenletproblema)
prop.table(table(H$y,H$egyenletproblema),1)





### 2.2.2. Mekkora a különbség?


H$egyenletproblemaDIFFkat <- 0
H$egyenletproblemaDIFFkat[abs(H$egyenletproblemaDIFF) >0  & abs(H$egyenletproblemaDIFF) <= 1] <- 1
H$egyenletproblemaDIFFkat[abs(H$egyenletproblemaDIFF) > 1 & abs(H$egyenletproblemaDIFF) <=10 ] <- 2
H$egyenletproblemaDIFFkat[abs(H$egyenletproblemaDIFF) >  10 & abs(H$egyenletproblemaDIFF) <=100 ] <- 3
H$egyenletproblemaDIFFkat[abs(H$egyenletproblemaDIFF) >  100 & abs(H$egyenletproblemaDIFF) <=1000 ] <- 4
H$egyenletproblemaDIFFkat[abs(H$egyenletproblemaDIFF) >  1000 ] <- 5

#table(H$y,H$egyenletproblemaDIFFkat)
#round(prop.table(table(H$y,H$egyenletproblemaDIFFkat),1),3)

H$egyenletproblemaDIFFkat <- factor(H$egyenletproblemaDIFFkat,
                                    levels=0:5,
                                    labels=c("0",
                                             "0<diff<=1",
                                             "1<diff<=10",
                                             "10<diff<=100",
                                             "100<diff<=1000",
                                             "diff>1000"))

table(H$y,H$egyenletproblemaDIFFkat)
round(prop.table(table(H$y,H$egyenletproblemaDIFFkat),1),3)


## 2.3. HA a HY140G negatív értékeit nem vesszük figyelembe, hogyan változik az eloszlás?


### összes HY140G-n való változás

#HX090 és HY020 jav1: minden negatív HY140G estetén korrigálok ("csúszók")
H$csuszik <- 0
H$csuszik[H$HY140G < 0] <- 1

H$HY020jav1 <- H$HY020
H$HY020jav1[H$HY140G < 0] <- H$HY020[H$HY140G < 0] + H$HY140G[H$HY140G < 0]

H$HX090jav1 <- H$HY020jav1 / H$HX050




HX090javhist <- function(ev) {
  # Histogram határai
  kuszob <- medianokEU27$AROP60tresh[medianokEU27$y == ev & medianokEU27$c == "HU"]
  teteje <- medianokEU27$also90[medianokEU27$y == ev & medianokEU27$c == "HU"]
  
  # Histogram generálása
  hist <- hist(H$HX090[H$y == ev & H$HX090 < teteje[1] & H$HX090 > -500],
               plot = FALSE,
               breaks = seq(-500, round(teteje[1] / 100 + 1) * 100, 100)) 
  histJAV <- hist(H$HX090jav1[H$y == ev & H$HX090jav1 < teteje[1] & H$HX090jav1 > -500],
                  plot = FALSE,
                  breaks = seq(-500, round(teteje[1] / 100 + 1) * 100, 100))
  # Kiemelt területek definíciója
  highlight_ranges <- list(
    "2017" = list(c(-500, 0)),
    "2018" = list(c(3200, 3300)),
    "2019" = list(c(3500, 3800)),
    "2020" = list(c(3900, 4000)),
    "2021" = list(c(4000, 4100)),                  
    "2022" = list(c(0, 1)),
    "2023" = list(c(7800, 7900), c(4400, 4500))
  )
  
  # Oszlopszínek: alaplila, piros, stb.
  colors <- rep("#999999", length(hist$mids))
  colors[hist$mids < kuszob[1]] <- "#984ea3"
  ranges <- highlight_ranges[[as.character(ev)]]
  if (!is.null(ranges)) {
    for (range in ranges) {
      colors[hist$mids >= range[1] & hist$mids <= range[2]] <- "#ca0020"
    }
  }
  
  # Alap histogram kirajzolása
  plot(hist, col = colors, main = str_c(ev," HY140G miatti eloszlásváltozás"),
       xlab = "Egy fogyasztási egységre jutó jövedelem, 100 eurós sávokban", 
       ylab = "Esetek száma a 100 eurós sávban")
  
  abline(v = kuszob[1], col = "#377eb8", lwd = 2)
  text(x = kuszob[1], 
       y = max(hist$counts) * 0.85, 
       labels = paste("szegénységi küszöb", round(kuszob[1]), "€"), 
       pos = 2, col = "#377eb8", cex = 1.4)
  
  
  
  # Előkészítés a sávokra
  szxleft <- hist$breaks[-length(hist$breaks)]
  szxright <- szxleft + 100
  szcount <- hist$counts
  szcountJAV <- histJAV$counts
  
  szinezni <- data.frame(szxleft = szxleft, # bal oldali határoló x érték
                         szxright = szxright, # jobb oldali határoló x érték
                         szcount = szcount,
                         szcountJAV = szcountJAV) # y érték (milyen magasra megy ebben a sávban histogram (javítás nélkül))
  
  # Eredeti oszlopsáv színezés csúszókra (sötétkék)
  szinezni$pluszminusz <- szinezni$szcount - szinezni$szcountJAV #ha negatív: hiányt: világossal ha pozitív: többletet sötéttel
  
  for (i in 1:nrow(szinezni)) {
    if (szinezni$pluszminusz[i] > 0) {
      rect(xleft = szinezni$szxleft[i],
           xright = szinezni$szxright[i],
           ybottom = szinezni$szcount[i],
           ytop = szinezni$szcountJAV[i],
           col = "darkblue",
           border = NA)
    }
  }
  
  # világoskék színezés
  for (i in 1:nrow(szinezni)) {
    if (szinezni$pluszminusz[i] < 0) {
      rect(xleft = szinezni$szxleft[i],
           xright = szinezni$szxright[i],
           ybottom = szinezni$szcount[i],
           ytop = szinezni$szcountJAV[i] ,
           col = "lightblue",  
           border = NA)
    }
  }
  
  # Várható eloszlás (zöld vonal)
  x_vals <- seq(0, teteje[1], length.out = 16)
  y_vals <- emp$y
  y_vals <- y_vals * quantile(hist$counts, probs = seq(0, 1, 0.05))["95%"] / max(y_vals) 
  lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
  
  # Jelmagyarázat
  ymax <- max(hist$counts)
  xpos <- teteje[1] * 0.65
  ypos <- ymax * 0.95
  
  segments(x0 = xpos, y0 = ypos, x1 = xpos + 280, y1 = ypos, col = "darkgreen", lwd = 2)
  text(x = xpos + 290, y = ypos, labels = "Várható eloszlás (2005–2016 alapján)", adj = 0)
  
  ypos <- ypos - 40
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 30, col = "#ca0020", border = NA)
  text(x = xpos + 290, y = ypos, labels = "Jövedelemcsomósodás", adj = 0)
  
  ypos <- ypos - 35
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 10, col = "darkblue", border = NA)
  text(x = xpos + 290, y = ypos, labels = "Eloszlástöbblet HY140G beszámítása miatt", adj = 0, col = "darkblue")
  
  ypos <- ypos - 35
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 10, col = "lightblue", border = "lightblue")
  text(x = xpos + 290, y = ypos, labels = "Eloszlás hiátusa HY140G beszámítása miatt", adj = 0)
}





for (ev in 2014:2021) {
  print(HX090javhist(ev)) }





## 2.4. Ha csak a küszöbátlépőknél tekintünk el a HY140G negatív értékétől, 
#hogyan változik az eloszlás?


### összes HY140G-n való változás

#HX090 és HY020 jav2: csak a küszöbátlépők esetén korrigálok 

H$HY020jav2 <- H$HY020
H$HY020jav2[H$HY140G < 0 ] <- H$HY020[H$HY140G < 0 ] + H$HY140G[H$HY140G < 0 ]
H$HX090jav2 <- H$HY020jav2 / H$HX050

H$utazo <- 0
H$utazo[H$HX090 > H$AROP60tresh & 
          H$HX090jav2 < H$AROP60tresh] <- 1

# aki nem utazo, ott a jav2-t visszarakom az eredetire
H$HX090jav2[H$utazo==0] <- H$HX090[H$utazo==0]


HX090jav2hist <- function(ev) {
  # Histogram határai
  kuszob <- medianokEU27$AROP60tresh[medianokEU27$y == ev & medianokEU27$c == "HU"]
  teteje <- medianokEU27$also90[medianokEU27$y == ev & medianokEU27$c == "HU"]
  
  # Histogram generálása
  hist <- hist(H$HX090[H$y == ev & H$HX090 < teteje[1] & H$HX090 > -500],
               plot = FALSE,
               breaks = seq(-500, round(teteje[1] / 100 + 1) * 100, 100)) 
  histJAV <- hist(H$HX090jav2[H$y == ev & H$HX090jav2 < teteje[1] & H$HX090jav2 > -500],
                  plot = FALSE,
                  breaks = seq(-500, round(teteje[1] / 100 + 1) * 100, 100))
  # Kiemelt területek definíciója
  highlight_ranges <- list(
    "2017" = list(c(-500, 0)),
    "2018" = list(c(3200, 3300)),
    "2019" = list(c(3500, 3800)),
    "2020" = list(c(3900, 4000)),
    "2021" = list(c(4000, 4100)),                  
    "2022" = list(c(0, 1)),
    "2023" = list(c(7800, 7900), c(4400, 4500))
  )
  
  # Oszlopszínek: alaplila, piros, stb.
  colors <- rep("#999999", length(hist$mids))
  colors[hist$mids < kuszob[1]] <- "#984ea3"
  ranges <- highlight_ranges[[as.character(ev)]]
  if (!is.null(ranges)) {
    for (range in ranges) {
      colors[hist$mids >= range[1] & hist$mids <= range[2]] <- "#ca0020"
    }
  }
  
  # Alap histogram kirajzolása
  plot(hist, col = colors, main = str_c(ev,": küszöbátlépők helyváltoztatása"),
       xlab = "Egy fogyasztási egységre jutó jövedelem, 100 eurós sávokban", 
       ylab = "Mintabeli esetek száma a 100 eurós sávban")
  
  abline(v = kuszob[1], col = "#377eb8", lwd = 2)
  text(x = kuszob[1], 
       y = max(hist$counts) * 0.85, 
       labels = paste("szegénységi küszöb", round(kuszob[1]), "€"), 
       pos = 2, col = "#377eb8", cex = 1)
  
  
  
  # Előkészítés a sávokra
  szxleft <- hist$breaks[-length(hist$breaks)]
  szxright <- szxleft + 100
  szcount <- hist$counts
  szcountJAV <- histJAV$counts
  
  szinezni <- data.frame(szxleft = szxleft, # bal oldali határoló x érték
                         szxright = szxright, # jobb oldali határoló x érték
                         szcount = szcount,
                         szcountJAV = szcountJAV) # y érték (milyen magasra megy ebben a sávban histogram (javítás nélkül))
  
  # Eredeti oszlopsáv színezés csúszókra (sötétkék)
  szinezni$pluszminusz <- szinezni$szcount - szinezni$szcountJAV #ha negatív: hiányt: világossal ha pozitív: többletet sötéttel
  
  for (i in 1:nrow(szinezni)) {
    if (szinezni$pluszminusz[i] > 0) {
      rect(xleft = szinezni$szxleft[i],
           xright = szinezni$szxright[i],
           ybottom = szinezni$szcount[i],
           ytop = szinezni$szcountJAV[i],
           col = "darkblue",
           border = NA)
    }
  }
  
  # világoskék színezés
  for (i in 1:nrow(szinezni)) {
    if (szinezni$pluszminusz[i] < 0) {
      rect(xleft = szinezni$szxleft[i],
           xright = szinezni$szxright[i],
           ybottom = szinezni$szcount[i],
           ytop = szinezni$szcountJAV[i] ,
           col = "lightblue",  
           border = NA)
    }
  }
  
  # Várható eloszlás (zöld vonal)
  x_vals <- seq(0, teteje[1], length.out = 16)
  y_vals <- emp$y
  y_vals <- y_vals * quantile(hist$counts, probs = seq(0, 1, 0.05))["95%"] / max(y_vals) 
  lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
  
  # Jelmagyarázat
  ymax <- max(hist$counts)
  xpos <- teteje[1] * 0.55
  ypos <- ymax * 0.95
  
  segments(x0 = xpos, y0 = ypos, x1 = xpos + 280, y1 = ypos, col = "darkgreen", lwd = 2)
  text(x = xpos + 390, y = ypos, labels = "Várható eloszlás", adj = 0)
  
  ypos <- ypos - 40
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 30, col = "#ca0020", border = NA)
  text(x = xpos + 290, y = ypos, labels = "Jövedelemtorlódás", adj = 0)
  
  ypos <- ypos - 35
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 10, col = "darkblue", border = NA)
  text(x = xpos + 290, y = ypos, labels = "Eloszlástöbblet negatív adó és járulék miatt", adj = 0, col = "darkblue")
  
  ypos <- ypos - 35
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 10, col = "lightblue", border = "lightblue")
  text(x = xpos + 290, y = ypos, labels = "Eloszlás hiátusa negatív adó és járulék miatt", adj = 0,col= "cornflowerblue")
}

HX090jav2hist(2018)


for (ev in 2014:2021) {
  print(HX090jav2hist(ev))
}

### kék ábra angolul (cím nélkül)
HX090jav2histEN <- function(ev) {
  # Histogram határai
  kuszob <- medianokEU27$AROP60tresh[medianokEU27$y == ev & medianokEU27$c == "HU"]
  teteje <- medianokEU27$also90[medianokEU27$y == ev & medianokEU27$c == "HU"]
  
  # Histogram generálása
  hist <- hist(H$HX090[H$y == ev & H$HX090 < teteje[1] & H$HX090 > -500],
               plot = FALSE,
               breaks = seq(-500, round(teteje[1] / 100 + 1) * 100, 100)) 
  histJAV <- hist(H$HX090jav2[H$y == ev & H$HX090jav2 < teteje[1] & H$HX090jav2 > -500],
                  plot = FALSE,
                  breaks = seq(-500, round(teteje[1] / 100 + 1) * 100, 100))
  # Kiemelt területek definíciója
  highlight_ranges <- list(
    "2017" = list(c(-500, 0)),
    "2018" = list(c(3200, 3300)),
    "2019" = list(c(3500, 3800)),
    "2020" = list(c(3900, 4000)),
    "2021" = list(c(4000, 4100)),                  
    "2022" = list(c(0, 1)),
    "2023" = list(c(7800, 7900), c(4400, 4500))
  )
  
  # Oszlopszínek: alaplila, piros, stb.
  colors <- rep("#999999", length(hist$mids))
  colors[hist$mids < kuszob[1]] <- "#984ea3"
  ranges <- highlight_ranges[[as.character(ev)]]
  if (!is.null(ranges)) {
    for (range in ranges) {
      colors[hist$mids >= range[1] & hist$mids <= range[2]] <- "#ca0020"
    }
  }
  
  # Alap histogram kirajzolása
  plot(hist, col = colors, main = NULL,
       xlab = "Equivalised disposable household income in Euros, in €100 intervals", 
       ylab = "Number of cases")
  
  abline(v = kuszob[1], col = "#377eb8", lwd = 2)
  text(x = kuszob[1], 
       y = max(hist$counts) * 0.85, 
       labels = paste("poverty threshold", round(kuszob[1]), "€"), 
       pos = 2, col = "#377eb8", cex = 1)
  
  
  
  # Előkészítés a sávokra
  szxleft <- hist$breaks[-length(hist$breaks)]
  szxright <- szxleft + 100
  szcount <- hist$counts
  szcountJAV <- histJAV$counts
  
  szinezni <- data.frame(szxleft = szxleft, # bal oldali határoló x érték
                         szxright = szxright, # jobb oldali határoló x érték
                         szcount = szcount,
                         szcountJAV = szcountJAV) # y érték (milyen magasra megy ebben a sávban histogram (javítás nélkül))
  
  # Eredeti oszlopsáv színezés csúszókra (sötétkék)
  szinezni$pluszminusz <- szinezni$szcount - szinezni$szcountJAV #ha negatív: hiányt: világossal ha pozitív: többletet sötéttel
  
  for (i in 1:nrow(szinezni)) {
    if (szinezni$pluszminusz[i] > 0) {
      rect(xleft = szinezni$szxleft[i],
           xright = szinezni$szxright[i],
           ybottom = szinezni$szcount[i],
           ytop = szinezni$szcountJAV[i],
           col = "darkblue",
           border = NA)
    }
  }
  
  # világoskék színezés
  for (i in 1:nrow(szinezni)) {
    if (szinezni$pluszminusz[i] < 0) {
      rect(xleft = szinezni$szxleft[i],
           xright = szinezni$szxright[i],
           ybottom = szinezni$szcount[i],
           ytop = szinezni$szcountJAV[i] ,
           col = "lightblue",  
           border = NA)
    }
  }
  
  # Várható eloszlás (zöld vonal)
  x_vals <- seq(0, teteje[1], length.out = 16)
  y_vals <- emp$y
  y_vals <- y_vals * quantile(hist$counts, probs = seq(0, 1, 0.05))["95%"] / max(y_vals) 
  lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
  
  # Jelmagyarázat
  ymax <- max(hist$counts)
  xpos <- teteje[1] * 0.42
  ypos <- ymax * 0.95
  
  segments(x0 = xpos, y0 = ypos, x1 = xpos + 280, y1 = ypos, col = "darkgreen", lwd = 2)
  text(x = xpos + 390, 
       y = ypos, 
       labels = "Expected distribution (based on years 2005-2016)", 
       adj = 0,
       cex=0.8)
  
  ypos <- ypos - 40
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 30, col = "#ca0020", border = NA)
  text(x = xpos + 290, y = ypos, labels = "Income clustering", adj = 0,cex=0.8)
  
  ypos <- ypos - 35
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 10, col = "darkblue", border = NA)
  text(x = xpos + 290, y = ypos, 
       labels = "Distribution surplus due to negative taxes and contributions", adj = 0, 
       col = "darkblue", 
       cex=0.8)
  
  ypos <- ypos - 35
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 10, 
       col = "lightblue", border = "lightblue",cex=0.8)
  text(x = xpos + 290, y = ypos, 
       labels = "Distribution gap due to negative taxes and contributions", 
       adj = 0,col= "cornflowerblue",cex=0.8)
}

HX090jav2histEN(2018)


# küszöb környékére eső esetek:

H$m050kornyek <- 0
H$m060kornyek <- 0
H$m070kornyek <- 0
H$m100kornyek <- 0

H$m050kornyek[H$HX090>H$HX090median*0.48 & H$HX090<H$HX090median*0.52] <- 1
H$m060kornyek[H$HX090>H$HX090median*0.58 & H$HX090<H$HX090median*0.62] <- 1
H$m070kornyek[H$HX090>H$HX090median*0.68 & H$HX090<H$HX090median*0.72] <- 1
H$m100kornyek[H$HX090>H$HX090median*0.98 & H$HX090<H$HX090median*1.02] <- 1


Hegyfo <- subset(H,HX060==5)
table(Hegyfo$y)
table(Hegyfo$y,Hegyfo$m060kornyek)
table(Hegyfo$y,Hegyfo$HY140Gneg)
table(Hegyfo$y,Hegyfo$bruttonettobaj)

H %>% filter(y==2020) %>% filter(HX090==3996.3110974) %>% count(HX060)


### egyfősre a küszöbátlépő utazók:

HX090jav2histEGYFO <- function(ev) {
  # Histogram határai
  kuszob <- medianokEU27$AROP60tresh[medianokEU27$y == ev & medianokEU27$c == "HU"]
  teteje <- medianokEU27$also90[medianokEU27$y == ev & medianokEU27$c == "HU"]
  
  # Histogram generálása
  hist <- hist(Hegyfo$HX090[Hegyfo$y == ev & Hegyfo$HX090 < teteje[1] & Hegyfo$HX090 > -500],
               plot = FALSE,
               breaks = seq(-500, round(teteje[1] / 100 + 1) * 100, 100)) 
  histJAV <- hist(Hegyfo$HX090jav2[Hegyfo$y == ev & Hegyfo$HX090jav2 < teteje[1] & Hegyfo$HX090jav2 > -500],
                  plot = FALSE,
                  breaks = seq(-500, round(teteje[1] / 100 + 1) * 100, 100))
  # Kiemelt területek definíciója
  highlight_ranges <- list(
    "2017" = list(c(-500, 0)),
    "2018" = list(c(3200, 3300)),
    "2019" = list(c(3500, 3800)),
    "2020" = list(c(3900, 4000)),
    "2021" = list(c(4000, 4100)),                  
    "2022" = list(c(0, 1)),
    "2023" = list(c(7800, 7900), c(4400, 4500))
  )
  
  # Oszlopszínek: alaplila, piros, stb.
  colors <- rep("#999999", length(hist$mids))
  colors[hist$mids < kuszob[1]] <- "#984ea3"
  ranges <- highlight_ranges[[as.character(ev)]]
  if (!is.null(ranges)) {
    for (range in ranges) {
      colors[hist$mids >= range[1] & hist$mids <= range[2]] <- "#ca0020"
    }
  }
  
  # Alap histogram kirajzolása
  plot(hist, col = colors, main = str_c("EGYFŐS ",ev,": küszöbátlépők helyváltoztatása"),
       xlab = "Egy fogyasztási egységre jutó jövedelem, 100 eurós sávokban", 
       ylab = "Esetek száma a 100 eurós sávban")
  
  abline(v = kuszob[1], col = "#377eb8", lwd = 2)
  text(x = kuszob[1], 
       y = max(hist$counts) * 0.85, 
       labels = paste("szegénységi küszöb", round(kuszob[1]), "€"), 
       pos = 2, col = "#377eb8", cex = 1.4)
  
  
  
  # Előkészítés a sávokra
  szxleft <- hist$breaks[-length(hist$breaks)]
  szxright <- szxleft + 100
  szcount <- hist$counts
  szcountJAV <- histJAV$counts
  
  szinezni <- data.frame(szxleft = szxleft, # bal oldali határoló x érték
                         szxright = szxright, # jobb oldali határoló x érték
                         szcount = szcount,
                         szcountJAV = szcountJAV) # y érték (milyen magasra megy ebben a sávban histogram (javítás nélkül))
  
  # Eredeti oszlopsáv színezés csúszókra (sötétkék)
  szinezni$pluszminusz <- szinezni$szcount - szinezni$szcountJAV #ha negatív: hiányt: világossal ha pozitív: többletet sötéttel
  
  for (i in 1:nrow(szinezni)) {
    if (szinezni$pluszminusz[i] > 0) {
      rect(xleft = szinezni$szxleft[i],
           xright = szinezni$szxright[i],
           ybottom = szinezni$szcount[i],
           ytop = szinezni$szcountJAV[i],
           col = "darkblue",
           border = NA)
    }
  }
  
  # világoskék színezés
  for (i in 1:nrow(szinezni)) {
    if (szinezni$pluszminusz[i] < 0) {
      rect(xleft = szinezni$szxleft[i],
           xright = szinezni$szxright[i],
           ybottom = szinezni$szcount[i],
           ytop = szinezni$szcountJAV[i] ,
           col = "lightblue",  
           border = NA)
    }
  }
  
  # Várható eloszlás (zöld vonal)
  x_vals <- seq(0, teteje[1], length.out = 16)
  y_vals <- emp$y
  y_vals <- y_vals * quantile(hist$counts, probs = seq(0, 1, 0.05))["95%"] / max(y_vals) 
  lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
  
  # Jelmagyarázat
  ymax <- max(hist$counts)
  xpos <- teteje[1] * 0.65
  ypos <- ymax * 0.95
  
  segments(x0 = xpos, y0 = ypos, x1 = xpos + 280, y1 = ypos, col = "darkgreen", lwd = 2)
  text(x = xpos + 290, y = ypos, labels = "Várható eloszlás (2005–2016 alapján)", adj = 0)
  
  ypos <- ypos - 40
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 30, col = "#ca0020", border = NA)
  text(x = xpos + 290, y = ypos, labels = "Jövedelemcsomósodás", adj = 0)
  
  ypos <- ypos - 35
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 10, col = "darkblue", border = NA)
  text(x = xpos + 290, y = ypos, labels = "Eloszlástöbblet HY140G beszámítása miatt", adj = 0, col = "darkblue")
  
  ypos <- ypos - 35
  rect(xleft = xpos, ybottom = ypos - 10, xright = xpos + 120, ytop = ypos + 10, col = "lightblue", border = "lightblue")
  text(x = xpos + 290, y = ypos, labels = "Eloszlás hiátusa HY140G beszámítása miatt", adj = 0, col="cornflowerblue")
}



for (ev in 2014:2021) {
  print(HX090jav2histEGYFO(ev))
}



## 2.5. Mekkora az "utazó" / "küszöbátlépő" esetek aránya, akik a HY140G vonatán utaznak át a szegénységi küszöbön?


H120130140ugyek <- H %>% group_by(y) %>%
  summarize(bruttonettobaj = mean(bruttonettobaj,na.rm=T,vartype=NULL),
            HY120N_NA = mean(HY120N_NA,na.rm=T,vartype=NULL),
            HY130N_NA = mean(HY130N_NA,na.rm=T,vartype=NULL),
            HY140N_NA = mean(HY140N_NA,na.rm=T,vartype=NULL),
            HY120G_NA = mean(HY120G_NA,na.rm=T,vartype=NULL),
            HY130G_NA = mean(HY130G_NA,na.rm=T,vartype=NULL),
            HY140G_NA = mean(HY140G_NA,na.rm=T,vartype=NULL),
            HY120Gneg = mean(HY120Gneg,na.rm=T,vartype=NULL),
            HY130Gneg = mean(HY130Gneg,na.rm=T,vartype=NULL),
            HY140Gneg = mean(HY140Gneg,na.rm=T,vartype=NULL),
            utazo=mean(utazo,na.rm=T,vartype=NULL))

H120130140ugyek %>% select(y,HY140Gneg,utazo)
barplot(H120130140ugyek$utazo,names.arg=2005:2023,cex.axis=1,cex.names=1)



## 2.6. Melyik évben milyen háztartástípusokat érint a küszöbátlépés?




H$HX060fact <- factor(H$HX060,levels = c(5,6,7,8,9,10,11,12,13,16),labels=c("One person household",
                                                                            "2 adults <65 years",
                                                                            "2 adults 1>=65 years",
                                                                            "Other households no children",
                                                                            "Single parent 1+ children",
                                                                            "2 adults, one dependent child",
                                                                            "2 adults, two dependent children",
                                                                            "2 adults, 3+ dependent children",
                                                                            "Other households with  children",
                                                                            "Other (Leaken excluded"))

# évenként, háztartástípusonként a küszöbátlépők száma
addmargins(table(H$HX060fact[H$utazo==1], H$y[H$utazo==1]), margin = c(1,2))

# ezek aránya az adott év összes ilyen típusú háztartásához viszonyítva
# addmargins(table(H$HX060fact[H$y %in% 2014:2021], H$y[H$y %in% 2014:2021]), margin = c(1,2))
# sz <- addmargins(table(H$HX060fact[H$utazo==1], H$y[H$utazo==1]), margin = c(1,2))
# n <- addmargins(table(H$HX060fact[H$y %in% 2014:2021], H$y[H$y %in% 2014:2021]), margin = c(1,2))
# 
# round(sz/n,2)





## 2.7. Alternatív szegénységbecslés


H$HY020jav1 <- H$HY020
H$HY020jav1[H$HY140G < 0] <- H$HY020[H$HY140G < 0] + H$HY140G[H$HY140G < 0]

H$HX090jav1 <- H$HY020jav1 / H$HX050

H$AROP60jav1 <- 0
H$AROP60jav1[H$HX090jav1 < H$AROP60tresh] <- 1

H$AROP60regi <- 0
H$AROP60regi[H$HX090 < H$AROP60tresh] <- 1


H %>% group_by(y) %>% summarize(rata_jav = mean(AROP60jav1),
                                rata_regi = mean(AROP60regi))


# súlyozott ráta lekéréséhez survey design
H_survey <- svydesign(ids=~1,weights=~weight,data=H)
H_srvyr <- as_survey_design(.data=H,weights=weight)

RATES <- H_srvyr  %>%group_by(y) %>% 
  summarize(AROP60rateREGI      = survey_mean(AROP60regi,na.rm=T,vartype=NULL),
            AROP60rateJAV      = survey_mean(AROP60jav1,na.rm=T,vartype=NULL))
RATES
# mediánok

medianokHregiuj <- H_srvyr   %>% group_by(y) %>% 
  summarize(HX090medianREGI = survey_median(HX090,na.rm=T,vartype=NULL),
            HX090medianJAV = survey_median(HX090jav1,na.rm=T,vartype=NULL))
medianokHregiuj

medianokHregiuj$medianDIFF <- medianokHregiuj$HX090medianJAV - medianokHregiuj$HX090medianREGI
medianokHregiuj


H120130140ugyek %>% select(y,HY140Gneg)

H <- left_join(H,medianokHregiuj,by=c("y"))

H$AROP60_H140Gkorrekcioval <- 0
H$AROP60_H140Gkorrekcioval[H$HX090jav1 < H$HX090medianJAV*0.6] <- 1

table(H$y,H$AROP60_H140Gkorrekcioval)

# súlyozott ráta lekéréséhez survey design
H_survey <- svydesign(ids=~1,weights=~weight,data=H)
H_srvyr <- as_survey_design(.data=H,weights=weight)

RATES <- H_srvyr  %>%group_by(y) %>% 
  summarize(AROP60rateREGI      = survey_mean(AROP60regi,na.rm=T,vartype=NULL),
            AROP60rateJAV      = survey_mean(AROP60jav1,na.rm=T,vartype=NULL),
            AROP60rateJAVmedianIS = survey_mean(AROP60_H140Gkorrekcioval,na.rm=T,vartype=NULL))
RATES

plot(RATES$y,RATES$AROP60rateREGI,
     type = "l",ylim=c(0,0.2))
lines(RATES$y,RATES$AROP60rateJAV,col="blue")
lines(RATES$y,RATES$AROP60rateREGI,col="black")
lines(RATES$y,RATES$AROP60rateJAVmedianIS,col="red")
lines(RATES$y,RATES$AROP60rateREGI,col="black")


############### szegénységi rés gyermekeknél
# RB081: referenciaidőszak végén betöltött  EZeknél egy csomó missing van
# RB082: interjú idején betöltött év Ezeknél egy csomó missing van


H$kor <- H$RB010 - H$szulev

H_srvyr <- as_survey_design(.data=H,weights=weight)

GYRESEK <- H_srvyr   %>% 
  filter(AROP60regi ==1) %>%
  filter(kor < 18) %>% 
  group_by(y) %>% 
  summarize(SZEGHX090medianREGIgy = survey_median(HX090,na.rm=T,vartype=NULL),
            SZEGHX090medianJAVgy = survey_median(HX090jav1,na.rm=T,vartype=NULL))

OGYER <- H_srvyr   %>% 
  filter(kor < 18) %>% 
  group_by(y) %>% 
  summarize(ogyerHX090medianREGIgy = survey_median(HX090,na.rm=T,vartype=NULL),
            ogyerX090medianJAVgy = survey_median(HX090jav1,na.rm=T,vartype=NULL))

NSZGYER <- H_srvyr   %>% 
  filter(AROP60regi ==0) %>%
  filter(kor < 18) %>% 
  group_by(y) %>% 
  summarize(NSZEGHX090medianREGIgy = survey_median(HX090,na.rm=T,vartype=NULL),
            NSZEGHX090medianJAVgy = survey_median(HX090jav1,na.rm=T,vartype=NULL))


TRES <- H_srvyr   %>%                        
  filter(AROP60regi ==1) %>%
  group_by(y) %>% 
  summarize(SZEGHX090medianREGIo = survey_median(HX090,na.rm=T,vartype=NULL),
            SZEGHX090medianJAVo = survey_median(HX090jav1,na.rm=T,vartype=NULL))

GYRESEK
TRES

RESEK <- left_join(medianokHregiuj,GYRESEK,by="y")
RESEK <- left_join(RESEK,OGYER,by="y")
RESEK <- left_join(RESEK,NSZGYER,by="y")
RESEK <- left_join(RESEK,TRES,by="y")

RESEK



names(RESEK)
RESEK$kuszob <- RESEK$HX090medianREGI *0.6
RESEK$kuszobjav <- RESEK$HX090medianJAV *0.6

RESEK$resTELJES <-  (RESEK$kuszob - RESEK$SZEGHX090medianREGIo) / RESEK$kuszob
RESEK$resGYEREK <- (RESEK$kuszob - RESEK$SZEGHX090medianREGIgy) / RESEK$kuszob

RESEK$resTELJESjav <-  (RESEK$kuszobjav - RESEK$SZEGHX090medianJAVo) / RESEK$kuszobjav
RESEK$resGYEREKjav <- (RESEK$kuszobjav - RESEK$SZEGHX090medianJAVgy) / RESEK$kuszobjav


write.csv2(RESEK,file="C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/reszeredmenyek/resek.csv")

## 2.8. PY010 és PY100 változók eloszlása 

### 2.8.1. Melyik évben melyik változót lehet használni? Mennyi a 0-k aránya? A pozitív értékek aránya?


# melyik évben melyik változó van egyáltalán feltöltve:

H$PY010N_NA <- is.na(H$PY010N)
H$PY010G_NA <- is.na(H$PY010G)
H$PY100N_NA <- is.na(H$PY100N)
H$PY100G_NA <- is.na(H$PY100G)

H$PY010N_0 <- 0
H$PY010G_0 <- 0
H$PY100N_0 <- 0
H$PY100G_0 <- 0

H$PY010N_0[H$PY010N==0] <- 1
H$PY010G_0[H$PY010G==0] <- 1
H$PY100N_0[H$PY100N==0] <- 1
H$PY100G_0[H$PY100G==0] <- 1

H$PY010Gpoz <- 0
H$PY010Gpoz[H$PY010G >0] <- 1
H$PY100Gpoz <- 0
H$PY100Gpoz[H$PY100G > 0] <- 1
H$PY010Npoz <- 0
H$PY010Npoz[H$PY010N >0] <- 1
H$PY100Npoz <- 0
H$PY100Npoz[H$PY100N > 0] <- 1



egyenijovedelmek <- H %>% group_by(y) %>%
  summarize(PY010N_NA = mean(PY010N_NA,na.rm=T,vartype=NULL),
            PY010G_NA = mean(PY010G_NA,na.rm=T,vartype=NULL),
            PY100N_NA = mean(PY100N_NA,na.rm=T,vartype=NULL),
            PY100G_NA = mean(PY100G_NA,na.rm=T,vartype=NULL),
            PY010N_0  = mean(PY010N_0 ,na.rm=T,vartype=NULL),
            PY010G_0  = mean(PY010G_0,na.rm=T,vartype=NULL),
            PY100N_0  = mean(PY100N_0,na.rm=T,vartype=NULL),
            PY100G_0 =  mean(PY100G_0,na.rm=T,vartype=NULL),
            PY010Gpoz = mean(PY010Gpoz,na.rm=T,vartype=NULL),
            PY100Gpoz = mean(PY100Gpoz,na.rm=T,vartype=NULL),
            PY010Npoz = mean(PY010Npoz,na.rm=T,vartype=NULL),
            PY100Npoz = mean(PY100Npoz,na.rm=T,vartype=NULL))
egyenijovedelmek
write.csv2(egyenijovedelmek,file="C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/reszeredmenyek/egyenijovedelmek.csv")


# Tanulság: PY010G és PY100G idősorát lehet végigkövetni. 
# A 0 értékek aránya a PY010G-ben 2018-ban meglepően alacsony (szokásos 40-45% helyett csak 15%), ezzel szemben a pozitív érték a szokásos 40% helyett 67%.

### 2.8.2. PY010G  Employee cash or near cash income  histogramjai

# -   Sötétzöld vonal: minimálbér (éves bruttó), 
# -   narancs: garantált bérminimum (éves bruttó), 
# -   lila: közfoglalkoztatotti bérminimum (éves bruttó)


PY010G_hist_berekkel <- function(ev){
  #kuszob <- medianok$AROP60tresh[medianok$y == ev]
  teteje <- medianokEU27$also90[medianokEU27$y == ev & medianokEU27$c=="HU"]
  minimalberB <- miniber$minimalberHAVIbruttoFT[miniber==ev] *12 / miniber$euro[miniber==ev] 
  garberminB <-  miniber$garantaltberminimumHAVIbruttoFT[miniber==ev] *12 / miniber$euro[miniber==ev] 
  kozfolgB <-  miniber$kozfolgberminimumHAVIbruttoFT[miniber==ev] *12 / miniber$euro[miniber==ev] 
  n <- length(H$PY010G[H$y == ev & 
                         H$PY010G < teteje[1] & 
                         H$PY010G > 0])
  #Hisztogram elmentése, hogy lehessen szerkeszteni
  hist <- hist(H$PY010G[H$y == ev & 
                          H$PY010G < teteje[1] & 
                          H$PY010G > 0],
               plot = FALSE,
               breaks = seq(0,
                            round(teteje[1] / 100 + 1) * 100,
                            100))
  #Ábrázolás
  plot(hist, col = "grey", main = str_c(ev," PY010G>0",", n=",n),
       xlab = "PY010G: 100 eurós sávokban",
       ylab="Esetek száma a 100 eurós sávban")
  abline(v = minimalberB[1], col = "darkgreen", lwd=2, lty=2)
  abline(v = garberminB[1], col = "orange", lwd=2, lty=2)
  abline(v = kozfolgB[1], col = "purple", lwd=2, lty=2)
}  

PY010G_hist_berekkel(2005)

# Hisztogramok generálása 2005-2023 évekre
for (ev in 2005:2023) {
  PY010G_hist_berekkel(ev)
}



# Kiugróan sok eset 2018,2019,2020-ban
# -   2018: 200-300 euro
# -   2019: 0-100 euro
# -   2020: 0-100 euro



# kiugróan sok eset:
#2018
hist2018 <- hist(H$PY010G[H$y == 2018 & 
                            H$PY010G < medianokEU27$also90[medianokEU27$y == 2018 & medianokEU27$c=="HU"] & 
                            H$PY010G > 0],
                 plot = FALSE,
                 breaks = seq(0,round(medianokEU27$also90[medianokEU27$y == 2018 & medianokEU27$c=="HU"] / 100 + 1) * 100,100))
# legnagyobb esetszám (vs teljes érvényes esetszám)
max(hist2018$counts)
max(hist2018$counts) /12374
# Legnagyobb gyakoriság alsó határa
hist2018$breaks[which.max(hist2018$counts)]
# 2019
hist2019 <- hist(H$PY010G[H$y == 2019 & 
                            H$PY010G < medianokEU27$also90[medianokEU27$y == 2019 & medianokEU27$c=="HU"] & 
                            H$PY010G > 0],
                 plot = FALSE,
                 breaks = seq(0,round(medianokEU27$also90[medianokEU27$y == 2019 & medianokEU27$c=="HU"] / 100 + 1) * 100,100))
# legnagyobb esetszám (vs teljes érvényes esetszám)
max(hist2019$counts)
max(hist2019$counts) /8089
# Legnagyobb gyakoriság alsó határa
hist2019$breaks[which.max(hist2019$counts)]
# 2020
hist2020 <- hist(H$PY010G[H$y == 2020 & 
                            H$PY010G < medianokEU27$also90[medianokEU27$y == 2020 & medianokEU27$c=="HU"] & 
                            H$PY010G > 0],
                 plot = FALSE,
                 breaks = seq(0,round(medianokEU27$also90[medianokEU27$y == 2020 & medianokEU27$c=="HU"] / 100 + 1) * 100,100))
# legnagyobb esetszám (vs teljes érvényes esetszám)
max(hist2020$counts)
max(hist2020$counts) / 6887
# Legnagyobb gyakoriság alsó határa
hist2020$breaks[which.max(hist2019$counts)]



### 2.8.3. PY100G: Old-age benefits histogramjai 2005-2023



PY100G_hist <- function(ev){
  #kuszob <- medianok$AROP60tresh[medianok$y == ev]
  teteje <- medianokEU27$also90[medianokEU27$y == ev & medianokEU27$c=="HU"]
  n <- length(H$PY100G[H$y == ev & 
                         H$PY100G < teteje[1] & 
                         H$PY100G > 0])
  #Hisztogram elmentése, hogy lehessen szerkeszteni
  hist <- hist(H$PY100G[H$y == ev & 
                          H$PY100G < teteje[1] & 
                          H$PY100G > 0],
               plot = FALSE,
               breaks = seq(0,
                            round(teteje[1] / 100 + 1) * 100,
                            100))
  #Ábrázolás
  plot(hist, col = "grey", main = str_c(ev," PY100G>0",", n=",n),
       xlab = "PY100G: 100 eurós sávokban",
       ylab="Esetek száma a 100 eurós sávban")
}  



# Hisztogramok generálása 2005-2023 évekre
for (ev in 2005:2023) {
  PY100G_hist(ev)
}

#### rések

# árfolyamok

arf <- H %>% group_by(y) %>%
  summarize(arf = mean(HX010,na.rm=T,vartype=NULL))


write.csv2(arf,file="C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/reszeredmenyek/arf.csv")


#### 2020 egyfős ábra adatokkal
sEGYFO <- Hegyfo

HX090_hist_minimalber_egyfos_teljeseloszlas <- function(ev){
  kuszob <- medianokEU27$AROP60tresh[medianokEU27$y == ev & medianokEU27$c=="HU"]
  minimalber <- miniber$minimalberEVESnettoEUR[miniber$y == ev ]
  teteje <- medianokEU27$also90[medianokEU27$y == ev & medianokEU27$c=="HU"]
  #Hisztogram elmentése, hogy lehessen szerkeszteni
  hist <- hist(sEGYFO$HX090[sEGYFO$y == ev & sEGYFO$HX090 < teteje[1] & sEGYFO$HX090 > -500],
               plot = FALSE,
               breaks = seq(-500,
                            round(teteje[1] / 100 + 1) * 100,
                            100))
  # Egyedi színezési tartományok év szerint
  highlight_ranges <- list(
    "2017" = list(c(-500,0)),
    "2018" = list(c(0, 1)),
    "2019" = list(c(0,1)),
    "2020" = list(c(3900,4000)),
    "2021" = list(c(0,1)),                  
    "2022" = list(c(0,1)),
    "2023" = list(c(4400,4500)))
  # Alapértelmezett színezés: minden szürke
  colors <- rep("#999999", length(hist$mids))
  # Szegénységi küszöb alatti oszlopok kékre állítása
  colors[hist$mids < kuszob[1]] <- "#984ea3"
  # Kiemelt tartományok pirosra állítása
  ranges <- highlight_ranges[[as.character(ev)]]
  if (!is.null(ranges)) {
    for (range in ranges) {
      colors[hist$mids >= range[1] & hist$mids <= range[2]] <- "#ca0020"
    }
  }
  #Ábrázolás
  plot(hist, col = colors, main = NULL,
       xlab = "Éves nettó összjövedelem euróban, 100 eurós sávokban",
       yla="Esetek száma a 100 eurós sávban")
  abline(v = kuszob[1], col = "#377eb8", lwd=2)
  abline(v= minimalber[1],col="darkgreen",lty=2)

  # normálgörbe rárajzolása
  mu <- median(sEGYFO$HX090[sEGYFO$y == ev & sEGYFO$HX090 < teteje[1] & sEGYFO$HX090 > -500],na.rm=T) # medián legyen a várható érték
  x_vals <- seq(0, teteje[1], length.out = 18) # számsorozat a vonal generálásához
  y_vals <- HUempELOSZLASegyfo$y #normális eloszlás sűrűségfüggvénye
  y_vals <- y_vals * quantile(hist$counts,probs=seq(0,1,0.05))["95%"]  / max(y_vals) # felszorzott ávltozat picit emelve
  lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
  
}

HX090_hist_minimalber_egyfos_teljeseloszlas(2020)

# közfoglalkoztatotti bérminimum
abline(v=miniber$kozfolgberminEVESnettoEUR[miniber$y==2020],col="purple",lty=2)
#garantált bérminimum 
abline(v=miniber$garberminEVESnettoEUR[miniber$y==2020],col="orange",lty=2)

# feliratok:
text(x=6000,y=350,labels=paste("közfoglalkoztatotti bérminimum",
                               round(miniber$kozfolgberminEVESnettoEUR[miniber$y==2020]),
                               "€"),
     pos=4,col="purple")
text(x=6000,y=325,labels=paste("minimálbér",
                               round(miniber$minimalberEVESnettoEUR[miniber$y==2020]),
                               "€"),
     pos=4,col="darkgreen")
text(x=6000,y=300,labels=paste("szegénységi küszöb",
                               round(medianokEU27$AROP60tresh[medianokEU27$y==2020 & 
                                                                medianokEU27$c=="HU"]),
                               "€"),
     pos=4,col="#377eb8")
text(x=6000,y=275,labels=paste("jövedelemtorlódás a 3900-4000 € sávban"),pos=4,col="#ca0020")
text(x=6000,y=250,labels=paste("garantált bérminimum",
                               round(miniber$garberminEVESnettoEUR[miniber$y==2020]),
                               "€"),
     pos=4,col="orange")


##### ugyanez angolul

HX090_hist_minimalber_egyfos_teljeseloszlasEN <- function(ev){
  kuszob <- medianokEU27$AROP60tresh[medianokEU27$y == ev & medianokEU27$c=="HU"]
  minimalber <- miniber$minimalberEVESnettoEUR[miniber$y == ev ]
  teteje <- medianokEU27$also90[medianokEU27$y == ev & medianokEU27$c=="HU"]
  #Hisztogram elmentése, hogy lehessen szerkeszteni
  hist <- hist(sEGYFO$HX090[sEGYFO$y == ev & sEGYFO$HX090 < teteje[1] & sEGYFO$HX090 > -500],
               plot = FALSE,
               breaks = seq(-500,
                            round(teteje[1] / 100 + 1) * 100,
                            100))
  # Egyedi színezési tartományok év szerint
  highlight_ranges <- list(
    "2017" = list(c(-500,0)),
    "2018" = list(c(0, 1)),
    "2019" = list(c(0,1)),
    "2020" = list(c(3900,4000)),
    "2021" = list(c(0,1)),                  
    "2022" = list(c(0,1)),
    "2023" = list(c(4400,4500)))
  # Alapértelmezett színezés: minden szürke
  colors <- rep("#999999", length(hist$mids))
  # Szegénységi küszöb alatti oszlopok kékre állítása
  colors[hist$mids < kuszob[1]] <- "#984ea3"
  # Kiemelt tartományok pirosra állítása
  ranges <- highlight_ranges[[as.character(ev)]]
  if (!is.null(ranges)) {
    for (range in ranges) {
      colors[hist$mids >= range[1] & hist$mids <= range[2]] <- "#ca0020"
    }
  }
  #Ábrázolás
  plot(hist, col = colors, main = NULL,
       xlab ="Equivalised disposable household income in Euros, in €100 intervals",
       yla="Number of cases")
  abline(v = kuszob[1], col = "#377eb8", lwd=2)
  abline(v= minimalber[1],col="darkgreen",lty=2)
  
  # normálgörbe rárajzolása
  mu <- median(sEGYFO$HX090[sEGYFO$y == ev & sEGYFO$HX090 < teteje[1] & sEGYFO$HX090 > -500],na.rm=T) # medián legyen a várható érték
  x_vals <- seq(0, teteje[1], length.out = 18) # számsorozat a vonal generálásához
  y_vals <- HUempELOSZLASegyfo$y #normális eloszlás sűrűségfüggvénye
  y_vals <- y_vals * quantile(hist$counts,probs=seq(0,1,0.05))["95%"]  / max(y_vals) # felszorzott ávltozat picit emelve
  lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
  
}

HX090_hist_minimalber_egyfos_teljeseloszlasEN(2020)

# közfoglalkoztatotti bérminimum
abline(v=miniber$kozfolgberminEVESnettoEUR[miniber$y==2020],col="purple",lty=2)
#garantált bérminimum 
abline(v=miniber$garberminEVESnettoEUR[miniber$y==2020],col="orange",lty=2)

# feliratok:
text(x=5800,y=350,labels=paste("public workers minimum wage",
                               round(miniber$kozfolgberminEVESnettoEUR[miniber$y==2020]),
                               "€"),
     pos=4,col="purple")
text(x=5800,y=325,labels=paste("minimum wage",
                               round(miniber$minimalberEVESnettoEUR[miniber$y==2020]),
                               "€"),
     pos=4,col="darkgreen")
text(x=5800,y=300,labels=paste("poverty threshold",
                               round(medianokEU27$AROP60tresh[medianokEU27$y==2020 & 
                                                                medianokEU27$c=="HU"]),
                               "€"),
     pos=4,col="#377eb8")
text(x=5800,y=275,labels=paste("income clustering in the €3900–€4000 range"),pos=4,col="#ca0020")
text(x=5800,y=250,labels=paste("guaranteed minimum wage",
                               round(miniber$garberminEVESnettoEUR[miniber$y==2020]),
                               "€"),
     pos=4,col="orange")

#### Nyugdíjasok vonatkozásában
H %>% filter(y==2020) %>% filter(PY100G>0) %>% count(HY140Gneg)

H %>% filter(y==2020) %>% filter(PY100G>0) %>% count(kor)

H %>% filter(y==2020) %>% filter(PY100G>0) %>% count(utazo)

H %>% filter(y==2020) %>% filter(PY100G>0) %>% filter(HX060==5) %>% count(HY140Gneg)

H$tmpHX090_2000alatt <- 0
H$tmpHX090_2000alatt[H$HX090 < 2000] <- 1
H %>% filter(y==2023) %>% filter(HX060 %in% c(9,10,11,12,13)) %>% count(tmpHX090_2000alatt)

H %>% filter(y==2023) %>% filter(HX060 %in% c(9,10,11,12,13)) %>% filter(HX080==1) %>% count(tmpHX090_2000alatt)

# 2017, 2021, 2022

H %>% filter(y %in% c(2017,2021,2022)) %>% 
  group_by(y) %>% 
  filter(HX060 %in% c(9,10,11,12,13)) %>% 
  filter(HX080==1) %>% 
  count(tmpHX090_2000alatt)



H %>% filter(y %in% c(20)) %>% 
  group_by(y) %>% 
  filter(HX060 %in% c(9,10,11,12,13)) %>% 
  filter(HX080==1) %>% 
  count(tmpHX090_2000alatt)

H$bruttoEGYFOGY <- H$HY010 / H$HX050
H$nettoEGYFOGY <- H$HY020 / H$HX050
H$hy140gEGYFOGY <- H$HY140G / H$HX050

H %>% filter (y==2023) %>% 
  filter(HX060 %in% c(9,10,11,12,13)) %>% 
  filter(HX090 < HX090median) %>%
  group_by(HX080) %>% 
  summarize(hy140g=mean(HY140G),na.rm=T)

H$hy140gEGYFOGY <- H$HY140G / H$HX050

H %>% filter (y==2023) %>% 
  filter(HX060 %in% c(9,10,11,12,13)) %>% 
  filter(HX090 < HX090median) %>%
  group_by(HX080) %>% 
  summarize(hy140gEGYFOGY=mean(hy140gEGYFOGY),na.rm=T)

# igazából az ábrán lévők
H %>% filter (y==2023) %>% 
  filter(HX060 %in% c(9,10,11,12,13)) %>% 
  filter(bruttoEGYFOGY < HX090median) %>%
  group_by(HX080) %>% 
  summarize(hy140gEGYFOGYatl=mean(hy140gEGYFOGY),na.rm=T)

# igazából az ábrán lévők
H %>% filter (y==2023) %>% 
  filter(HX060 %in% c(9,10,11,12,13)) %>% 
  filter(bruttoEGYFOGY < HX090median) %>%
  group_by(HX080) %>% 
  summarize(hy140gEGYFOGYmed=median(hy140gEGYFOGY),na.rm=T)

# ha a medián alatti nettójúakat nézzük
H %>% filter (y==2023) %>% 
  filter(HX060 %in% c(9,10,11,12,13)) %>% 
  filter(nettoEGYFOGY < HX090median) %>%
  group_by(HX080) %>% 
  summarize(hy140gEGYFOGY=mean(hy140gEGYFOGY),na.rm=T)


H %>% filter (y==2023) %>% 
  filter(HX060 %in% c(9,10,11,12,13)) %>% 
  filter(HX090 < HX090median) %>%
  group_by(HX080) %>% 
  count(HX080)

H$hy140gEGYFOGYar <- H$hy140gEGYFOGY / H$bruttoEGYFOGY

H %>% filter (y==2023) %>% 
  filter(HX060 %in% c(9,10,11,12,13)) %>% 
  filter(bruttoEGYFOGY < HX090median) %>%
  group_by(HX080) %>% 
  summarize(hy140gEGYFOGYar=mean(hy140gEGYFOGYar),na.rm=T)
