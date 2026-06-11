rm(list=ls()) # enviroment kiürítése

library(tidyverse)
library(haven)

# kiindulópontja egy olyan mappa, amiben az összes magyar file egy helyen van:
setwd("C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/original/EU27fileokegymappaban")

# I list the csv files 
fnames <- list.files(getwd(),".csv")
# country- year combination like HU05 - Hungary 2005
cy <- names(table(str_sub(fnames,6,9)))
cy #ezekből főzünk
# fix part of the file name UDB_c
filefix <- names(table(str_sub(fnames,1,5)))


# FILEs and IDs
# household register (D) #DB030 household ID 
# household data     (H) #HB030: HOUSEHOLD ID
# personal register  (R) #RB030 personal ID   #RB040: CURRENT HOUSEHOLD ID
# personal data      (P) #PB030: PERSONAL ID only 16+

# kiválasztott változók
## D-ből
#DB030 household ID 
#DB040 region

## H-ból
#HB030: HOUSEHOLD ID
# HB010:év, 
# HX060 háztartástípus
# HX090 egy fogy egységre jutó jöv
# HY020 nettó összjöv
# HY020_F flag
# HY020_I imputation factor
# HY010 bruttó összjöv
# HX050: Equivalised household size

## R-ből
#RB030 personal ID
# RB020 ország
# RB050=weight
# RX030 hid,
# RB030 pid
# RB080 születési év

## P-ből
#PB030: PERSONAL ID only 16+
#PY010G egyéni bruttó munkajövedelem
#PY010N egyéni nettó munkajövedelem
#PY010N_F flag
#PY010N_I imputation factor

# E lesz naaaaagy file.
E <- NULL
for(i in 1:length(cy)) {
  d <- read.csv(str_c(filefix,cy[i],"D.csv"))
  h <- read.csv(str_c(filefix,cy[i],"H.csv"))
  r <- read.csv(str_c(filefix,cy[i],"R.csv"))
  p <- read.csv(str_c(filefix,cy[i],"P.csv"))
  ## 2021-től HY020_I helyett HY020_IF van és ugyanígy: PY010N_I helyett PY010_IF
  ev <- as.numeric(str_sub(cy[i],3,4))
  # csak a szükséges változókat hagyom meg
  d <- d[,c("DB030","DB040")]
  if (ev<21) {h <- h[,c("HB030","HB010","HX060","HX090","HY020",
                        "HY120G","HY130G","HY140G",
                        "HY020_F","HY020_I","HY010","HY010_F","HY010_I","HX050")]}
  if (ev>20) {h <- h[,c("HB030","HB010","HX060","HX090","HY020",
                        "HY120G","HY130G","HY140G",
                        "HY020_F","HY020_IF","HY010_F","HY010_IF","HY010","HX050")]}
  r <- r[,c("RB030","RB020","RB050","RX030","RB080")]
  if (ev<21) {p <- p[,c("PB030","PY010G","PY010N","PY010N_F","PY010N_I","PY010G_F","PY010G_I",
                        "PY200G","PY200G_F","PY200G_I")]}
  if (ev>20) {p <- p[,c("PB030","PY010G","PY010N","PY010N_F","PY010N_IF","PY010G_F","PY010G_IF")]}
  w <- merge(merge(r,p,by.x="RB030",by.y="PB030",all.x=T),
             merge(d,h,by.x="DB030",by.y="HB030"),by.x="RX030",by.y="DB030",all.x=T)
  E <- bind_rows(E,w)
  print(cy[i])}


# ## egyedi csekkoló amíg kerestem, hogy miért nem stimmelnek a változónevek
# #AT21H
# th <- read.csv(str_c(filefix,"AT22","H.csv"))
# c("HB030","HB010","HX060","HX090","HY020","HY020_F","HY020_I","HY010","HX050") %in% names(th)
# th %>% select(starts_with("HY020")) %>% names()
# 
# tp <- read.csv(str_c(filefix,"AT21","P.csv"))
# c("PB030","PY010G","PY010N","PY010N_F","PY010N_I") %in% names(tp)
# names(tp)

# nézzük csak hány év van benne, esetszámok rendben vannak-e
table(E$HB010,useNA="always")
table(E$RB020,useNA="always")
# ha nincs év, nincs adat, ezeket az eseteket kidobom
E <- E[is.na(E$HB010)==F,]
# ha nincs súly, nincs adat
E <- E[is.na(E$RB050)==F,]
# Görögök az elején GR utána EL
E$RB020[E$RB020=="GR"] <- "EL"

save(E,file="C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/adatfile_valtozatok/EUSILCeu27.RData")

load("C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/adatfile_valtozatok/EUSILCeu27.RData")
#write.csv2(E,"C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/adatfile_valtozatok/EUSILCeu27.csv")
#write_dta(E,"C:/Panni-NAS/MasMunka/2024/Adatproblemak_kozos/data/feher/adatfile_valtozatok/EUSILCeu27.dta")
