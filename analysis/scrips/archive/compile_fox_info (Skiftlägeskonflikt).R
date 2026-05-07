library(data.table)
library(here)
library(flexplot)
library(ggplot2)
library(GGally)
library(readxl)
# library(tidyverse)

script <- "SCRIPT"
data <- "DATA"
fig <- "LATEX/figures"

##################### ***** Captures General ******* #################

# Mark data = capture 
df1_cap <- fread(here(data,"capture.csv"))
names(df1_cap)
str(df1_cap)
summary(df1_cap)
dim(df1_cap)
head(df1_cap)

df2_cap <- df1_cap[Mortalitet==""]
dim(df2_cap)
#174 captures
df2_cap[,capt := 1:.N, by = Namn]
with(df2_cap,table(capt))
fwrite(df2_cap, here(data,"capture2.csv"))
# capt                                                     
#   1   2 
# 160  14 

df3_cap <- df2_cap[capt==1]
with(df3_cap, table(Kön, Ålder2))

#     Ålder2
# Kön    ? juvenil vuxen
#   ?     0       1     0
#   hane  3      44    42
#   hona  4      40    26
with(df3_cap, table(Studieområde, Kön))
#            Kön                                          
# Studieområde  ? hane hona
#    Grimsö     0    7    4
#    Hedemora   0   10    4
#    Hedmark    0   16    9
#    Kolmården  1   56   53

df4_cap <- df3_cap[Kön=="hona" & (Ålder2=="vuxen" | Ålder2 == "juvenil")]
flexplot(Vikt ~ Studieområde | Ålder2, data = df4_cap)
ggsave(here(fig,"Vikt_Females.pdf"))

df5_cap <- df3_cap[Kön=="hane" & (Ålder2=="vuxen" | Ålder2 == "juvenil")]
flexplot(Vikt ~ Studieområde | Ålder2, data = df5_cap)
ggsave(here(fig,"Vikt_MAles.pdf"))

df1_cap[, recov := fifelse(Mortalitet == "", 1,0)]
df2b_cap <- df1_cap[recov==0]
head(df2b_cap)
fread(df2b_cap, "recov_kol-grm_hede.csv")
#Note there 74 recoveries of the 160 individuals captured. Collars did help of course.



##################### ***** Radio_collares ******* #################


## Collar file 
## Ann, Runar, Tallåsa one time series two trx
## Frans, Gunilla, Ingvar, Oskar => two time lines (1,2)
df_foxcol <- read_excel(here(data,"trx.xls"))
names(df_foxcol)
head(df_foxcol)
str(df_foxcol)
dim(df_foxcol)
df_foxcol <- setDT(df_foxcol)
df_foxcol[,time := as.numeric(End - Start)]
df_foxcol[,CAUSE := as.factor(CAUSE)]
fwrite(df_foxcol, here(data,"GPS_Col.csv"))

# 144 foxes were captured and collared
with(df_foxcol,table(Sex, Age))
#  Age                                                             
# Sex  ? adult valp
#   F  1    32   25
#   M  3    46   37
 
# with(df_foxcol,table(Sex, AREA1))
# Sex Grimsö Hedemora Hedmark Kolmården
#   F      4        4       8        42
#   M      7       10      14        55

sum(with(df_foxcol,table(Sex, CAUSE)))
#    CAUSE                                                           
# Sex  1  3  4  5  6  9
#   F 10  0  3  0  0 45
#   M 19  6  0  1  5 55
with(df_foxcol,table(AREA1, CAUSE))
# AREA1        1  3  4  5  6  9
#   Grimsö     2  0  0  0  1  8
#   Hedemora   3  1  0  1  1  8
#   Hedmark    7  1  0  0  1 13
#   Kolmården 17  4  3  0  2 71

with(df_foxcol,table(Sex))


## Death causes
#0 Lever
#1 Skjuten
#2 (reserved)
#3 Påkörd
#4 Sjukdom
#5 Predation
#6 Stress/Svält
#7 (reserved)
#8 Försvann tidigt
#9 Batteriet slut

flexplot(Weeks ~  AREA1 | Sex, data = df_foxcol2)
ggsave(here(fig,"GPS_Weeks_Area.pdf"))
with(df_foxcol2, summary(Weeks))









df_foxcol[, S_YEAR := year(Start)]
summey

# 206 captures
# summaryFox <- alla2[,.(.N, first_date = NewDate[1], last_date = NewDate[.N] ),keyby=.(FoxID)]
# dt = mydata[, rank:=frank(-distance,ties.method = "min"), by=carrier]
df1_cap[,capt := 1:.N, by = Namn]
with(df1_cap,table(Namn,capt))
with(df1_cap,table(capt))
#capt                                             
#   1   2   3 
# 140  60   6 
# Captured three times:
# NicklaS
# Christina
# Brasse
# Runar
# Roger
# Stephanie

with(df1_cap,table(SändarID, capt))
# SändarID   1   2   3
#        1 103  13   0
recapt <- df1_cap[capt > 1]
head(recapt)
# Brasse, Gunilla, Hen, Kira, Nicklas, Roger, Stephanie
# Fick sändare efter första fångst
# Sara och Svea fångades två gånger men fick aldrig någon sändare
# Ann, Frans, Ingvar, Oskar, Runar fick ny sändare
# Ann, Runar är kontinuerlig





df1_cap[, S_YEAR := year(Datum)]
df1_cap[, S_MONTH := month(dat)]
df1_cap[, S_DAY := mday(dat)]
df1_cap[, S_WEEK := week(dat)]

df1_cap$Född