library(data.table)
library(here)
library(flexplot)
library(ggplot2)
library(GGally)
library(readxl)

script <- "SCRIPT"
data <- "DATA"
fig <- "LATEX/figures"

##################### ***** Radio_collares ******* #################

# Use updated file from Zea
df_foxcol <-  fread(here(data,"collars.csv"))
names(df_foxcol)
head(df_foxcol)
str(df_foxcol)
dim(df_foxcol)
df_foxcol <- setDT(df_foxcol)
df_foxcol[FoxID=="Bane"]

## Collar file 
## Ann, Runar, Tallåsa one time series two trx
## Frans, Gunilla, Ingvar, Oskar => two time lines (1,2)
df_foxcol_old <-  fread(here(data,"GPS_Col2.csv"))
df_foxcol_old <- setDT(df_foxcol_old)
col<- names(df_foxcol_old)
col[1] <- "FoxID"
colnames(df_foxcol_old) <- col
dim(df_foxcol_old)
df_foxcol_old[FoxID=="Bane"]

merged <- merge(df_foxcol_old, df_foxcol, by = "FoxID" )
with(merged, table(CAUSE))
col2 <- names(merged)
col2[2] <- "Sex1"
col2[16] <- "Sex2"
col2[3] <- "Age1"
col2[17] <- "Age2"
colnames(merged) <- col2

# 135 unique foxes were captured and collared
# Excluded Doomed
with(merged,table(Sex2, Age2))
#     Age2                                   
# Sex2 AD SA
#    F 34 23
#    M 48 36

 
with(merged,table(Sex2, Area))
#     Area                                   
# Sex2 Grimsö Hedemora Hedmark Kolmården
#    F      4        4       9        40
#    M      7       10      15        52

with(merged,table(Age2, CAUSE))
#     CAUSE                                  
# Sex2  1  3  4  5  6  9
#    F  9  0  3  0  0 45
#    M 19  6  0  1  5 53
with(merged,table(Area, CAUSE))
# 
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

names(merged)
head(merged2)
# "2011-02-09 UTC"
merged[, S_YEAR := year(Start)]
merged[, E_YEAR := year(End)]
merged[, S_MONTH := month(Start)]
merged[, S_DAY := mday(Start)]
merged[, S_WEEK := week(Start)]
merged[, E_WEEK := week(End)]
min(merged$S_YEAR)
max(merged$S_YEAR)
# 2011-2019

with(merged,table(S_YEAR))
with(merged,table(S_WEEK))

## Remove ALfa
merged2 <- merged[-2]

# SKapa år som löpnummer 19
merged2[, S_Y := S_YEAR - 2010]
merged2[, E_Y := E_YEAR - 2010]
with(merged2,table(S_Y))
with(merged2,table(E_Y))

## SKapar veckonummer som en löpande serie där första vecka är 37 och 36 är 0 
# har lite betydelse för vilken vecka där överlevnaden är 1

l <- dim(merged2)[1]
for (i in 1:l){
	merged2[, S_W := (S_Y-1)*52 + S_WEEK]
	merged2[, E_W := (E_Y-1)*52 + E_WEEK]
	merged2[, S_W2 := S_W - 36]
	merged2[, E_W2 := E_W - 36 ]
}
merged2[, Weeks := E_W2 - S_W2]

with(merged2, table(Age2, E_W2))
flexplot(Weeks ~  AREA1 | Sex1, data = merged2)
ggsave(here(fig,"GPS_Time_Area.pdf"))
summary(merged2$Weeks)
# ## HAve to adjust the years becasue we start at week 36 and go inte to the following year
# ## Thus the year has to be subtracted by 1 if it is 
# l_level <- c(16,68,120,172,224,276,328,380,432,484)
# u_level <- c(52, 104, 156, 208, 260, 312, 364, 416, 468, 520)
# df_foxcol2[,S_Y2 := S_Y] ## De som märkets  och försvann under år ett i det justerade året.
# df_foxcol2[,S_Y2 := S_Y]
# for (w in 1:10){
# df_foxcol2[(S_W2>l_level[w] & S_W2<=u_level[w]), S_Y2 := S_Y-1]
# df_foxcol2[(E_W2>llcheck[w] & E_W2<=ulcheck[w]), E_Y2 := E_Y-1]
# }

## So we have 8 years of start years and 9 years of end year

str(merged2)
merged2[, CENS := fifelse(CAUSE<7,1,0)]
with(merged2,table(CENS))
## 43 dödstillfällen

ind<-dim(merged2)[1] #140
p1<-c(53,105,157,209, 261, 313, 365, 417) #week numbers of end of 8 years   
p2<-c(0, 52, 104, 156, 208, 260,312,364 ) # start of the 8 years

## Below I create a data.table for all individuals for each week they are in the study! 
#colnames(fox_matrix) <- c('id','sw', 'ew', 'cens', 'sex','age', 'site', 'syr','cause') 

fox_surv <- data.table(id=rep('id', 2797), sw=rep(0, 2797), ew=rep(0,2797), cens=rep(9, 2797), sex=rep('sex', 2797),age=rep('age', 2797), site=rep('site', 2797),cause=rep(999,2797)) 
str(fox_surv)
start<- end<-census<-numeric()
    # OBS att ew och sw får nya värden i samma skala som start och end!!!
    rad <- 0 
    for (i in 1:ind) {  # För varje individ
        for (j in 1:417) {      # Sätt upp levande död för varje vecka i studien
            if ((merged2$S_W2[i]>j) | (j>merged2$E_W2[i]))  next # Om den ligger utanför intervallet så ta nästa! 
                    else {
                rad <- rad + 1 
                start <- j 
                end <- j + 1 
                census<-0  # Lägger in en veckas överlevnad och sätter censor till 0 som default
                census <- ifelse(merged2$E_W2[i]==j,  merged2$CENS[i], 0) # Men om den dog den veckan så sätts ny cens

                fox_surv[rad,1] <- merged2$FoxID[i]
                fox_surv[rad,2] <- start
                fox_surv[rad,3] <- end
                fox_surv[rad,4] <- census
                fox_surv[rad,5] <- merged2$Sex2[i]
                fox_surv[rad,6] <- merged2$Age2[i]
                fox_surv[rad,7] <- merged2$Area[i]
                fox_surv[rad,8] <- merged2$CAUSE[i]
                }
            }
            print(merged2$FoxID[i])
        }
fox_surv$sex <- as.factor(fox_surv$sex)
fox_surv$age <- as.factor(fox_surv$age)
fox_surv$site <- as.factor(fox_surv$site)
#fox_surv <- fox_surv[id!= "id"]
with(fox_surv,table(age))
tail(fox_surv)
dim(fox_surv)

## Justera till ett poolat år 
fox_surv$ew2 <- fox_surv$ew
fox_surv$sw2 <- fox_surv$sw
for(i in 1:8){
fox_surv[sw > (i*52) & sw <= ((i+1)*52), sw2 := sw - (i*52)]
}
fox_surv$ew2 <- fox_surv$sw2+1   
tail(fox_surv)
head(fox_surv)
with(fox_surv, table(sw2))

## Create season in a year which is 26 weeks
wk_cut <- seq(1,427,26)
seasons <- rep(c(1,2),13)
for (i in 1:25){
fox_surv[(sw2>=wk_cut[i] & sw2<wk_cut[(i+1)]), season := seasons[i]]
}

with(fox_surv, table(season))

## Justera till säsonger 
fox_surv$sw3 <- fox_surv$sw2
fox_surv[season == 2, sw3 := sw2 - 26]
fox_surv$ew3 <- fox_surv$sw3+1
with(fox_surv, table(sw3))
fox_surv[sw3>25]
tail(fox_surv)
## Justera veckorna i season

str(fox_surv)
tail(fox_surv)
# Sub-adults becoming adults after week 36 No need for adjustments.
# Add a code for season
with(fox_surv, table(age, ew2)) 
fwrite(fox_surv, here(data, "foxsurv2.csv"))

################################# Survival Analysis #######################
library(survival)
library(AICcmodavg)
fox_surv <- fread(here(data, "foxsurv2.csv"))

a1<-survfit(Surv(sw2,ew2,cens) ~ 1 , data = fox_surv)
plot(a1)
a2<-survfit(Surv(sw2,ew2,cens) ~ sex , data = fox_surv)
plot(a2)
a3<-survfit(Surv(sw2,ew2,cens) ~ age , data = fox_surv)
plot(a3)
summary(a3)


m1<-coxph(Surv(sw2,ew2,cens)~1, data=fox_surv)
m2<-coxph(Surv(sw2,ew2,cens)~sex, data=fox_surv)
m3<-coxph(Surv(sw2,ew2,cens)~age, data=fox_surv)
candidates1 = list(m1, m2, m3)
model.names1 = c("intercept", "sex", "age")
aictab(cand.set = candidates1, modnames = model.names1, sort = TRUE)
summary(m3)
cox.zph(m3, transform="km", global=TRUE)

m1a<-coxph(Surv(sw3,ew3,cens)~1, data=fox_surv)
m2a<-coxph(Surv(sw3,ew3,cens)~ as.factor(season), data=fox_surv)
m3a<-coxph(Surv(sw3,ew3,cens)~ age, data=fox_surv)
m4a<-coxph(Surv(sw3,ew3,cens) ~ age + as.factor(season), data=fox_surv)

candidates = list(m1a, m2a, m3a, m4a)
model.names = c("intercept", "season", "age", "age+season")
aictab(cand.set = candidates, modnames = model.names, sort = TRUE)
cox.zph(m4a, transform="km", global=TRUE)
summary(m4a)
m4ap<-survfit(Surv(sw3,ew3,cens) ~ age + as.factor(season), data=fox_surv)

q<-summary(survfit(Surv(sw3,ew3,cens) ~ age + as.factor(season), data=fox_surv))
q
library(survminer)
outcox <- cox.zph(m4a)
ggcoxzph(outcox)

pdf(here(fig,"annualpooled_age_surv.pdf"))
ggsurvplot(
   a3,                     # survfit object with calculated statistics.
   data = fox_surv,             # data used to fit survival curves.
   risk.table = TRUE,       # show risk table.
   pval = FALSE,             # show p-value of log-rank test.
   conf.int = TRUE,         # show confidence intervals for 
                            # point estimates of survival curves.
   xlim = c(0,52),         # present narrower X axis, but not affect
                            # survival estimates.
   xlab = "Time in weeks",   # customize X axis label.
   break.time.by = 5,     # break X axis in time intervals by 500.
   ggtheme = theme_light(), # customize plot and risk table with a theme.
 risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
                            # in legend of risk table
)
dev.off()

# ggsave("annuaL_age_surv.pdf")
pdf(here(fig,"seasonal_age_surv.pdf"))
ggsurvplot(
   m4ap,                     # survfit object with calculated statistics.
   data = fox_surv,             # data used to fit survival curves.
   risk.table = TRUE,       # show risk table.
   pval = FALSE,             # show p-value of log-rank test.
   conf.int = TRUE,         # show confidence intervals for 
                            # point estimates of survival curves.
   xlim = c(0,26),         # present narrower X axis, but not affect
                            # survival estimates.
   xlab = "Time in weeks",   # customize X axis label.
   break.time.by = 5,     # break X axis in time intervals by 500.
   ggtheme = theme_light(), # customize plot and risk table with a theme.
 risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
                            # in legend of risk table
)
dev.off()

# ggsave(here(fig,"seasonal_age_surv.pdf"))
