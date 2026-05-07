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
df_foxcol <- setDT(df_foxcol)
## Adjust for start of study
df_foxcol[,date.begin:=fifelse(FoxID=="ÖstbyM", as.IDate("2011-10-01"), date.begin)]
# ## Edit wrong date
df_foxcol[,date.begin:=fifelse(FoxID=="Molly", as.IDate("2013-01-20"), date.begin)]
## Collar file 
## Ann, Runar, Tallåsa one time series two trx
## Frans, Gunilla, Ingvar, Oskar => two time lines (1,2)
df_foxcol_old <-  fread(here(data,"GPS_Col2.csv"))
df_foxcol_old <- setDT(df_foxcol_old)
col<- names(df_foxcol_old)
col[1] <- "FoxID"
colnames(df_foxcol_old) <- col
merged <- merge(df_foxcol_old, df_foxcol, by = "FoxID" )
col2 <- names(merged)
col2[2] <- "Sex1"
col2[16] <- "Sex2"
col2[3] <- "Age1"
col2[17] <- "Age2"
colnames(merged) <- col2
# Check
names(merged)

########################################## Prepare data for Cox regression ##################
# "2011-02-09 UTC"
merged[, S_YEAR := year(date.begin)]
merged[, E_YEAR := year(date.end)]
merged[, S_WEEK := week(date.begin)]
merged[, E_WEEK := week(date.end)]
# 2011-2019
## Remove ALfa
merged2 <- merged[-2]

# SKapa år som löpnummer 19
merged2[, S_Y := S_YEAR - 2010]
merged2[, E_Y := E_YEAR - 2010]

############################### Make a continius week scale ####
## starten till vecka 40
Y <- 9
W <- 40 # 0

l <- dim(merged2)[1]
for (i in 1:l){
merged2[, S_W := (S_Y-1)*52 + S_WEEK]
merged2[, E_W := (E_Y-1)*52 + E_WEEK]
merged2[, S_W2 := S_W - W]  #OBS THESE VALUES GOES INTO THE ACCUMULATING WEEKS IN THE LONG LOOP BELOW
merged2[, E_W2 := E_W - W ]
}
with(merged2,table(S_W)) # 37 - 438
with(merged2,table(E_W)) # 57 -  444

merged2[, Weeks := E_W - S_W]
merged2[Weeks>40] # Cristina1 53 Weeks

flexplot(Weeks ~  AREA1 | Sex1, data = merged2)
ggsave(here(fig,"GPS_Time_Area.pdf"))
summary(merged2$Weeks)
 # Min. 1st Qu.  Median    Mean 3rd Qu.     Max.
 #   0.00    8.00   19.00   18.89   27.00   53.00 


#### Adjust the Year to the new 1-52 () #################

cut_week <- (0:Y)*52+W  # Determines the cutoff values for the new annual weeks 
cut_week

merged2[,S_Y2 := S_Y]
merged2[,E_Y2 := E_Y]
for (w in 1:Y){
merged2[(S_W > cut_week[w] & S_W <= cut_week[w+1]), S_Y2 := w]
merged2[(E_W > cut_week[w] & E_W <= cut_week[w+1]), E_Y2 := w]
}

with(merged2, table(S_Y2, S_Y))
fwrite(merged2, here(data,"merged2.csv"))

## Remove some variables to make processing simpler
names(merged2)
# "Sex1" "Age1" "Born" "AREA1" "AREA2" "TRX" "Start"  "End"  "Kommentar"  "Fate"  "löpNr"  "time" "No" "Group" 
merged3 <- merged2[,-c(2,3,4,5,6,7,8,9,10,11,12,14,15,18) ] 
names(merged3)
merged3[S_W<50]

##########  Subadults that stayed until they became adults
# Survived more than 1 year in the new annual scale 
merged3[(Age2 == "SA" & (E_Y2-S_Y2)>0)]
merged3[, CENS := fifelse(CAUSE<7,1,0)]
with(merged3,table(CENS))
## 43 dödstillfällen

ind<-dim(merged3)[1] #140
##################
fwrite(merged3, here(data, "merged3.csv"))

merged3[,Age2]

#################################################### Create file for Andersson-Gill #######################################


## Below I create a data.table for all individuals for each week they are in the study! 
#colnames(fox_matrix) <- c('id','sw', 'ew', 'cens', 'sex','age', 'site', 'syr','cause') 

fox_surv <- data.table(id=rep('id', 3000), sw=rep(0, 3000), ew=rep(0,3000), cens=rep(9, 3000), sex=rep('sex', 3000),age=rep('age', 3000), site=rep('site', 3000),cause=rep(999,3000), SY=rep(999,3000), EY=rep(999,3000)) 
str(fox_surv)
start<- end<-census<-numeric()
    # OBS att ew och sw får nya värden i samma skala som start och end!!!
    rad <- 0 
    for (i in 1:ind) {  # För varje individ
        for (j in 1:417) {      # Sätt upp levande död för varje vecka i studien
            if ((merged3$S_W2[i]>j) | (j>merged3$E_W2[i]))  next # Om den ligger utanför intervallet så ta nästa! 
                    else {
                rad <- rad + 1 
                start <- j 
                end <- j + 1 
                census<-0  # Lägger in en veckas överlevnad och sätter censor till 0 som default
                census <- ifelse(merged3$E_W2[i]==j,  merged3$CENS[i], 0) # Men om den dog den veckan så sätts ny cens

# Which foxes were captured as subadults and survived more than week 36 the next year!
                fox_surv[rad,1] <- merged3$FoxID[i]
                fox_surv[rad,2] <- start
                fox_surv[rad,3] <- end
                fox_surv[rad,4] <- census
                fox_surv[rad,5] <- merged3$Sex2[i]
                fox_surv[rad,6] <- merged3$Age2[i]
                fox_surv[rad,7] <- merged3$Area[i]
                fox_surv[rad,8] <- merged3$CAUSE[i]
                fox_surv[rad,9] <- merged3$S_Y2[i]
                fox_surv[rad,10] <- merged3$E_Y2[i]
                
                }
            }
            print(merged2$FoxID[i])
        }
# fox_surv <- fox_surv[age != "age"] ## Adjust for emptylines at the end
with(fox_surv,table(age))
tail(fox_surv)
head(fox_surv)
## Justera till ett poolat år 
fox_surv$ew2 <- fox_surv$ew
fox_surv$sw2 <- fox_surv$sw
cut_week <- (0:9)*52  # Determines the cutoff values for the new annual weeks 
cut_week
for(i in 2:8){
fox_surv[ (sw > cut_week[i] & sw <= cut_week[i+1]), sw2 := sw - (i-1)*52]
}
fox_surv$ew2 <- fox_surv$sw2+1   
tail(fox_surv)
head(fox_surv)
with(fox_surv, table(sw2, age))

########################### Subadults becoming adults #################
names(merged3)
merged3[(Age2 == "SA" &  S_WEEK<45)]

# # Nine individuals that went from SA to AD
# # Find the week number where they become adults 
# ChekID <- c("Bakken", Brasse, "Carmen",  "Klas", "Niklas", "Snerta", "Tessan")   
# merged3[,E_W2,FoxID == "Bakken"] # 369
# merged3[,E_W2,FoxID == "Brasse"] # 159
# merged3[,E_W2,FoxID == "Carmen"] # 111
# merged3[,E_W2,FoxID == "Klas"] # 165
# merged3[,E_W2,FoxID == "Niklas"] # 166
# merged3[,E_W2,FoxID == "Snerta"] # 112
# merged3[,E_W2,FoxID == "Tessan"] # 213

## Justera de SA som blev AD medans de var med i studien
## Se veckovärden ovan. Dålig kod men bli rätt
# Dessa beror på vilken vecka som sätts som statrvecka. 
W
# 36


fox_surv[ew2>50 & age=="SA"]
merged3[FoxID=="Alba"]
                                                
fox_surv[ ,age := fifelse((id=="Bakken" & ew>365), "AD", age)]
# fox_surv[ ,age := fifelse((id=="Brasse" & ew>157), "AD", age)]
fox_surv[ ,age := fifelse((id=="Carmen" & ew> 105), "AD", age)]
fox_surv[ ,age := fifelse((id=="Klas" & ew>157), "AD", age)]
fox_surv[ ,age := fifelse((id=="Niklas" & ew>157), "AD", age)]
fox_surv[ ,age := fifelse((id=="Snerta" & ew>105), "AD", age)]
fox_surv[ ,age := fifelse((id=="Tessan" & ew>209), "AD", age)]


fwrite(fox_surv, here(data, "fox_surv"))

################################# Survival Analysis #######################
library(survival)
library(AICcmodavg)
library(survminer)

# fox_surv <- fread(here(data, "foxsurv.csv"))
fox_surv$sex <- as.factor(fox_surv$sex)
fox_surv$age <- as.factor(fox_surv$age)
fox_surv$site <- as.factor(fox_surv$site)

# fox_surv <- fox_surv[sw3>9]
head(fox_surv)


a1<-survfit(Surv(sw2,ew2,cens) ~ 1 , data = fox_surv)
plot(a1)
a2<-survfit(Surv(sw2,ew2,cens) ~ sex , data = fox_surv)
plot(a2)
a3<-survfit(Surv(sw2,ew2,cens) ~ age , data = fox_surv)
plot(a3)
a4<-survfit(Surv(sw2,ew2,cens) ~ age + sex , data = fox_surv)
summary(a4)

# Kolla sample size subadult-sex
m1<-coxph(Surv(sw2,ew2,cens)~1, data=fox_surv)
m2<-coxph(Surv(sw2,ew2,cens)~sex, data=fox_surv)
m3<-coxph(Surv(sw2,ew2,cens)~age, data=fox_surv)
m4<-coxph(Surv(sw2,ew2,cens)~age+sex, data=fox_surv)
m5<-coxph(Surv(sw2,ew2,cens)~age*sex, data=fox_surv)

candidates1 = list(m1, m2, m3, m4, m5)
model.names1 = c("intercept", "sex", "age", "age_sex", "age*sex")
aictab(cand.set = candidates1, modnames = model.names1, sort = TRUE)
summary(m3)
summary(m4)
cox.zph(m4, transform="km", global=TRUE)

# pdf(here(fig,"annualpooled_age_surv.pdf"))
# ggsurvplot(
#    a3,                     # survfit object with calculated statistics.
#    data = fox_surv,             # data used to fit survival curves.
#    risk.table = TRUE,       # show risk table.
#    pval = FALSE,             # show p-value of log-rank test.
#    conf.int = TRUE,         # show confidence intervals for 
#                             # point estimates of survival curves.
#    xlim = c(0,53),         # present narrower X axis, but not affect
#                             # survival estimates.
#    xlab = "Time in weeks",   # customize X axis label.
#    break.time.by = 5,     # break X axis in time intervals by 500.
#    ggtheme = theme_light(), # customize plot and risk table with a theme.
#  risk.table.y.text.col = T, # colour risk table text annotations.
#   risk.table.y.text = FALSE # show bars instead of names in text annotations
#                             # in legend of risk table
# )
# dev.off()


## Figur3en nedan visar att det är för få unga per kön för att lita på resultaten?
pdf(here(fig,"annualpooled_age_sex_surv.pdf"))
ggsurvplot(
   a4,                     # survfit object with calculated statistics.
   data = fox_surv,             # data used to fit survival curves.
   risk.table = TRUE,       # show risk table.
   pval = FALSE,             # show p-value of log-rank test.
   conf.int = TRUE,         # show confidence intervals for 
                            # point estimates of survival curves.
   xlim = c(0,53),         # present narrower X axis, but not affect
                            # survival estimates.
   xlab = "Time in weeks",   # customize X axis label.
   break.time.by = 5,     # break X axis in time intervals by 500.
   ggtheme = theme_light(), # customize plot and risk table with a theme.
 risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
                            # in legend of risk table
)
dev.off()

# m1a<-coxph(Surv(sw3,ew3,cens)~1, data=fox_surv)
# m2a<-coxph(Surv(sw3,ew3,cens)~ season, data=fox_surv)
# m3a<-coxph(Surv(sw3,ew3,cens)~ age, data=fox_surv)
# m4a<-coxph(Surv(sw3,ew3,cens) ~ sex, data=fox_surv)
# m5a <- coxph(Surv(sw3,ew3,cens) ~ age + season, data=fox_surv)
# m6a <- coxph(Surv(sw3,ew3,cens) ~ sex + season, data=fox_surv)
# m7a <- coxph(Surv(sw3,ew3,cens) ~ sex + age, data=fox_surv)
# m8a <- coxph(Surv(sw3,ew3,cens) ~ age + season+sex, data=fox_surv)
# candidates = list(m1a, m2a, m3a, m4a, m5a, m6a, m7a, m8a)
# model.names = c("intercept", "season", "age", "sex", "age+season", "sex+season", "sex+age", "sex+age+season")
# aictab(cand.set = candidates, modnames = model.names, sort = TRUE)
# Model selection based on AICc:

# #                K   AICc Delta_AICc AICcWt Cum.Wt      LL
# # sex+age+season 3 389.87       0.00   0.42   0.42 -191.93
# # age+season     2 389.94       0.07   0.40   0.82 -192.97
# # sex+season     2 392.97       3.10   0.09   0.91 -194.48
# # season         1 393.20       3.33   0.08   0.98 -195.60
# # sex+age        2 397.86       7.99   0.01   0.99 -196.93
# # age            1 398.41       8.54   0.01   1.00 -198.20
# # sex            1 400.92      11.05   0.00   1.00 -199.46
# # intercept      0 401.65      11.78   0.00   1.00 -200.83

# cox.zph(m5a, transform="km", global=TRUE)
# summary(m5a)
# #       chisq df    p                                                           
# # age     3.84  1 0.05
# # season  1.13  1 0.29
# # GLOBAL  4.82  2 0.09
 
# cox.zph(m8a, transform="km", global=TRUE)
# summary(m8a)
# outcox <- cox.zph(m5a)
# ggcoxzph(outcox)


# m5ap<-survfit(Surv(sw3,ew3,cens) ~ age + season, data=fox_surv)

pdf(here(fig,"seasonal_age_surv_new2.pdf"))

ggsurvplot(
   a4,                     # survfit object with calculated statistics.
   data = fox_surv,             # data used to fit survival curves.
   risk.table = TRUE,       # show risk table.
   pval = FALSE,             # show p-value of log-rank test.
   conf.int = FALSE,         # show confidence intervals for 
                            # point estimates of survival curves.
   xlim = c(0,52),         # present narrower X axis, but not affect
                            # survival estimates.
   xlab = "Time in weeks",   # customize X axis label.
   break.time.by = 5,     # break X axis in time intervals by 500.
 #  ggtheme = custom_theme(), # customize plot and risk table with a theme.
 risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = T, # show bars instead of names in text annotations
                            # in legend of risk table
  censor.shape = "l", linetype = c(1,6,1,6),
  color="strata",
  legend.labs = c("AD Fem", "AD Male", "SA Fem", "SA Male"),
 palette = c("black", "#070807","#5d615d", "#6a6e6a"), 
 #facet.by = "season"
  )

dev.off()

summary(m5ap)
#See file summary_m5ap.tex

SA_M_1 <- fox_surv[age=="SA" & sex == "M" & season==1]  # 311
AD_M_1 <- fox_surv[age=="AD" & sex == "M" & season==1]  # 494
SA_M_2<- fox_surv[age=="SA"  & sex == "M" & season==2]  # 351
AD_M_2 <- fox_surv[age=="AD" & sex == "M" & season==2]  # 516
# 1672
SA_F_1 <- fox_surv[age=="SA" & sex == "F" & season==1]  # 174
AD_F_1 <- fox_surv[age=="AD" & sex == "F" & season==1]  # 287
SA_F_2 <- fox_surv[age=="SA" & sex == "F" & season==2]  # 236
AD_F_2 <- fox_surv[age=="AD" & sex == "F" & season==2]  # 416
# 1113

311+494+351+516
174+287+236+416

\toprule
Sex     & Age       & Season I & Season II \\
\midrule
Males   & Adult     &  494     &  516      \\ 
Males   & Subadult  &  311     &  351      \\
Females & Adult     &  287     &  416      \\
Females & Subadult  &  174     &  236      \\
\bottomrule
