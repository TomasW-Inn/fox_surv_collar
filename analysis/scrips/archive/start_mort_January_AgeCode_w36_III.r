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
W <- 35
# Young should enter the study at week W
# Use updated file from Zea
df_foxcol <-  fread(here(data,"collars.csv"))
df_foxcol <- setDT(df_foxcol)
names(df_foxcol)
unique(df_foxcol[,FoxID]) # 141 individuals
# Wrong date
df_foxcol[,date.begin:=fifelse(FoxID=="Molly", as.IDate("2013-01-20"), date.begin)]



# Adjust start to common week for Subadults
df_foxcol[,date.begin:=fifelse(FoxID=="Jens", as.IDate("2018-09-02"), date.begin)]
df_foxcol[,date.begin:=fifelse(FoxID=="Ludde", as.IDate("2016-09-02"), date.begin)]
df_foxcol[,date.begin:=fifelse(FoxID=="Per", as.IDate("2014-09-02"), date.begin)]
df_foxcol[,date.begin:=fifelse(FoxID=="Zea", as.IDate("2016-09-02"), date.begin)]
df_foxcol[,date.begin:=fifelse(FoxID=="Tommy", as.IDate("2017-09-02"), date.begin)]
df_foxcol[,date.begin:=fifelse(FoxID=="Olle", as.IDate("2013-09-02"), date.begin)]

library(stringr)
result1 <- str_detect(df_foxcol[,FoxID], "1")
df_foxcol[result1]
result2 <- str_detect(df_foxcol[,FoxID], "2")
# 6 individuals recaptured
df_foxcol[result2]

#      No. FoxID    Sex Age Group  Area     date.begin   date.end                            
# 1:   6      Ann2   F  AD   FAD Kolmården 2013-12-26 2014-09-15
# 2:  19 Cristina2   F  AD   FAD Kolmården 2018-03-29 2018-06-24
# 3:  35    Frans2   M  AD   MAD Kolmården 2017-03-21 2017-09-17
# 4:  41  Gunilla2   F  AD   FAD Kolmården 2013-05-30 2013-11-10
# 5:  53   Ingvar2   M  AD   MAD Kolmården 2014-05-08 2015-01-26
# 6:  97    Oskar2   M  AD   MAD Kolmården 2019-05-08 2019-05-17
names <- df_foxcol[result2]$FoxID
initial <- df_foxcol[!(FoxID %in% names),]
dim(initial)
with(initial, table(Sex, Age))


df_foxcol[, S_YEAR := year(date.begin)]
df_foxcol[, E_YEAR := year(date.end)]
df_foxcol[, S_WEEK := week(date.begin)]
df_foxcol[, E_WEEK := week(date.end)]

df_foxcol[, S_Y := S_YEAR - 2010]
df_foxcol[, E_Y := E_YEAR - 2010]

Y <- 9
l <- dim(df_foxcol)[1]
for (i in 1:l){
df_foxcol[, S_W := (S_Y-1)*52 + S_WEEK]
df_foxcol[, E_W := (E_Y-1)*52 + E_WEEK]
df_foxcol[, S_W2 := S_W - W]  #OBS THESE VALUES GOES INTO THE ACCUMULATING WEEKS IN THE LONG LOOP BELOW
df_foxcol[, E_W2 := E_W - W ]
}
with(df_foxcol,table(S_W)) # 37 - 438
with(df_foxcol,table(E_W)) # 57 -  444
df_foxcol[, Weeks := E_W - S_W]

dim(df_foxcol)
summary(df_foxcol)

df_foxcol_old <-  fread(here(data,"GPS_Col2.csv"))
df_foxcol_old <- setDT(df_foxcol_old)
col<- names(df_foxcol_old)
col[1] <- "FoxID"
colnames(df_foxcol_old) <- col
names(df_foxcol_old)
df_foxcol_old <- df_foxcol_old[,c(1,13)]

df_foxcol2 <- merge(df_foxcol_old, df_foxcol, by = "FoxID" )

## Remove Alfa and those that contributed less than two weeks 
remove <- c("Alfa","Rocio", "Stephanie", "Zea", "Anders",   "Atna", "Drevsjo2", "Kniven",   "Kvilten" )
df_foxcol2 <- df_foxcol2[!(FoxID %in% remove),]
dim(df_foxcol2)
# 132 

summary(df_foxcol2)


########################################## Prepare data for Cox regression ##################
############################### Make a continius week scale ####
## starten till vecka 35

flexplot(Weeks ~  Area | Sex, data = df_foxcol2)
ggsave(here(fig,"GPS_Time_Area.pdf"))
summary(df_foxcol2$Weeks)
 # Min. 1st Qu.  Median    Mean 3rd Qu.     Max.
 #   0.00    8.00   19.00   18.89   27.00   53.00 
dim(df_foxcol2)

#### Adjust the Year to the new 1-52 () #################

cut_week <- (0:Y)*52+W  # Determines the cutoff values for the new Year designation
cut_week

df_foxcol2[,S_Y2 := S_Y]
df_foxcol2[,E_Y2 := E_Y]
for (w in 1:Y){
df_foxcol2[(S_W > cut_week[w] & S_W <= cut_week[w+1]), S_Y2 := w]
df_foxcol2[(E_W > cut_week[w] & E_W <= cut_week[w+1]), E_Y2 := w]
}

with(df_foxcol2, table(S_Y2, S_Y))
fwrite(df_foxcol2, here(data,"df_foxcol2.csv"))
# df_foxcol2 <- fread(here(data,"df_foxcol2.csv")) 
names(df_foxcol2)

## Remove some variables to make processing simpler
df_foxcol2[, CENS := fifelse(CAUSE<7,1,0)]

#####################################################################################
with(df_foxcol2,table(CENS))
## 35 dödstillfällen
with (df_foxcol2,table(CAUSE, Sex, Age))
with (df_foxcol2[CAUSE<7],table(CAUSE, Sex))
with (df_foxcol2,table(CAUSE, Sex, Age))

ind<-dim(df_foxcol2)[1] #132
##################
# fwrite(df_foxcol2, here(data, "df_foxcol2.csv"))

#################################################### Create file for Andersson-Gill #######################################


## Below I create a data.table for all individuals for each week they are in the study! 
#colnames(fox_matrix) <- c('id','sw', 'ew', 'cens', 'sex','age', 'site', 'syr','cause') 


# merged3 <- fread(here(data, "merged3.csv")
fox_surv <- data.table(id=rep('id', 3900), sw=rep(0, 3900), ew=rep(0,3900), cens=rep(9, 3900), sex=rep('sex', 3900),age=rep('age', 3900), site=rep('site', 3900),cause=rep(999,3900), SY=rep(999,3900), EY=rep(999,3900)) 
str(fox_surv)
start<- end<-census<-numeric()
    # OBS att ew och sw får nya värden i samma skala som start och end!!!
    rad <- 0 
    for (i in 1:ind) {  # För varje individ
        for (j in 1:417) {      # Sätt upp levande död för varje vecka i studien
            if ((df_foxcol2$S_W2[i]>j) | (j>df_foxcol2$E_W2[i]))  next # Om den ligger utanför intervallet så ta nästa! 
                    else {
                rad <- rad + 1 
                start <- j 
                end <- j + 1 
                census<-0  # Lägger in en veckas överlevnad och sätter censor till 0 som default
                census <- ifelse(df_foxcol2$E_W2[i]==j,  df_foxcol2$CENS[i], 0) # Men om den dog den veckan så sätts ny cens

# Which foxes were captured as subadults and survived more than week 36 the next year!
                fox_surv[rad,1] <- df_foxcol2$FoxID[i]
                fox_surv[rad,2] <- start
                fox_surv[rad,3] <- end
                fox_surv[rad,4] <- census
                fox_surv[rad,5] <- df_foxcol2$Sex[i]
                fox_surv[rad,6] <- df_foxcol2$Age[i]
                fox_surv[rad,7] <- df_foxcol2$Area[i]
                fox_surv[rad,8] <- df_foxcol2$CAUSE[i]
                fox_surv[rad,9] <- df_foxcol2$S_Y2[i]
                fox_surv[rad,10] <- df_foxcol2$E_Y2[i]
                
                }
            }
            print(merged3$FoxID[i])
        }
# fox_surv <- fox_surv[age != "age"] ## Adjust for emptylines at the end
tail(fox_surv)
fox_surv <- fox_surv[age != "age"]
with(fox_surv,table(age))

## Justera till ett poolat år 
fox_surv$ew2 <- fox_surv$ew
fox_surv$sw2 <- fox_surv$sw
summary(fox_surv)
cut_week <- (0:9)*52  # Determines the cutoff values for the new annual weeks 
cut_week
for(i in 2:8){
fox_surv[ (sw > cut_week[i] & sw <= cut_week[i+1]), sw2 := sw - (i-1)*52]
}
fox_surv$ew2 <- fox_surv$sw2+1   
tail(fox_surv)
head(fox_surv)
summary(fox_surv)

########################### Subadults becoming adults #################

# Dessa beror på vilken vecka som sätts som statrvecka. 
W
# 35
nam1 <- unique(fox_surv[(age == "SA" & ew2 > 35), id ])
fox_surv[id==nam1[1]]
fox_surv[id=="Bakken"]


fox_surv[ ,age := fifelse((id=="Bakken" & ew>365), "AD", age)]
fox_surv[ ,age := fifelse((id=="Brasse" & ew>157), "AD", age)]
fox_surv[ ,age := fifelse((id=="Carmen" & ew> 105), "AD", age)]
fox_surv[ ,age := fifelse((id=="Klas" & ew>157), "AD", age)]
fox_surv[ ,age := fifelse((id=="Niklas" & ew>157), "AD", age)]
fox_surv[ ,age := fifelse((id=="Snerta" & ew>105), "AD", age)]
fox_surv[ ,age := fifelse((id=="Tessan" & ew>209), "AD", age)]
fox_surv[ ,age := fifelse((id=="Cristina1" & ew>157), "AD", age)]
fox_surv[ ,age := fifelse((id=="Gijom" & ew>209), "AD", age)]


# fwrite(fox_surv, here(data, "fox_surv"))

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
df_foxcol2[Sex=="F"  & CENS == 1 & Weeks<3, FoxID]
df_foxcol2[Sex=="M" &  CENS == 1 & Weeks<3, FoxID]
df_foxcol2[CENS == 1, FoxID]
fox_surv[cens == 1, id]
with(fox_surv, table(cens))

a1<-survfit(Surv(sw2,ew2,cens) ~ 1 , data = fox_surv)
plot(a1)
a2<-survfit(Surv(sw2,ew2,cens) ~ sex , data = fox_surv)
plot(a2)
a3<-survfit(Surv(sw2,ew2,cens) ~ age , data = fox_surv)
plot(a3)
a4<-survfit(Surv(sw2,ew2,cens) ~ age + sex , data = fox_surv)
plot(a4)

###########################################  Kolla unga honor tidigt #############
# Kolla sample size subadult-sex
m1<-coxph(Surv(sw2,ew2,cens)~1, data=fox_surv)
m2<-coxph(Surv(sw2,ew2,cens)~sex, data=fox_surv)
m3<-coxph(Surv(sw2,ew2,cens)~age, data=fox_surv)
m4<-coxph(Surv(sw2,ew2,cens)~age+sex, data=fox_surv)
m5<-coxph(Surv(sw2,ew2,cens)~age*sex, data=fox_surv)

candidates1 = list(m1, m2, m3, m4, m5)
model.names1 = c("intercept", "sex", "age", "age_sex", "age*sex")
aictab(cand.set = candidates1, modnames = model.names1, sort = TRUE)
summary(m2)
summary(m4)
cox.zph(m4, transform="km", global=TRUE)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

pdf("Sex_Age_Surv.pdf")
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

ggsave(here(fig,"Sex_Age_Surv.pdf"))

summary(a4)

# AdFem 0.677   0.141            1
# AdMal 0.534  0.0984        0.767
# SAFem 0.538   0.152        0.937
# SAMal 0.373   0.109        0.660

SA_M_1 <- fox_surv[age=="SA" & sex == "M"]  # 653
dim(SA_M_1)
AD_M_1 <- fox_surv[age=="AD" & sex == "M"]  # 1003
dim(AD_M_1)

SA_F_1 <- fox_surv[age=="SA" & sex == "F"]  # 408
dim(SA_F_1)
AD_F_1 <- fox_surv[age=="AD" & sex == "F"]  # 696
dim(AD_F_1)

tmp <- fox_surv[cens==1]
head(tmp)
with(tmp, table(cause, age, sex))
with(tmp, table(cause))
