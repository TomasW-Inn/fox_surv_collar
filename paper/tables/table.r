setwd("/home/tomasw/Dropbox/A_Work/PROJECTS/fox_mort_surv/paper/tables")
pattern <- "\\.csv$"
files <- list.files(pattern=pattern )

library(knitr)
for (i in 1:6){
print(files[i])    
dat <- read.csv(files[i] )
cat(kable(dat, format = "latex", booktabs = TRUE))
}
