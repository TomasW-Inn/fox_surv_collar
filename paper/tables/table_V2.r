setwd("~/Dropbox/A_Work/PROJECTS/JaktEffekt/SCRIPT5/Final_Report_Paper/LATEX/tables")
library(knitr)
dat <- read.csv("LineLength.csv")
cat(kable(dat, format = "latex", booktabs = TRUE))
