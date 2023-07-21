library(tidyverse)
library(stats)
library(na.tools)
library(modelr)
library(dplyr)
library(multiplex)
library(data.table); library(Jmisc); library("lavaan");
library(qgraph); library(psych); library(corrplot); library("psych");
library(ggplot2); library(car); library(compare); library(gdata); 
library(corrplot); library(moments); library(ltm); library(Hmisc)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## load data ##
main.ed.med.icv.residual<-read.csv('main.ed.med.icv.residual.csv')
sex.diff <- main.ed.med.icv.residual
#save sex x volume interaction
#center the input variables
sex.diff$FEMALE_b <- as.numeric(as.character(sex.diff$FEMALE_b))
FEMALE_bc <- sex.diff$FEMALE_b - mean(sex.diff$FEMALE_b)
vol_7_rc <- sex.diff$vol_7_r - mean(sex.diff$vol_7_r)
vol_23_rc <- sex.diff$vol_23_r - mean(sex.diff$vol_23_r)
vol_29_rc <- sex.diff$vol_29_r - mean(sex.diff$vol_29_r)
vol_30_rc <- sex.diff$vol_30_r - mean(sex.diff$vol_30_r)
vol_41_rc <- sex.diff$vol_41_r - mean(sex.diff$vol_41_r)
vol_42_rc <- sex.diff$vol_42_r - mean(sex.diff$vol_42_r)
vol_48_rc <- sex.diff$vol_48_r - mean(sex.diff$vol_48_r)
vol_55_rc <- sex.diff$vol_55_r - mean(sex.diff$vol_55_r)
vol_56_rc <- sex.diff$vol_56_r - mean(sex.diff$vol_56_r)
vol_57_rc <- sex.diff$vol_57_r - mean(sex.diff$vol_57_r)

#create the interaction variable
sex.diff$vol_7r_female <- FEMALE_bc * vol_7_rc
sex.diff$vol_23r_female <- FEMALE_bc * vol_23_rc
sex.diff$vol_29r_female <- FEMALE_bc * vol_29_rc
sex.diff$vol_30r_female <- FEMALE_bc * vol_30_rc
sex.diff$vol_41r_female <- FEMALE_bc * vol_41_rc
sex.diff$vol_42r_female <- FEMALE_bc * vol_42_rc
sex.diff$vol_48r_female <- FEMALE_bc * vol_48_rc
sex.diff$vol_55r_female <- FEMALE_bc * vol_55_rc
sex.diff$vol_56r_female <- FEMALE_bc * vol_56_rc
sex.diff$vol_57r_female <- FEMALE_bc * vol_57_rc

# replace NA with .
sex.diff[is.na(sex.diff)]<-"."

write.dat(sex.diff, "sex.diff")
