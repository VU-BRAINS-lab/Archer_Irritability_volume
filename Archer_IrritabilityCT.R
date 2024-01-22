#Irritability and CT
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
data<-readRDS('longitudinal_old.rds')
# change characters to numeric
indx<-sapply(data, is.character)
data[indx]<-lapply(data[indx], function(x) as.numeric(x))

# make data frame of variables
irritability.data<-data[,c(2:4,6,7,10,12:14,36,126:128,135,297,305:309,311,314,317,1003,1007,1570:1637)]
irritability.data<-as.data.frame(irritability.data)
irritability.data <- irritability.data %>% rename("ct_1" = "smri_thick_cdk_banksstslh_b", 
                                                  "ct_2" = "smri_thick_cdk_cdacatelh_b",
                                                  "ct_3" = "smri_thick_cdk_cdmdfrlh_b", 
                                                  "ct_4" = "smri_thick_cdk_cuneuslh_b",
                                                  "ct_5" = "smri_thick_cdk_ehinallh_b", 
                                                  "ct_6" = "smri_thick_cdk_fusiformlh_b",
                                                  "ct_7" = "smri_thick_cdk_ifpllh_b",
                                                  "ct_8" = "smri_thick_cdk_iftmlh_b",
                                                  "ct_9" = "smri_thick_cdk_ihcatelh_b", 
                                                  "ct_10" = "smri_thick_cdk_locclh_b",
                                                  "ct_11" = "smri_thick_cdk_lobfrlh_b", 
                                                  "ct_12" = "smri_thick_cdk_linguallh_b",
                                                  "ct_13" = "smri_thick_cdk_mobfrlh_b", 
                                                  "ct_14" = "smri_thick_cdk_mdtmlh_b",
                                                  "ct_15" = "smri_thick_cdk_parahpallh_b", 
                                                  "ct_16" = "smri_thick_cdk_paracnlh_b",
                                                  "ct_17" = "smri_thick_cdk_parsopclh_b", 
                                                  "ct_18" = "smri_thick_cdk_parsobislh_b",
                                                  "ct_19" = "smri_thick_cdk_parstgrislh_b", 
                                                  "ct_20" = "smri_thick_cdk_pericclh_b",
                                                  "ct_21" = "smri_thick_cdk_postcnlh_b", 
                                                  "ct_22" = "smri_thick_cdk_ptcatelh_b",
                                                  "ct_23" = "smri_thick_cdk_precnlh_b", 
                                                  "ct_24" = "smri_thick_cdk_pclh_b",
                                                  "ct_25" = "smri_thick_cdk_rracatelh_b", 
                                                  "ct_26" = "smri_thick_cdk_rrmdfrlh_b",
                                                  "ct_27" = "smri_thick_cdk_sufrlh_b", 
                                                  "ct_28" = "smri_thick_cdk_supllh_b",
                                                  "ct_29" = "smri_thick_cdk_sutmlh_b", 
                                                  "ct_30" = "smri_thick_cdk_smlh_b",
                                                  "ct_31" = "smri_thick_cdk_frpolelh_b", 
                                                  "ct_32" = "smri_thick_cdk_tmpolelh_b",
                                                  "ct_33" = "smri_thick_cdk_trvtmlh_b", 
                                                  "ct_34" = "smri_thick_cdk_insulalh_b",
                                                  "ct_35" = "smri_thick_cdk_banksstsrh_b", 
                                                  "ct_36" = "smri_thick_cdk_cdacaterh_b",
                                                  "ct_37" = "smri_thick_cdk_cdmdfrrh_b", 
                                                  "ct_38" = "smri_thick_cdk_cuneusrh_b",
                                                  "ct_39" = "smri_thick_cdk_ehinalrh_b", 
                                                  "ct_40" = "smri_thick_cdk_fusiformrh_b",
                                                  "ct_41" = "smri_thick_cdk_ifplrh_b", 
                                                  "ct_42" = "smri_thick_cdk_iftmrh_b",
                                                  "ct_43" = "smri_thick_cdk_ihcaterh_b", 
                                                  "ct_44" = "smri_thick_cdk_loccrh_b",
                                                  "ct_45" = "smri_thick_cdk_lobfrrh_b", 
                                                  "ct_46" = "smri_thick_cdk_lingualrh_b",
                                                  "ct_47" = "smri_thick_cdk_mobfrrh_b", 
                                                  "ct_48" = "smri_thick_cdk_mdtmrh_b",
                                                  "ct_49" = "smri_thick_cdk_parahpalrh_b", 
                                                  "ct_50" = "smri_thick_cdk_paracnrh_b",
                                                  "ct_51" = "smri_thick_cdk_parsopcrh_b", 
                                                  "ct_52" = "smri_thick_cdk_parsobisrh_b",
                                                  "ct_53" = "smri_thick_cdk_parstgrisrh_b", 
                                                  "ct_54" = "smri_thick_cdk_periccrh_b",
                                                  "ct_55" = "smri_thick_cdk_postcnrh_b", 
                                                  "ct_56" = "smri_thick_cdk_ptcaterh_b",
                                                  "ct_57" = "smri_thick_cdk_precnrh_b", 
                                                  "ct_58" = "smri_thick_cdk_pcrh_b",
                                                  "ct_59" = "smri_thick_cdk_rracaterh_b", 
                                                  "ct_60" = "smri_thick_cdk_rrmdfrrh_b",
                                                  "ct_61" = "smri_thick_cdk_sufrrh_b", 
                                                  "ct_62" = "smri_thick_cdk_suplrh_b",
                                                  "ct_63" = "smri_thick_cdk_sutmrh_b", 
                                                  "ct_64" = "smri_thick_cdk_smrh_b",
                                                  "ct_65" = "smri_thick_cdk_frpolerh_b", 
                                                  "ct_66" = "smri_thick_cdk_tmpolerh_b",
                                                  "ct_67" = "smri_thick_cdk_trvtmrh_b", 
                                                  "ct_68" = "smri_thick_cdk_insularh_b")
#VERIFY VARIABLE TYPES
irritability.data<-irritability.data[!is.na(irritability.data$wt_NR_mwacs_b),]
irritability.data[is.na(irritability.data)]<-"."
irritability.data$FEMALE_b <- as.factor(irritability.data$FEMALE_b)
irritability.data$NHWHITE_b <- as.factor(irritability.data$NHWHITE_b)
irritability.data$HISPANIC_b <- as.factor(irritability.data$HISPANIC_b)
irritability.data$AFRICAN_b <- as.factor(irritability.data$AFRICAN_b)
irritability.data$medication_b <- as.factor(irritability.data$medication_b)
irritability.data$COIL2_b <- as.factor(irritability.data$COIL2_b)
irritability.data$COIL3_b <- as.factor(irritability.data$COIL3_b)
irritability.data$COIL4_b <- as.factor(irritability.data$COIL4_b)
irritability.data$COIL5_b <- as.factor(irritability.data$COIL5_b)

# replace NA with .
write.dat(irritability.data, "irritability.ct")
