#This script residualizes the data to prepare it for various iterations of analyses
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
irritability.data<-data[,c(2:4,6,7,10,12:14,36,126:128,135,297,305:309,311,314,317,1003,1570:1637)]
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
irritability.data$FEMALE_b <- as.factor(irritability.data$FEMALE_b)
irritability.data$NHWHITE_b <- as.factor(irritability.data$NHWHITE_b)
irritability.data$HISPANIC_b <- as.factor(irritability.data$HISPANIC_b)
irritability.data$AFRICAN_b <- as.factor(irritability.data$AFRICAN_b)
irritability.data$medication_b <- as.factor(irritability.data$medication_b)
irritability.data$COIL2_b <- as.factor(irritability.data$COIL2_b)
irritability.data$COIL3_b <- as.factor(irritability.data$COIL3_b)
irritability.data$COIL4_b <- as.factor(irritability.data$COIL4_b)
irritability.data$COIL5_b <- as.factor(irritability.data$COIL5_b)

#CREATE RESIDUALS FOR PRIMARY ANALYSIS

#run residualization
Y.primary <- as.matrix(irritability.data[c(25:92)])
lm.tores.primary <- lm(Y.primary ~ age_b + FEMALE_b + NHWHITE_b + AFRICAN_b + HISPANIC_b + 
                         COIL2_b + COIL3_b + COIL4_b + COIL5_b + parent_education_b + medication_b + TICV_b, data = irritability.data)
lm.residuals.primary <- lm.tores.primary$residuals
#add residuals matrix to the data frame and rename the data frame
ct.residual <- irritability.data %>% spread_residuals(lm.tores.primary)
#condense new data frame to just sub_num, weight, and residual column
ct.residual <- ct.residual[c(grep("subnum_char_b|wt_NR_mwacs_b|lm.tores.primary", names(ct.residual)))]
#omit cases with NAs in row
ct.residual <- na.omit(ct.residual)
#save csv file with sub ID, weight, and all residuals
write.csv(ct.residual,"ct.residual.csv", row.names = FALSE)
ct.residual<-read.csv('ct.residual.csv')
ct.residual <-(ct.residual[c(1,3:70)])

#rename
ct.residual <- ct.residual %>% rename("ct_1_r" = "lm.tores.primary.ct_1", 
                                                                "ct_2_r" = "lm.tores.primary.ct_2",
                                                                "ct_3_r" = "lm.tores.primary.ct_3", 
                                                                "ct_4_r" = "lm.tores.primary.ct_4",
                                                                "ct_5_r" = "lm.tores.primary.ct_5", 
                                                                "ct_6_r" = "lm.tores.primary.ct_6",
                                                                "ct_7_r" = "lm.tores.primary.ct_7",
                                                                "ct_8_r" = "lm.tores.primary.ct_8",
                                                                "ct_9_r" = "lm.tores.primary.ct_9", 
                                                                "ct_10_r" = "lm.tores.primary.ct_10",
                                                                "ct_11_r" = "lm.tores.primary.ct_11", 
                                                                "ct_12_r" = "lm.tores.primary.ct_12",
                                                                "ct_13_r" = "lm.tores.primary.ct_13", 
                                                                "ct_14_r" = "lm.tores.primary.ct_14",
                                                                "ct_15_r" = "lm.tores.primary.ct_15", 
                                                                "ct_16_r" = "lm.tores.primary.ct_16",
                                                                "ct_17_r" = "lm.tores.primary.ct_17", 
                                                                "ct_18_r" = "lm.tores.primary.ct_18",
                                                                "ct_19_r" = "lm.tores.primary.ct_19", 
                                                                "ct_20_r" = "lm.tores.primary.ct_20",
                                                                "ct_21_r" = "lm.tores.primary.ct_21", 
                                                                "ct_22_r" = "lm.tores.primary.ct_22",
                                                                "ct_23_r" = "lm.tores.primary.ct_23", 
                                                                "ct_24_r" = "lm.tores.primary.ct_24",
                                                                "ct_25_r" = "lm.tores.primary.ct_25", 
                                                                "ct_26_r" = "lm.tores.primary.ct_26",
                                                                "ct_27_r" = "lm.tores.primary.ct_27", 
                                                                "ct_28_r" = "lm.tores.primary.ct_28",
                                                                "ct_29_r" = "lm.tores.primary.ct_29", 
                                                                "ct_30_r" = "lm.tores.primary.ct_30",
                                                                "ct_31_r" = "lm.tores.primary.ct_31", 
                                                                "ct_32_r" = "lm.tores.primary.ct_32",
                                                                "ct_33_r" = "lm.tores.primary.ct_33", 
                                                                "ct_34_r" = "lm.tores.primary.ct_34",
                                                                "ct_35_r" = "lm.tores.primary.ct_35", 
                                                                "ct_36_r" = "lm.tores.primary.ct_36",
                                                                "ct_37_r" = "lm.tores.primary.ct_37", 
                                                                "ct_38_r" = "lm.tores.primary.ct_38",
                                                                "ct_39_r" = "lm.tores.primary.ct_39", 
                                                                "ct_40_r" = "lm.tores.primary.ct_40",
                                                                "ct_41_r" = "lm.tores.primary.ct_41", 
                                                                "ct_42_r" = "lm.tores.primary.ct_42",
                                                                "ct_43_r" = "lm.tores.primary.ct_43", 
                                                                "ct_44_r" = "lm.tores.primary.ct_44",
                                                                "ct_45_r" = "lm.tores.primary.ct_45", 
                                                                "ct_46_r" = "lm.tores.primary.ct_46",
                                                                "ct_47_r" = "lm.tores.primary.ct_47", 
                                                                "ct_48_r" = "lm.tores.primary.ct_48",
                                                                "ct_49_r" = "lm.tores.primary.ct_49", 
                                                                "ct_50_r" = "lm.tores.primary.ct_50",
                                                                "ct_51_r" = "lm.tores.primary.ct_51", 
                                                                "ct_52_r" = "lm.tores.primary.ct_52",
                                                                "ct_53_r" = "lm.tores.primary.ct_53", 
                                                                "ct_54_r" = "lm.tores.primary.ct_54",
                                                                "ct_55_r" = "lm.tores.primary.ct_55", 
                                                                "ct_56_r" = "lm.tores.primary.ct_56",
                                                                "ct_57_r" = "lm.tores.primary.ct_57", 
                                                                "ct_58_r" = "lm.tores.primary.ct_58",
                                                                "ct_59_r" = "lm.tores.primary.ct_59", 
                                                                "ct_60_r" = "lm.tores.primary.ct_60",
                                                                "ct_61_r" = "lm.tores.primary.ct_61", 
                                                                "ct_62_r" = "lm.tores.primary.ct_62",
                                                                "ct_63_r" = "lm.tores.primary.ct_63", 
                                                                "ct_64_r" = "lm.tores.primary.ct_64",
                                                                "ct_65_r" = "lm.tores.primary.ct_65", 
                                                                "ct_66_r" = "lm.tores.primary.ct_66",
                                                                "ct_67_r" = "lm.tores.primary.ct_67", 
                                                                "ct_68_r" = "lm.tores.primary.ct_68")
write.csv(ct.residual,"ct.residual.csv", row.names = FALSE)

ct.residual<-read.csv('ct.residual.csv')
ct.residual<- merge(irritability.data, ct.residual, by = "subnum_char_b")
# replace NA with .
ct.residual[is.na(ct.residual)]<-"."
write.csv(ct.residual,"ct.residual.csv", row.names = FALSE)
#write_csv(main.data, "irritability_new_data.csv")
write.dat(ct.residual, "ct.residual")
