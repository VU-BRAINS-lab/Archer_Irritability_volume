#Irritability and Volume Longitudinal
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
irritability.data<-data[,c(2:4,6,7,10,12:14,36,126:128,135,297,305:309,311,314,317,1003,1268:1335,1374,1379:1383,1386:1388,1390:1391,1396:1404,1965,2055:2057,2064)]
irritability.data<-as.data.frame(irritability.data)
irritability.data <- irritability.data %>% rename("vol_1" = "smri_vol_cdk_banksstslh_b", 
                                                  "vol_2" = "smri_vol_cdk_cdacatelh_b",
                                                  "vol_3" = "smri_vol_cdk_cdmdfrlh_b", 
                                                  "vol_4" = "smri_vol_cdk_cuneuslh_b",
                                                  "vol_5" = "smri_vol_cdk_ehinallh_b", 
                                                  "vol_6" = "smri_vol_cdk_fusiformlh_b",
                                                  "vol_7" = "smri_vol_cdk_ifpllh_b",
                                                  "vol_8" = "smri_vol_cdk_iftmlh_b",
                                                  "vol_9" = "smri_vol_cdk_ihcatelh_b", 
                                                  "vol_10" = "smri_vol_cdk_locclh_b",
                                                  "vol_11" = "smri_vol_cdk_lobfrlh_b", 
                                                  "vol_12" = "smri_vol_cdk_linguallh_b",
                                                  "vol_13" = "smri_vol_cdk_mobfrlh_b", 
                                                  "vol_14" = "smri_vol_cdk_mdtmlh_b",
                                                  "vol_15" = "smri_vol_cdk_parahpallh_b", 
                                                  "vol_16" = "smri_vol_cdk_paracnlh_b",
                                                  "vol_17" = "smri_vol_cdk_parsopclh_b", 
                                                  "vol_18" = "smri_vol_cdk_parsobislh_b",
                                                  "vol_19" = "smri_vol_cdk_parstgrislh_b", 
                                                  "vol_20" = "smri_vol_cdk_pericclh_b",
                                                  "vol_21" = "smri_vol_cdk_postcnlh_b", 
                                                  "vol_22" = "smri_vol_cdk_ptcatelh_b",
                                                  "vol_23" = "smri_vol_cdk_precnlh_b", 
                                                  "vol_24" = "smri_vol_cdk_pclh_b",
                                                  "vol_25" = "smri_vol_cdk_rracatelh_b", 
                                                  "vol_26" = "smri_vol_cdk_rrmdfrlh_b",
                                                  "vol_27" = "smri_vol_cdk_sufrlh_b", 
                                                  "vol_28" = "smri_vol_cdk_supllh_b",
                                                  "vol_29" = "smri_vol_cdk_sutmlh_b", 
                                                  "vol_30" = "smri_vol_cdk_smlh_b",
                                                  "vol_31" = "smri_vol_cdk_frpolelh_b", 
                                                  "vol_32" = "smri_vol_cdk_tmpolelh_b",
                                                  "vol_33" = "smri_vol_cdk_trvtmlh_b", 
                                                  "vol_34" = "smri_vol_cdk_insulalh_b",
                                                  "vol_35" = "smri_vol_cdk_banksstsrh_b", 
                                                  "vol_36" = "smri_vol_cdk_cdacaterh_b",
                                                  "vol_37" = "smri_vol_cdk_cdmdfrrh_b", 
                                                  "vol_38" = "smri_vol_cdk_cuneusrh_b",
                                                  "vol_39" = "smri_vol_cdk_ehinalrh_b", 
                                                  "vol_40" = "smri_vol_cdk_fusiformrh_b",
                                                  "vol_41" = "smri_vol_cdk_ifplrh_b", 
                                                  "vol_42" = "smri_vol_cdk_iftmrh_b",
                                                  "vol_43" = "smri_vol_cdk_ihcaterh_b", 
                                                  "vol_44" = "smri_vol_cdk_loccrh_b",
                                                  "vol_45" = "smri_vol_cdk_lobfrrh_b", 
                                                  "vol_46" = "smri_vol_cdk_lingualrh_b",
                                                  "vol_47" = "smri_vol_cdk_mobfrrh_b", 
                                                  "vol_48" = "smri_vol_cdk_mdtmrh_b",
                                                  "vol_49" = "smri_vol_cdk_parahpalrh_b", 
                                                  "vol_50" = "smri_vol_cdk_paracnrh_b",
                                                  "vol_51" = "smri_vol_cdk_parsopcrh_b", 
                                                  "vol_52" = "smri_vol_cdk_parsobisrh_b",
                                                  "vol_53" = "smri_vol_cdk_parstgrisrh_b", 
                                                  "vol_54" = "smri_vol_cdk_periccrh_b",
                                                  "vol_55" = "smri_vol_cdk_postcnrh_b", 
                                                  "vol_56" = "smri_vol_cdk_ptcaterh_b",
                                                  "vol_57" = "smri_vol_cdk_precnrh_b", 
                                                  "vol_58" = "smri_vol_cdk_pcrh_b",
                                                  "vol_59" = "smri_vol_cdk_rracaterh_b", 
                                                  "vol_60" = "smri_vol_cdk_rrmdfrrh_b",
                                                  "vol_61" = "smri_vol_cdk_sufrrh_b", 
                                                  "vol_62" = "smri_vol_cdk_suplrh_b",
                                                  "vol_63" = "smri_vol_cdk_sutmrh_b", 
                                                  "vol_64" = "smri_vol_cdk_smrh_b",
                                                  "vol_65" = "smri_vol_cdk_frpolerh_b", 
                                                  "vol_66" = "smri_vol_cdk_tmpolerh_b",
                                                  "vol_67" = "smri_vol_cdk_trvtmrh_b", 
                                                  "vol_68" = "smri_vol_cdk_insularh_b",
                                                  "vol_107" = "smri_vol_cdk_total_b",
                                                  "vol_112" = "smri_vol_scs_crbcortexlh_b",
                                                  "vol_113" = "smri_vol_scs_tplh_b",
                                                  "vol_114" = "smri_vol_scs_caudatelh_b",
                                                  "vol_115" = "smri_vol_scs_putamenlh_b",
                                                  "vol_116" = "smri_vol_scs_pallidumlh_b",
                                                  "vol_119" = "smri_vol_scs_bstem_b",
                                                  "vol_120" = "smri_vol_scs_hpuslh_b",
                                                  "vol_121" = "smri_vol_scs_amygdalalh_b",
                                                  "vol_124" = "smri_vol_scs_aal_b",
                                                  "vol_125" = "smri_vol_scs_vedclh_b",
                                                  "vol_130" = "smri_vol_scs_crbcortexrh_b",
                                                  "vol_131" = "smri_vol_scs_tprh_b",
                                                  "vol_132" = "smri_vol_scs_caudaterh_b",
                                                  "vol_133" = "smri_vol_scs_putamenrh_b",
                                                  "vol_134" = "smri_vol_scs_pallidumrh_b",
                                                  "vol_135" = "smri_vol_scs_hpusrh_b",
                                                  "vol_136" = "smri_vol_scs_amygdalarh_b",
                                                  "vol_138" = "smri_vol_scs_aar_b",
                                                  "vol_139" = "smri_vol_scs_vedcrh_b")

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

irritability.data$cbcl_q86_p_b <- as.numeric(irritability.data$cbcl_q86_p_b)
irritability.data$cbcl_q87_p_b <- as.numeric(irritability.data$cbcl_q87_p_b)
irritability.data$cbcl_q88_p_b <- as.numeric(irritability.data$cbcl_q88_p_b)
irritability.data$cbcl_q95_p_b <- as.numeric(irritability.data$cbcl_q95_p_b)

irritability.data$baseline_irr <- rowMeans(irritability.data[,10:14])

# replace NA with .
#write_csv(main.data, "irritability_new_data.csv")
irritability.data[is.na(irritability.data)]<-"."
write.dat(irritability.data, "irritability.data.long")
