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
irritability.data<-data[,c(2:4,6,7,10,12:14,297,305:309,311,314,317,1003,1268:1335,1374,1379:1383,1386:1388,1390:1391,1396:1404,1965,2055:2057,2064)]
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
Y.primary <- as.matrix(irritability.data[c(20:107)])
lm.tores.primary <- lm(Y.primary ~ age_b + FEMALE_b + NHWHITE_b + AFRICAN_b + HISPANIC_b + 
                         COIL2_b + COIL3_b + COIL4_b + COIL5_b + parent_education_b + medication_b + TICV_b, data = irritability.data)
lm.residuals.primary <- lm.tores.primary$residuals
#add residuals matrix to the data frame and rename the data frame
long.main.ed.med.icv.residual <- irritability.data %>% spread_residuals(lm.tores.primary)
#condense new data frame to just sub_num, weight, and residual column
long.main.ed.med.icv.residual <- long.main.ed.med.icv.residual[c(grep("subnum_char_b|wt_NR_mwacs_b|lm.tores.primary", names(long.main.ed.med.icv.residual)))]
#omit cases with NAs in row
long.main.ed.med.icv.residual <- na.omit(long.main.ed.med.icv.residual)
#save csv file with sub ID, weight, and all residuals
write.csv(long.main.ed.med.icv.residual,"long.main.ed.med.icv.residual.csv", row.names = FALSE)
long.main.ed.med.icv.residual<-read.csv('long.main.ed.med.icv.residual.csv')

#rename
long.main.ed.med.icv.residual <- long.main.ed.med.icv.residual %>% rename("vol_1_r" = "lm.tores.primary.vol_1", 
                                                                "vol_2_r" = "lm.tores.primary.vol_2",
                                                                "vol_3_r" = "lm.tores.primary.vol_3", 
                                                                "vol_4_r" = "lm.tores.primary.vol_4",
                                                                "vol_5_r" = "lm.tores.primary.vol_5", 
                                                                "vol_6_r" = "lm.tores.primary.vol_6",
                                                                "vol_7_r" = "lm.tores.primary.vol_7",
                                                                "vol_8_r" = "lm.tores.primary.vol_8",
                                                                "vol_9_r" = "lm.tores.primary.vol_9", 
                                                                "vol_10_r" = "lm.tores.primary.vol_10",
                                                                "vol_11_r" = "lm.tores.primary.vol_11", 
                                                                "vol_12_r" = "lm.tores.primary.vol_12",
                                                                "vol_13_r" = "lm.tores.primary.vol_13", 
                                                                "vol_14_r" = "lm.tores.primary.vol_14",
                                                                "vol_15_r" = "lm.tores.primary.vol_15", 
                                                                "vol_16_r" = "lm.tores.primary.vol_16",
                                                                "vol_17_r" = "lm.tores.primary.vol_17", 
                                                                "vol_18_r" = "lm.tores.primary.vol_18",
                                                                "vol_19_r" = "lm.tores.primary.vol_19", 
                                                                "vol_20_r" = "lm.tores.primary.vol_20",
                                                                "vol_21_r" = "lm.tores.primary.vol_21", 
                                                                "vol_22_r" = "lm.tores.primary.vol_22",
                                                                "vol_23_r" = "lm.tores.primary.vol_23", 
                                                                "vol_24_r" = "lm.tores.primary.vol_24",
                                                                "vol_25_r" = "lm.tores.primary.vol_25", 
                                                                "vol_26_r" = "lm.tores.primary.vol_26",
                                                                "vol_27_r" = "lm.tores.primary.vol_27", 
                                                                "vol_28_r" = "lm.tores.primary.vol_28",
                                                                "vol_29_r" = "lm.tores.primary.vol_29", 
                                                                "vol_30_r" = "lm.tores.primary.vol_30",
                                                                "vol_31_r" = "lm.tores.primary.vol_31", 
                                                                "vol_32_r" = "lm.tores.primary.vol_32",
                                                                "vol_33_r" = "lm.tores.primary.vol_33", 
                                                                "vol_34_r" = "lm.tores.primary.vol_34",
                                                                "vol_35_r" = "lm.tores.primary.vol_35", 
                                                                "vol_36_r" = "lm.tores.primary.vol_36",
                                                                "vol_37_r" = "lm.tores.primary.vol_37", 
                                                                "vol_38_r" = "lm.tores.primary.vol_38",
                                                                "vol_39_r" = "lm.tores.primary.vol_39", 
                                                                "vol_40_r" = "lm.tores.primary.vol_40",
                                                                "vol_41_r" = "lm.tores.primary.vol_41", 
                                                                "vol_42_r" = "lm.tores.primary.vol_42",
                                                                "vol_43_r" = "lm.tores.primary.vol_43", 
                                                                "vol_44_r" = "lm.tores.primary.vol_44",
                                                                "vol_45_r" = "lm.tores.primary.vol_45", 
                                                                "vol_46_r" = "lm.tores.primary.vol_46",
                                                                "vol_47_r" = "lm.tores.primary.vol_47", 
                                                                "vol_48_r" = "lm.tores.primary.vol_48",
                                                                "vol_49_r" = "lm.tores.primary.vol_49", 
                                                                "vol_50_r" = "lm.tores.primary.vol_50",
                                                                "vol_51_r" = "lm.tores.primary.vol_51", 
                                                                "vol_52_r" = "lm.tores.primary.vol_52",
                                                                "vol_53_r" = "lm.tores.primary.vol_53", 
                                                                "vol_54_r" = "lm.tores.primary.vol_54",
                                                                "vol_55_r" = "lm.tores.primary.vol_55", 
                                                                "vol_56_r" = "lm.tores.primary.vol_56",
                                                                "vol_57_r" = "lm.tores.primary.vol_57", 
                                                                "vol_58_r" = "lm.tores.primary.vol_58",
                                                                "vol_59_r" = "lm.tores.primary.vol_59", 
                                                                "vol_60_r" = "lm.tores.primary.vol_60",
                                                                "vol_61_r" = "lm.tores.primary.vol_61", 
                                                                "vol_62_r" = "lm.tores.primary.vol_62",
                                                                "vol_63_r" = "lm.tores.primary.vol_63", 
                                                                "vol_64_r" = "lm.tores.primary.vol_64",
                                                                "vol_65_r" = "lm.tores.primary.vol_65", 
                                                                "vol_66_r" = "lm.tores.primary.vol_66",
                                                                "vol_67_r" = "lm.tores.primary.vol_67", 
                                                                "vol_68_r" = "lm.tores.primary.vol_68",
                                                                "vol_107_r" = "lm.tores.primary.vol_107",
                                                                "vol_112_r" = "lm.tores.primary.vol_112",
                                                                "vol_113_r" = "lm.tores.primary.vol_113",
                                                                "vol_114_r" = "lm.tores.primary.vol_114",
                                                                "vol_115_r" = "lm.tores.primary.vol_115",
                                                                "vol_116_r" = "lm.tores.primary.vol_116",
                                                                "vol_119_r" = "lm.tores.primary.vol_119",
                                                                "vol_120_r" = "lm.tores.primary.vol_120",
                                                                "vol_121_r" = "lm.tores.primary.vol_121",
                                                                "vol_124_r" = "lm.tores.primary.vol_124",
                                                                "vol_125_r" = "lm.tores.primary.vol_125",
                                                                "vol_130_r" = "lm.tores.primary.vol_130",
                                                                "vol_131_r" = "lm.tores.primary.vol_131",
                                                                "vol_132_r" = "lm.tores.primary.vol_132",
                                                                "vol_133_r" = "lm.tores.primary.vol_133",
                                                                "vol_134_r" = "lm.tores.primary.vol_134",
                                                                "vol_135_r" = "lm.tores.primary.vol_135",
                                                                "vol_136_r" = "lm.tores.primary.vol_136",
                                                                "vol_138_r" = "lm.tores.primary.vol_138",
                                                                "vol_139_r" = "lm.tores.primary.vol_139")
write.csv(long.main.ed.med.icv.residual,"long.main.ed.med.icv.residual.csv", row.names = FALSE)

long.main.ed.med.icv.residual<-read.csv('long.main.ed.med.icv.residual.csv')
long.main.ed.med.icv.residual<- (long.main.ed.med.icv.residual[c(1,3:90)])
long.main.ed.med.icv.residual<- merge(irritability.data, long.main.ed.med.icv.residual, by = "subnum_char_b")
# replace NA with .
long.main.ed.med.icv.residual[is.na(long.main.ed.med.icv.residual)]<-"."
#write_csv(main.data, "irritability_new_data.csv")
write.dat(long.main.ed.med.icv.residual, "long.main.ed.med.icv.residual")

