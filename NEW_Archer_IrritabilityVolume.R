#Irritability and Volume
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
data<-readRDS('ABCD_5.1.rds')
# change characters to numeric
indx<-sapply(data, is.character)
data[indx]<-lapply(data[indx], function(x) as.numeric(x))

# make data frame of variables
irritability.data<-data[,c(2,7,13:16,22,25,28,31,1546,1597:2191,2857,10666,11894,35671,36439,36441,40034,42938,42942,
                           42946,42950,42954,42958,42962,42966,42970,42974,42978,42982,42986,42990,
                           42994,42998,43002,43006,43010,43014,43018,43022,43026,43030,43034,43038,
                           43042,43046,43050,43054,43058,43062,43066,43070,43074,43078,43082,43086,
                           43090,43094,43098,43102,43106,43110,43114,43118,43122,43126,43130,43134,
                           43138,43142,43146,43150,43154,43158,43162,43166,43170,43174,43178,43182,
                           43186,43190,43194,43198,43202,43202,43206,43210,43214,43218,43222,43226
                           ,43230,43234,43238,43242,43246,43250,43254,43258,43262,43266,43270,43274
                           ,43278,43336,43339,43342,43345,43348,43351,43354,43357,43360,43363,43366,
                           43369,43372,43375,43378,43381,43384,43387,43390,43393,43396,43399,
                           43408,43420,43423,43426,43429,43438,43441,43444,43447,50958:50959,50989)]
irritability.data<-irritability.data[,-c(611:612,680:683,686:700,703,709:715,718:720)]
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
names(irritability.data)[691]<- c("TICV_b")
names(irritability.data)[702]<- c("parent_education_b")
names(irritability.data)[2]<- c("age_b")
names(irritability.data)[11]<- c("medication_b")
names(irritability.data)[607]<- c("income_b")
names(irritability.data)[3]<- c("NHWHITE_b")
names(irritability.data)[5]<- c("HISPANIC_b")
names(irritability.data)[4]<- c("AFRICAN_b")
names(irritability.data)[6]<- c("OTHER_b")

irritability.data <- irritability.data[,c(700,2,1,610:611,701,3:6,691,7:10,607,702,11,609,612:690,692:699,12:606,608)]
names(irritability.data)[5]<- c("FAMID_b")
irritability.data[is.na(irritability.data)]<-"."

irritability.data$FEMALE_b <- as.factor(irritability.data$FEMALE_b)
irritability.data$NHWHITE_b <- as.factor(irritability.data$NHWHITE_b)
irritability.data$HISPANIC_b <- as.factor(irritability.data$HISPANIC_b)
irritability.data$AFRICAN_b <- as.factor(irritability.data$AFRICAN_b)
irritability.data$OTHER_b <- as.factor(irritability.data$OTHER_b)
irritability.data$medication_b <- as.factor(irritability.data$medication_b)
irritability.data$COIL2_b <- as.factor(irritability.data$COIL2_b)
irritability.data$COIL3_b <- as.factor(irritability.data$COIL3_b)
irritability.data$COIL4_b <- as.factor(irritability.data$COIL4_b)
irritability.data$COIL5_b <- as.factor(irritability.data$COIL5_b)

# change characters to numeric
indx<-sapply(irritability.data, is.character)
irritability.data[indx]<-lapply(irritability.data[indx], function(x) as.numeric(x))

irritability.data <- irritability.data[irritability.data$excluded_missing == '1',]

#Add SA Variables 
test<-data[,grep("smri_area|subnum", x=names(data))]
test2<-test[,-grep("_2|_4|_b_r", x=names(test))]
test2<-test2[,-c(68,69)]

test2 <- test2 %>% rename("sa_1" = "smri_area_cdk_banksstslh_b", 
                                                  "sa_2" = "smri_area_cdk_cdacatelh_b",
                                                  "sa_3" = "smri_area_cdk_cdmdfrlh_b", 
                                                  "sa_4" = "smri_area_cdk_cuneuslh_b",
                                                  "sa_5" = "smri_area_cdk_ehinallh_b", 
                                                  "sa_6" = "smri_area_cdk_fusiformlh_b",
                                                  "sa_7" = "smri_area_cdk_ifpllh_b",
                                                  "sa_8" = "smri_area_cdk_iftmlh_b",
                                                  "sa_9" = "smri_area_cdk_ihcatelh_b", 
                                                  "sa_10" = "smri_area_cdk_locclh_b",
                                                  "sa_11" = "smri_area_cdk_lobfrlh_b", 
                                                  "sa_12" = "smri_area_cdk_linguallh_b",
                                                  "sa_13" = "smri_area_cdk_mobfrlh_b", 
                                                  "sa_14" = "smri_area_cdk_mdtmlh_b",
                                                  "sa_15" = "smri_area_cdk_parahpallh_b", 
                                                  "sa_16" = "smri_area_cdk_paracnlh_b",
                                                  "sa_17" = "smri_area_cdk_parsopclh_b", 
                                                  "sa_18" = "smri_area_cdk_parsobislh_b",
                                                  "sa_19" = "smri_area_cdk_parstgrislh_b", 
                                                  "sa_20" = "smri_area_cdk_pericclh_b",
                                                  "sa_21" = "smri_area_cdk_postcnlh_b", 
                                                  "sa_22" = "smri_area_cdk_ptcatelh_b",
                                                  "sa_23" = "smri_area_cdk_precnlh_b", 
                                                  "sa_24" = "smri_area_cdk_pclh_b",
                                                  "sa_25" = "smri_area_cdk_rracatelh_b", 
                                                  "sa_26" = "smri_area_cdk_rrmdfrlh_b",
                                                  "sa_27" = "smri_area_cdk_sufrlh_b", 
                                                  "sa_28" = "smri_area_cdk_supllh_b",
                                                  "sa_29" = "smri_area_cdk_sutmlh_b", 
                                                  "sa_30" = "smri_area_cdk_smlh_b",
                                                  "sa_31" = "smri_area_cdk_frpolelh_b", 
                                                  "sa_32" = "smri_area_cdk_tmpolelh_b",
                                                  "sa_33" = "smri_area_cdk_trvtmlh_b", 
                                                  "sa_34" = "smri_area_cdk_insulalh_b",
                                                  "sa_35" = "smri_area_cdk_banksstsrh_b", 
                                                  "sa_36" = "smri_area_cdk_cdacaterh_b",
                                                  "sa_37" = "smri_area_cdk_cdmdfrrh_b", 
                                                  "sa_38" = "smri_area_cdk_cuneusrh_b",
                                                  "sa_39" = "smri_area_cdk_ehinalrh_b", 
                                                  "sa_40" = "smri_area_cdk_fusiformrh_b",
                                                  "sa_41" = "smri_area_cdk_ifplrh_b", 
                                                  "sa_42" = "smri_area_cdk_iftmrh_b",
                                                  "sa_43" = "smri_area_cdk_ihcaterh_b", 
                                                  "sa_44" = "smri_area_cdk_loccrh_b",
                                                  "sa_45" = "smri_area_cdk_lobfrrh_b", 
                                                  "sa_46" = "smri_area_cdk_lingualrh_b",
                                                  "sa_47" = "smri_area_cdk_mobfrrh_b", 
                                                  "sa_48" = "smri_area_cdk_mdtmrh_b",
                                                  "sa_49" = "smri_area_cdk_parahpalrh_b", 
                                                  "sa_50" = "smri_area_cdk_paracnrh_b",
                                                  "sa_51" = "smri_area_cdk_parsopcrh_b", 
                                                  "sa_52" = "smri_area_cdk_parsobisrh_b",
                                                  "sa_53" = "smri_area_cdk_parstgrisrh_b", 
                                                  "sa_54" = "smri_area_cdk_periccrh_b",
                                                  "sa_55" = "smri_area_cdk_postcnrh_b", 
                                                  "sa_56" = "smri_area_cdk_ptcaterh_b",
                                                  "sa_57" = "smri_area_cdk_precnrh_b", 
                                                  "sa_58" = "smri_area_cdk_pcrh_b",
                                                  "sa_59" = "smri_area_cdk_rracaterh_b", 
                                                  "sa_60" = "smri_area_cdk_rrmdfrrh_b",
                                                  "sa_61" = "smri_area_cdk_sufrrh_b", 
                                                  "sa_62" = "smri_area_cdk_suplrh_b",
                                                  "sa_63" = "smri_area_cdk_sutmrh_b", 
                                                  "sa_64" = "smri_area_cdk_smrh_b",
                                                  "sa_65" = "smri_area_cdk_frpolerh_b", 
                                                  "sa_66" = "smri_area_cdk_tmpolerh_b",
                                                  "sa_67" = "smri_area_cdk_trvtmrh_b", 
                                                  "sa_68" = "smri_area_cdk_insularh_b")

irritability.data <-merge(irritability.data,test2, by=c("subnum_char_b"))
irritability.data <- irritability.data[,c(1:106,119,121,634,636,639,641,644,646,679,681,703:771)]

write.dat(irritability.data, "irritability.data")



