library(compare)
library(gdata)
library(mgcv)
library(mgcViz)
library(qgam)
library(visreg)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#LOAD MPLUS DAT FILE WITH EXTRACTED FACTORS
factor_variables <- c("CBCL03", "CBCL86", "CBCL87", "CBCL88", "CBCL95", "IRRITABILITY", "IRRITABILITY_SE", 
                        "WT_NR_MW", "subnum_char_b", "SITEN", "FAMID")
factor_data <- read.table("Irr_factor.dat", header = FALSE)
colnames(factor_data) = factor_variables

#CONDENSE MPLUS DAT FILE: delete all data except for subject number and factor scores 
factorscores <- factor_data[c(grep("subnum_char|^IRRITABILITY$", names(factor_data)))]
write.csv(factorscores,"Irritability_Factor.csv", row.names = FALSE)

#LOAD CORTICAL VOLUME DAT FILE: merge column names with dat file
dat_name <- sprintf('volume_names.txt',getwd())
dat_file <- sprintf('main.ed.med.icv.residual.dat', getwd())
dat <- read.table(dat_file, header = FALSE, sep = "", dec = ".", na.strings = ".")
dat_n <- scan(dat_name, what ="string")
colnames(dat) <- dat_n

#MERGE THE TWO DAT FILES
dataMerge <-merge(dat, factorscores, by=c("subnum_char_b"), all=TRUE) 

##CORTICAL REGIONS: creating a vector with all cortical regions, then creating a sum variable, then into cubic cm from cubic mm##
cortical_regions <- c("vol_1", "vol_2", "vol_3", "vol_4", "vol_5", "vol_6", "vol_7", "vol_8", "vol_9?", "vol_10", 
                      "vol_11", "vol_12", "vol_13", "vol_14", "vol_15", "vol_16", "vol_17?", "vol_18", "vol_19", "vol_20", 
                      "vol_21", "vol_22", "vol_23", "vol_24", "vol_25?", "vol_26", "vol_27", "vol_28", "vol_29", "vol_30", 
                      "vol_31", "vol_32", "vol_33?", "vol_34", "vol_35", "vol_36", "vol_37", "vol_38", "vol_39", "vol_40",
                      "vol_41?", "vol_42", "vol_43", "vol_44", "vol_45", "vol_46", "vol_47", "vol_48", "vol_49?", "vol_50", 
                      "vol_51", "vol_52", "vol_53", "vol_54", "vol_55", "vol_56", "vol_57?", "vol_58", "vol_59", "vol_60",
                      "vol_61", "vol_62", "vol_63", "vol_64", "vol_65?", "vol_66", "vol_67", "vol_68")

dataMerge$cortical_regions_sum <- rowSums(subset(dataMerge[25:92]))
dataMerge$cortical_regions_sum_cm <- dataMerge$cortical_regions_sum/1000

##CREATE GROUPED VARIABLES FOR COIL AND RACE
dataMerge$RACE <- names(dataMerge[7:9])[max.col(dataMerge[7:9])]
dataMerge$COIL <- names(dataMerge[16:19])[max.col(dataMerge[16:19])]

##DEFINE COVARIATES AS FACTORS OR NUMERIC
dataMerge$FEMALEFACTOR <- as.factor(dataMerge$FEMALE_b)
dataMerge$RACEFACTOR <- as.factor(dataMerge$RACE)
dataMerge$COILFACTOR <- as.factor(dataMerge$COIL)
dataMerge$ageNUMERIC <- as.numeric(dataMerge$age_b)
dataMerge$irritabilityNUMERIC <- as.numeric(dataMerge$IRRITABILITY)
dataMerge$volume <- as.numeric(dataMerge$`vol_57`)
dataMerge$volume <- dataMerge$volume/1000

#RUN GAM MODELS
attach(dataMerge)

corticalGam <- gam(volume ~ ageNUMERIC + FEMALEFACTOR + COILFACTOR + RACEFACTOR +
                     irritabilityNUMERIC, method="REML")
summary(corticalGam)

#Convert the fitted object to the gamViz class to use the tools in mgcViz
corticalGamViz <- getViz(corticalGam)

############ SCATTER PLOTS ###########

#cortical and irritability
plot(corticalGamViz, allTerms = TRUE, select = 5) + 
  labs(x = "Irritability", y = "Cortical Volume") +
  l_points(shape=19, size=0.7, color= "#17b050") +
  l_fitLine(colour = "black", size=1) +
  l_ciLine(mul = 5, colour = "black", linetype = 2, size=0.5) +
  theme(axis.title.x=element_text(size=11, colour = "#17b050"),
        axis.title.y=element_text(size=11, colour = "black"),
        axis.text.x=element_text(size=10, colour="black"),
        axis.text.y=element_text(size=10, colour="black")) +
  theme(legend.position = "none")

ggsave(file="Scatterplot_Vol57_Irr.png", width = 3, height = 3, units = 'in', dpi = 300)

