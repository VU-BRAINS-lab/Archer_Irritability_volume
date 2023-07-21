library(readxl); library(tidyverse); library(dplyr); 
library(data.table); library(Jmisc); library("lavaan");
library(qgraph); library(psych); library(corrplot); library("psych");
library(ggplot2); library(car); library(compare); library(gdata); 
library(corrplot); library(multiplex); library(moments); library(ltm)
## Set paths ##
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load data ##
data<-read.csv('main.ed.med.icv.residual.csv')

###############################
#### Table 1: Demographics ####
###############################

#Total sample means
meanAge_total <- mean(data$age_b)
meanAge_total

#Total sample sd
sdAge_total <- sd(data$age_b)
sdAge_total

#Total number of males and females (0=male, 1=female)
sexTable_total <- table(data$FEMALE_b)
sexTable_total

#Percentage of females
female_number <-length(which(data$FEMALE_b==1))
percentfemale <- female_number/nrow(data)
percentfemale
male_number <-length(which(data$FEMALE_b==0))
percentmale <- male_number/nrow(data)
percentmale

#Total number of whites (1=non-hispanic white, 0=not)
whiteTable_total <- table(data$NHWHITE_b)
whiteTable_total

#Total number of hispanic (1=hispanic, 0=not)
hispanicTable_total <- table(data$HISPANIC_b)
hispanicTable_total

#Total number of african (1=african, 0=not)
africanTable_total <- table(data$AFRICAN_b)
africanTable_total

#Total number of other (1=other, 0=not)
otherraceTable_total <- table(data$OTHER_b)
otherraceTable_total

#Percentage of each race group
white_number <-length(which(data$NHWHITE_b==1))
percentwhite <- white_number/nrow(data)
percentwhite
hispanic_number <-length(which(data$HISPANIC_b==1))
percenthispanic <- hispanic_number/nrow(data)
percenthispanic
african_number <-length(which(data$AFRICAN_b==1))
percentafrican <- african_number/nrow(data)
percentafrican
otherrace_number <-length(which(data$OTHER_b==1))
percentotherrace <- otherrace_number/nrow(data)
percentotherrace

#Income Summary table
income_total<-table(data$income_b)
income_total

#Percentage of each income_b response
lessthanfivek_number <-length(which(data$income_b==1))
lessthanfivek_number
percentlessthanfivek <- lessthanfivek_number/nrow(data)
percentlessthanfivek*100
fivektotwelvek_number <-length(which(data$income_b==2))
fivektotwelvek_number
percentfivektotwelvek <- fivektotwelvek_number/nrow(data)
percentfivektotwelvek*100
twelvektosixteenk_number <-length(which(data$income_b==3))
twelvektosixteenk_number
percenttwelvektosixteenk <- twelvektosixteenk_number/nrow(data)
percenttwelvektosixteenk*100
sixteenktotwentyfivek_number <-length(which(data$income_b==4))
sixteenktotwentyfivek_number
percentsixteenktotwentyfivek <- sixteenktotwentyfivek_number/nrow(data)
percentsixteenktotwentyfivek*100
twentyfivektothirtyfivek_number <-length(which(data$income_b==5))
twentyfivektothirtyfivek_number
percenttwentyfivektothirtyfivek <- twentyfivektothirtyfivek_number/nrow(data)
percenttwentyfivektothirtyfivek*100
thirtyfivektofiftyk_number <-length(which(data$income_b==6))
thirtyfivektofiftyk_number
percentthirtyfivektofiftyk <- thirtyfivektofiftyk_number/nrow(data)
percentthirtyfivektofiftyk*100 
fiftyktoseventyfivek_number <-length(which(data$income_b==7))
fiftyktoseventyfivek_number
percentfiftyktoseventyfivek <- fiftyktoseventyfivek_number/nrow(data)
percentfiftyktoseventyfivek*100
seventyfivektohundredk_number <-length(which(data$income_b==8))
seventyfivektohundredk_number
percentseventyfivektohundredk <- seventyfivektohundredk_number/nrow(data)
percentseventyfivektohundredk*100
hundredktotwohundrek_number <-length(which(data$income_b==9))
hundredktotwohundrek_number
percenthundredktotwohundrek <- hundredktotwohundrek_number/nrow(data)
percenthundredktotwohundrek*100
overtwohundredk_number <-length(which(data$income_b==10))
overtwohundredk_number
percentovertwohundredk <- overtwohundredk_number/nrow(data)
percentovertwohundredk*100
missingincome_b_number <-length(which(data$income_b=="."))
missingincome_b_number
percentmissingincome_b <- missingincome_b_number/nrow(data)
percentmissingincome_b*100

#Parental Education Summary table
parentaledu_total<-table(data$parent_education_b)
parentaledu_total

#Percentage of each parental education response
neverorkindergarten_number <-length(which(data$parent_education_b==0))
neverorkindergarten_number

first_number <-length(which(data$parent_education_b==1))
first_number

second_number <-length(which(data$parent_education_b==2))
second_number

third_number <-length(which(data$parent_education_b==3))
third_number

fourth_number <-length(which(data$parent_education_b==4))
fourth_number

fifth_number <-length(which(data$parent_education_b==5))
fifth_number

sixth_number <-length(which(data$parent_education_b==6))
sixth_number

seventh_number <-length(which(data$parent_education_b==7))
seventh_number

eighth_number <-length(which(data$parent_education_b==8))
eighth_number

ninth_number <-length(which(data$parent_education_b==9))
ninth_number

tenth_number <-length(which(data$parent_education_b==10))
tenth_number

eleventh_number <-length(which(data$parent_education_b==11))
eleventh_number

nodegree_number <-(neverorkindergarten_number + first_number + second_number + third_number +
                     fourth_number + fifth_number + sixth_number + seventh_number + eighth_number +
                     ninth_number + tenth_number + eleventh_number)
nodegree_number
percentnodegree <- (neverorkindergarten_number + first_number + second_number + third_number +
                      fourth_number + fifth_number + sixth_number + seventh_number + eighth_number +
                      ninth_number + tenth_number + eleventh_number)/nrow(data)
percentnodegree*100

highschoolgradGED_number <-length(which(data$parent_education_b==12))
highschoolgradGED_number
percenthighschoolgradGED <- highschoolgradGED_number/nrow(data)
percenthighschoolgradGED*100

somecollege_number <-length(which(data$parent_education_b==13))
somecollege_number
percentsomecoll <- somecollege_number/nrow(data)
percentsomecoll*100

associate_number <-length(which(data$parent_education_b==14))
associate_number
percentassociate <- associate_number/nrow(data)
percentassociate*100

bachelors_number <-length(which(data$parent_education_b==16))
bachelors_number
percentbach <- bachelors_number/nrow(data)
percentbach*100

masters_number <-length(which(data$parent_education_b==18))
masters_number
percentmasters <- masters_number/nrow(data)
percentmasters*100

profdocschool_number <-length(which(data$parent_education_b==20))
profdocschool_number
percentprofdocschool <- profdocschool_number/nrow(data)
percentprofdocschool*100

missing_parentedu_number <-length(which(data$parent_education_b=="."))
missing_parentedu_number
percent_missing_parentedu <- missing_parentedu_number/nrow(data)
percent_missing_parentedu*100