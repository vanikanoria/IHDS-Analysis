
#Loading the required packages and datasets
library("tidyverse")
library("patchwork")
library("dplyr")
library("forcats")
#install.packages("jtools")
library("ggplot2")
library("jtools")

dat <- da36151.0007     #Non-resident dataset 
datHH <- da36151.0002   #Household dataset
dat_indiv<-da36151.0001 #individual datset

##Join the datasets

#picking up relevant columns from datHH
datHH_slim <- datHH[,c("HHID","HHSPLITID","STATEID","DISTID","PSUID","INCOME","INCOMEPC","INCREMIT","NPERSONS","COPC", "URBAN2011")] 
#picking up relevant columns from dat
dat_slim <- dat[,c("HHID","HHSPLITID","STATEID","DISTID","PSUID","NR8","NR13A","NR13B","NR10","NR5","NR6","NNR","NR7","NR4")]
dat_joined<- left_join(dat_slim,datHH_slim, by= c("HHID"="HHID","HHSPLITID"="HHSPLITID","STATEID"="STATEID","DISTID"="DISTID","PSUID"="PSUID"))
dim(dat_joined)

#cleaning: checking for missing values
length(dat_joined$NPERSONS)
table(is.na(dat_joined$NPERSONS))
table(is.na(datHH$NPERSONS))
table(is.na(dat_joined$INCOME))
table(is.na(datHH$INCOME))

#make new column collapsing by education level
dat_joined$edu_level<-fct_collapse(dat_joined$NR10, "None" = c("(00) none"),
                                   "Classes 1-5" = c("(01) 1st class","(02) 2nd class","(03) 3rd class",
                                                     "(04) 4th class", "(05) 5th class"),
                                   "Classes 6-10" = c("(06) 6th class","(07) 7th class","(08) 8th class",
                                                      "(09) 9th class","(10) Secondary"),
                                   "Classes 11-12" = c("(11) 11th Class","(12) High Secondary"),
                                   "1-2 years post-secondary"= c("(13) 1 year post-secondary",
                                                                 "(14) 2 years post-secondary"),
                                   "Bachelors"= "(15) Bachelors", "Above Bachelors"= "(16) Above Bachelors")

dat_joined$edu_num<-fct_collapse(dat_joined$NR10, "0" = c("(00) none"),
                                                        "1" = c("(01) 1st class","(02) 2nd class","(03) 3rd class",
                                                                          "(04) 4th class", "(05) 5th class"),
                                                        "2" = c("(06) 6th class","(07) 7th class","(08) 8th class",
                                                                           "(09) 9th class","(10) Secondary"),
                                                        "3" = c("(11) 11th Class","(12) High Secondary"),
                                                        "4"= c("(13) 1 year post-secondary",
                                                                                      "(14) 2 years post-secondary"),
                                                        "5"= "(15) Bachelors", "6"= "(16) Above Bachelors")


dat_joined$edu_num<- as.numeric(dat_joined$edu_num)-1

#create a new column for gender with {0,1}
dat_joined$Gender<- ifelse ((dat_joined$NR5 == "(1) Male"),1,0)

#renaming the age column from NR6 to Age
dat_joined <- dat_joined %>% rename(Age=NR6)

#renaming the remittance column (remittance sent to hh by that non-resident)
#from NR13A to Remittance
dat_joined <- dat_joined %>% rename(Remittance=NR13A)

#renaming NPERSONS to hh_size
dat_joined <- dat_joined %>% rename(hh_size=NPERSONS)

#renaming NNR to num_non_res
dat_joined <- dat_joined %>% rename(num_non_res=NNR)

#renaming NR4 to relation_to_hh
dat_joined <- dat_joined %>% rename(relation_to_hh=NR4)

#creating dummy variables for relation_to_hh : TO DO!!!!!!!!!!!!!


#renaming NR7 to marital_status
dat_joined <- dat_joined %>% rename(marital_status=NR7)

#creating 3 dummy variables for marital status: Married, unmarried, widowed
dat_joined<-dat_joined %>% mutate(married = ifelse(marital_status == "(1) Married",1,0))
dat_joined<-dat_joined %>% mutate(unmarried = ifelse(marital_status == "(2) Unmarried",1,0))
dat_joined<-dat_joined %>% mutate(widowed = ifelse(marital_status == "(3) Widowed",1,0))

#making a new numerical variable for 'urban' variable using URBAN2011
dat_joined <- dat_joined %>% mutate(urban=as.numeric(URBAN2011)-1)

#boolean variable that returns true if the non-resident remits, false otherwise
dat_joined$non_resident_remits<- !(is.na(dat_joined$Remittance))

#create a column for household income minus remittance sent by that non-resident: hhY_minus_Remt
dat_joined<-dat_joined %>% mutate (hhY_minus_Remt = ifelse(non_resident_remits, INCOME-Remittance,INCOME))

#create a column for log of hhY_minus_Remt: log_hhY_minus_Remt
dat_joined<-dat_joined %>% mutate (log_hhY_minus_Remt = ifelse(hhY_minus_Remt>0,log(hhY_minus_Remt),
                                                               ifelse(hhY_minus_Remt<0, -log(abs(hhY_minus_Remt)), 0)))
#EAG states
#Making a separate column for numeric version of 'STATEID'
dat_joined<-dat_joined %>% mutate(states_numeric=as.numeric(STATEID))
#EAG states: 1 if the household belongs to the less relatively developed states of 
#Jharkhand (1), Uttarakhand (5), Orissa (21), Chhattisgarh (22) and Madhya Pradesh (23), and
#0 otherwise 
dat_joined<-dat_joined %>% mutate(EAG = ifelse(states_numeric %in% c(1,5,21,22,23),1,0))

#making separate datasets for each state: same state/another state/abroad
dat_same_state <- dat_joined %>% filter(NR8 == "(1) same state")
dat_another_state <- dat_joined %>% filter(NR8 == "(2) another state")
dat_abroad <- dat_joined %>% filter(NR8 == "(3) abroad")

dim(dat_same_state)     #8647 observations
dim(dat_another_state)  #4698 observations
dim(dat_abroad)         #881 observations

###Regression Analysis: Starting with age, gender and num_non_res

#Regression analysis of non-residents abroad
lm_1_abroad<-lm(non_resident_remits~Age+Gender+num_non_res,data=dat_abroad) 
lm_2_abroad<-lm(Remittance~Age+Gender+num_non_res,data=dat_abroad)

#Regression analysis of non-residents in the same state
lm_1_same_state<-lm(non_resident_remits~Age+Gender+num_non_res,data=dat_same_state) 
lm_2_same_state<-lm(Remittance~Age+Gender+num_non_res,data=dat_same_state)

#Regression analysis of non-residents in another state
lm_1_another_state<-lm(non_resident_remits~Age+Gender+num_non_res,data=dat_another_state) 
lm_2_another_state<-lm(Remittance~Age+Gender+num_non_res,data=dat_another_state)

#Oct 22, 2020
###Regression Analysis: with age, gender, education level, urban/rural, (household income-remittances), hh_size, EAG and num_non_res
#Age^2 instead of age to account for non-linearities to age

#Regression analysis of non-residents abroad
lm_1_a<-lm(non_resident_remits~Age^2+Gender+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_abroad) 
lm_2_a<-lm(Remittance~Age^2+Gender+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_abroad)

#Regression analysis of non-residents in the same state
lm_1_ss<-lm(non_resident_remits~Age^2+Gender+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_same_state) 
lm_2_ss<-lm(Remittance~Age^2+Gender+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_same_state)

#Regression analysis of non-residents in another state
lm_1_as<-lm(non_resident_remits~Age^2+Gender+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_another_state) 
lm_2_as<-lm(Remittance~Age^2+Gender+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_another_state)

#Regression with interaction terms
lm_2_as_interaction<-lm(Remittance~(Age^2)*Gender+num_non_res+edu_num+log_hhY_minus_Remt*Gender+urban+EAG,data=dat_another_state)


#Getting summaries of regression coefficients
#install.packages("broom")
library("broom")

#Printing regression coefficients as csv files
#Using the function 'tidy' from broom

path <- "/Users/vani/Documents/MyCode/IHDS_analysis/regression_output"
write.csv(tidy(lm_1_as), file.path(path, "lm_1_as.csv"))
write.csv(tidy(lm_2_as), file.path(path, "lm_2_as.csv"))
write.csv(tidy(lm_2_as_interaction), file.path(path, "lm_2_as_interaction.csv"))
write.csv(tidy(lm_1_ss), file.path(path, "lm_1_ss.csv"))
write.csv(tidy(lm_2_ss), file.path(path, "lm_2_ss.csv"))
write.csv(tidy(lm_1_a), file.path(path, "lm_1_a.csv"))
write.csv(tidy(lm_2_a), file.path(path, "lm_2_a.csv"))

#Nov 4, 2020
#some corrections
#1.age besides age^2 to account for non-linearities to age
#2.log for remittance
#3. fixing how I deal with negative income
# min(dat_joined$hhY_minus_Remt) = -777799
# so add that before taking the log
# sum(as.numeric(dat_joined$hhY_minus_Remt<0)) = 291

#create a column for log of hhY_minus_Remt: log_hhY_minus_Remt
dat_joined<-dat_joined %>% mutate (log_hhY_minus_Remt = log(hhY_minus_Remt + 777799))


#Regression analysis of non-residents abroad
lm_1_a_Nov4<-lm(non_resident_remits~Age+Age^2+Gender+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married+unmarried+widowed,data=dat_abroad) 
lm_2_a_Nov4<-lm(log(Remittance)~Age+Age^2+Gender+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married+unmarried+widowed,data=dat_abroad)

write.csv(tidy(lm_1_a_Nov4), file.path(path, "lm_1_a_Nov4.csv"))
write.csv(tidy(lm_2_a_Nov4), file.path(path, "lm_2_a_Nov4.csv"))

# Model 2: dummy variable for education

# dat_joined$edu_num<-dat_joined$NR10, "0" = c("(00) none"),
#                                  "1" = c("(01) 1st class","(02) 2nd class","(03) 3rd class",
#                                          "(04) 4th class", "(05) 5th class"),
#                                  "2" = c("(06) 6th class","(07) 7th class","(08) 8th class",
#                                          "(09) 9th class","(10) Secondary"),
#                                  "3" = c("(11) 11th Class","(12) High Secondary"),
#                                  "4"= c("(13) 1 year post-secondary",
#                                         "(14) 2 years post-secondary"),
#                                  "5"= "(15) Bachelors", "6"= "(16) Above Bachelors")


#creating dummy variables:






