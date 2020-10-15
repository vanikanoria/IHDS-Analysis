
#Loading the required packages and datasets
library("tidyverse")
library("patchwork")
library("dplyr")
library("forcats")
dat <- da36151.0007     #Non-resident dataset 
datHH <- da36151.0002   #Household dataset
dat_indiv<-da36151.0001 #individual datset

##Join the datasets

#picking up relevant columns from datHH
datHH_slim <- datHH[,c("HHID","HHSPLITID","STATEID","DISTID","PSUID","INCOME","INCOMEPC","INCREMIT","NPERSONS","COPC")] 
#picking up relevant columns from dat
dat_slim <- dat[,c("HHID","HHSPLITID","STATEID","DISTID","PSUID","NR8","NR13A","NR13B","NR10","NR5","NR6","NNR")]
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

dat_joined$Gender<- ifelse ((dat_joined$NR5 == "(1) Male"),1,0)

dat_joined$non_resident_remits<- !(is.na(dat_joined$NR13A))

#have to figure out education
#dat_joined$Years_of_education<-mutate(dat_joined$edu_level)

#making separate datasets for each state: same state/another state/abroad
dat_same_state <- dat_joined %>% filter(NR8 == "(1) same state")
dat_another_state <- dat_joined %>% filter(NR8 == "(2) another state")
dat_abroad <- dat_joined %>% filter(NR8 == "(3) abroad")

dim(dat_same_state)     #8647 observations
dim(dat_another_state)  #4698 observations
dim(dat_abroad)         #881 observations

#Regression Analysis: Starting with age, gender and NNRn
#create a new column for gender with {0,1}

#Regression analysis of non-residents abroad
lm.fit_1_abroad<-lm(non_resident_remits~NR6+Gender+NNR,data=dat_abroad) 
lm.fit_2_abroad<-lm(NR13A~NR6+Gender+NNR,data=dat_abroad)

#Regression analysis of non-residents in the same state
lm.fit_1_same_state<-lm(non_resident_remits~NR6+Gender+NNR,data=dat_same_state) 
lm.fit_2_same_state<-lm(NR13A~NR6+Gender+NNR,data=dat_same_state)

#Regression analysis of non-residents in another state
lm.fit_1_another_state<-lm(non_resident_remits~NR6+Gender+NNR,data=dat_another_state) 
lm.fit_2_another_state<-lm(NR13A~NR6+Gender+NNR,data=dat_another_state)

#Regression with interaction terms

