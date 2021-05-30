
#Loading the required packages and datasets
library("tidyverse")
library("patchwork")
library("dplyr")
library("forcats")
#install.packages("jtools")
library("ggplot2")
library("jtools")
install.packages("foreign")

dat <- da36151.0007     #Non-resident dataset 
datHH <- da36151.0002   #Household dataset
dat_indiv<-da36151.0001 #individual datset

##Join the datasets

#picking up relevant columns from datHH
datHH_slim <- datHH[,c("HHID","HHSPLITID","STATEID","DISTID","PSUID",
                       "INCOME","INCOMEPC","INCREMIT","NPERSONS","COPC", 
                       "URBAN2011","NADULTM","NADULTF","NCHILDM","NCHILDF","NTEENM","NTEENF",
                       "NELDERM","NELDERF")] 
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

#cleaning: removing observations with missing values of 
#location of nonresident(abroad/same state/another state) 
dat_joined<-dat_joined%>% drop_na(NR8)

dim(dat_joined)
dat_joined%>%group_by(NR8)%>%count()

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
dat_joined$edu_num<-factor(dat_joined$edu_num)

#create a variable for debt if hhY_minus_remt less than 0:
dat_joined$hh_in_debt<-ifelse((dat_joined$hhY_minus_Remt<0),1,0)

#create a new column for gender with {0,1}
dat_joined$Gender<- ifelse ((dat_joined$NR5 == "(1) Male"),1,0)

#renaming the age column from NR6 to Age
dat_joined <- dat_joined %>% rename(Age=NR6)

#renaming NR10 to education
dat_joined <- dat_joined %>% rename(educ = NR10)

#renaming the remittance column (remittance sent to hh by that non-resident)
#from NR13A to Remittance
dat_joined <- dat_joined %>% rename(Remittance=NR13A)

#renaming NR13B to rev_remit (reverse remittance)
dat_joined <- dat_joined %>% rename(rev_remit=NR13B)

#renaming NPERSONS to hh_size
dat_joined <- dat_joined %>% rename(hh_size=NPERSONS)

#renaming NNR to num_non_res
dat_joined <- dat_joined %>% rename(num_non_res=NNR)

#renaming NR4 to relation_to_hh
dat_joined <- dat_joined %>% rename(relation_to_hh=NR4)

#renaming NR7 to marital_status
dat_joined <- dat_joined %>% rename(marital_status=NR7)

#renaming NR5 to sex
dat_joined <- dat_joined %>% rename(sex=NR5)

#cleaning: removing observations with missing values of sex
dat_joined<-dat_joined%>% drop_na(sex)

#creating 3 dummy variables for marital status: Married, unmarried, widowed
dat_joined<-dat_joined %>% mutate(married = ifelse(marital_status == "(1) Married",1,0))

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

dat_joined%>% filter(!is.na(Remittance))%>%count(!is.na(NR13B))

#create a column for log of hhY_minus_Remt: log_hhY_minus_Remt
dat_joined<-dat_joined %>% mutate (log_hhY_minus_Remt = log(hhY_minus_Remt + 777799))

#Rename Gender variable to specify: male or female
dat_joined <- dat_joined %>% rename('Gender(Female=1)'=Gender)

#Add variable to combine number of children and teens
dat_joined<-dat_joined %>% mutate (NMinors=NTEENF+NTEENM+NCHILDM+NCHILDF);

#Add variable to combine number of male and female minors separately
dat_joined<-dat_joined %>% mutate (NMinorsF=NTEENF+NCHILDF);
dat_joined<-dat_joined %>% mutate (NMinorsM=NTEENM+NCHILDM);

#Add variable to calculate fraction of minors in family
dat_joined<-dat_joined %>% mutate (fraction_of_minors=NMinors/hh_size);

#Add variable to combine number of elderly people
dat_joined<-dat_joined %>% mutate (NElders=NELDERM+NELDERF);

#making separate datasets for each state: same state/another state/abroad
dat_same_state <- dat_joined %>% filter(NR8 == "(1) same state")
dat_another_state <- dat_joined %>% filter(NR8 == "(2) another state")
dat_abroad <- dat_joined %>% filter(NR8 == "(3) abroad")

dim(dat_same_state)     #8647 observations
dim(dat_another_state)  #4698 observations
dim(dat_abroad)         #881 observations

###Regression Analysis: Starting with age, gender and num_non_res

#Regression analysis of non-residents abroad
lm_1_abroad<-lm(non_resident_remits~Age+`Gender(Female=1)`+num_non_res,data=dat_abroad) 
lm_2_abroad<-lm(Remittance~Age+`Gender(Female=1)`+num_non_res,data=dat_abroad)

#Regression analysis of non-residents in the same state
lm_1_same_state<-lm(non_resident_remits~Age+`Gender(Female=1)`+num_non_res,data=dat_same_state) 
lm_2_same_state<-lm(Remittance~Age+`Gender(Female=1)`+num_non_res,data=dat_same_state)

#Regression analysis of non-residents in another state
lm_1_another_state<-lm(non_resident_remits~Age+`Gender(Female=1)`+num_non_res,data=dat_another_state) 
lm_2_another_state<-lm(Remittance~Age+`Gender(Female=1)`+num_non_res,data=dat_another_state)

#Oct 22, 2020
###Regression Analysis: with age, gender, education level, urban/rural, (household income-remittances), hh_size, EAG and num_non_res
#Age^2 instead of age to account for non-linearities to age

#Regression analysis of non-residents abroad
lm_1_a<-lm(non_resident_remits~Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_abroad) 
lm_2_a<-lm(Remittance~Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_abroad)

#Regression analysis of non-residents in the same state
lm_1_ss<-lm(non_resident_remits~Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_same_state) 
lm_2_ss<-lm(Remittance~Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_same_state)

#Regression analysis of non-residents in another state
lm_1_as<-lm(non_resident_remits~Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_another_state) 
lm_2_as<-lm(Remittance~Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_another_state)

#Regression with interaction terms
lm_2_as_interaction<-lm(Remittance~(Age^2)*`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt*`Gender(Female=1)`+urban+EAG,data=dat_another_state)


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


#Regression analysis of non-residents abroad
lm_1_a_Nov4<-lm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_abroad) 
lm_2_a_Nov4<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_abroad)

#Jan 6, 2021
mod.both1<-stepAIC(lm_1_a_Nov4, direction="both",trace = F)
summary(mod.both1)
lm_1_a_Nov4<-lm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num2+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_abroad) 
lm_1_as<-lm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_another_state)
lm_1_ss<-lm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_same_state)
summary(lm_1_ss)
summary(lm_1_a_Nov4)
plotResiduals(lm_1_as)
plotResiduals(lm_1_a_Nov4)
plotResiduals(lm_1_ss)
#Residual plot: 2 parallel lines
#Try transforming the model

#try using a logistic regression model:

#Same state
logit_model_ss_1<-glm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_same_state,binomial)
summary((logit_model_ss_1))
plot(logit_model_ss)

#Another state
logit_model_as_1<-glm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_another_state,binomial)
summary((logit_model_as))
plot(logit_model_as)

#Abroad
logit_model_a_1<-glm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_abroad,binomial)
##ALGORITHM DID NOT CONVERGE
summary((logit_model_a))
probabilities <- predict(logit_model_a, type = "response")
dat_abroad$logit<- log(probabilities/(1-probabilities))

#Remittances analysis
#Same state:
lm_2_ss_Jan6<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_same_state)
summary(lm_2_ss_Jan6)
plotResiduals(lm_2_ss_Jan6)
#normally distributed!

#Another state:
lm_2_as_Jan6<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_another_state)
summary(lm_2_as_Jan6)
plotResiduals(lm_2_as_Jan6)
#roughly normally distributed!

#Abroad:
dat_abroad<-dat_abroad%>%drop_na(Remittance)
dat_abroad<-dat_abroad%>%drop_na(log_hhY_minus_Remt)
options(na.action="na.exclude")
dat_abroad$Remittance[which(is.nan(dat_abroad$Remittance))] = NA
dat_abroad$Remittance[which(dat_abroad$Remittance==Inf)] = NA
dat_abroad$log_hhY_minus_Remt[which(dat_abroad$log_hhY_minus_Remt==Inf)] = NA
dat_abroad$log_hhY_minus_Remt[which(dat_abroad$log_hhY_minus_Remt==-Inf)] = NA
dat_abroad$log_hhY_minus_Remt[which(is.nan(dat_abroad$log_hhY_minus_Remt))] = NA

lm_2_a_Jan6<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_abroad)
remittances_abroad.res = resid(lm_2_a_Jan6)
#We now plot the residual against the observed values of the variable waiting.
plot(lm_2_a_Jan6$fitted.values, lm_2_a_Jan6$residuals, ylab="Residuals", xlab="Fitted values") 
abline(0, 0) 
#approximately normally distributed

write.csv(tidy(logit_model_ss_1), file.path(path, "logit_model_ss_1.csv"))
write.csv(tidy(logit_model_as_1), file.path(path, "logit_model_as_1.csv"))
write.csv(tidy(lm_2_ss_Jan6), file.path(path, "lm_2_ss_now.csv"))
write.csv(tidy(lm_2_as_Jan6), file.path(path, "lm_2_as_Jan6<.csv"))
write.csv(tidy(lm_2_a_Jan6), file.path(path, "lm_2_a_Jan6.csv"))

AIC(lm_2_a_Jan6)
BIC(lm_2_a_Jan6)

#both forward and backwards stepwise selecyion
library("MASS")
mod.both<-stepAIC(lm_2_a_Nov4, direction="both",trace = F)
summary(mod.both)
AIC(mod.both)
BIC(mod.both)

#the previous model is better

######################################
# LASSO
######################################
library(glmnet)
library(coefplot)

#Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_abroad)
xfactor<-model.matrix(non_resident_remits ~ (.)^2, data=ggdat)[,-1]

x1<-model.matrix(non_resident_remits ~ Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_abroad)
mod.lasso<-cv.glmnet(x=x1,y=dat_abroad$non_resident_remits,alpha=1) #an object of class 
#"cv.glmnet" is returned,
#which is a list with the ingredients of the cross-validation fit. 

coef(mod.lasso, "lambda.min") #minimum cross validation error
coef(mod.lasso, "lambda.1se") #most regularized model such that error
#is within one standard error of the minimum
extract.coef(mod.lasso)

#######################################

#Comparison of propensity of non-resident to remit across kind of migration and sex
dat_abroad%>%filter(sex=='(1) Male') %>% pull(non_resident_remits) %>% summary()
dat_abroad%>%filter(sex=='(2) Female') %>% pull(non_resident_remits) %>% summary()

dat_same_state%>%filter(sex=='(1) Male') %>% pull(non_resident_remits) %>% summary()
dat_same_state%>%filter(sex=='(2) Female') %>% pull(non_resident_remits) %>% summary()

dat_another_state%>%filter(sex=='(1) Male') %>% pull(non_resident_remits) %>% summary()
dat_another_state%>%filter(sex=='(2) Female') %>% pull(non_resident_remits) %>% summary()

#Comparison of remittances across kind of migration and sex
dat_abroad%>%filter(sex=='(1) Male') %>% pull(Remittance) %>% summary()
dat_abroad%>%filter(sex=='(2) Female') %>% pull(Remittance) %>% summary()

dat_same_state%>%filter(sex=='(1) Male') %>% pull(Remittance) %>% summary()
dat_same_state%>%filter(sex=='(2) Female') %>% pull(Remittance) %>% summary()

dat_another_state%>%filter(sex=='(1) Male') %>% pull(Remittance) %>% summary()
dat_another_state%>%filter(sex=='(2) Female') %>% pull(Remittance) %>% summary()

#Is the difference of means significant?

#abroad
t.test(x=dat_abroad%>%filter(sex=='(1) Male') %>% pull(Remittance), y=dat_abroad%>%filter(sex=='(2) Female') %>% pull(Remittance))
#the difference in means is not significant

#same state
t.test(x=dat_same_state%>%filter(sex=='(1) Male') %>% pull(Remittance), y=dat_same_state%>%filter(sex=='(2) Female') %>% pull(Remittance))
#the difference in means IS significant

#another state
t.test(x=dat_another_state%>%filter(sex=='(1) Male') %>% pull(Remittance), y=dat_another_state%>%filter(sex=='(2) Female') %>% pull(Remittance))
#the difference in means is not significant

#Looking at remittances sent by nonresidents of age<=15
dat_joined%>% filter(Age<=15) %>% group_by(NR8) %>% count(is.na(Remittance))

##STATE FIXED EFFECTS

fixed.dum <-lm(y ~ x1 + factor(country) - 1, data = dataPanel101)
summary(fixed.dum)

###Same state
logit_model_ss_1<-glm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_same_state,binomial)
summary((logit_model_ss_1))
plot(logit_model_ss)
#Fixed effect: removed EAG and added factor(STATEID)
logit_model_ss_1_fe<-glm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+married+factor(STATEID),data=dat_same_state,binomial)
summary(logit_model_ss_1_fe)
write.csv(tidy(logit_model_ss_1_fe), file.path(path, "logit_model_ss_1_fe.csv"))

###Another state
logit_model_as_1<-glm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_another_state,binomial)
summary((logit_model_as))
plot(logit_model_as)
#Fixed effect:
logit_model_as_1_fe<-glm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+married+factor(STATEID),data=dat_another_state,binomial)
summary(logit_model_as_1_fe)
write.csv(tidy(logit_model_as_1_fe), file.path(path, "logit_model_as_1_fe.csv"))


###Abroad
logit_model_a_1<-glm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_abroad,binomial)
##ALGORITHM DID NOT CONVERGE
summary((logit_model_a))
probabilities <- predict(logit_model_a, type = "response")
dat_abroad$logit<- log(probabilities/(1-probabilities))
#Fixed effect:
logit_model_a_1_fe<-glm(non_resident_remits~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+married+factor(STATEID),data=dat_abroad,binomial)
#Algorithm still does not converge

#Remittances analysis
###Same state:
lm_2_ss_Jan6<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_same_state)
summary(lm_2_ss_Jan6)
plotResiduals(lm_2_ss_Jan6)
#normally distributed!
#Fixed effect: Marital status variables are removed
lm_2_ss_final_fe<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+factor(STATEID),data=dat_same_state)
write.csv(tidy(lm_2_ss_final_fe), file.path(path, "lm_2_ss_final_fe.csv"))

###Another state: Marital status variables are removed
lm_2_as_Jan6<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG,data=dat_another_state)
write.csv(tidy(lm_2_as_Jan6), file.path(path, "lm_2_as_Jan6.csv"))
summary(lm_2_as_Jan6)
plotResiduals(lm_2_as_Jan6)
#roughly normally distributed!
#Fixed effect: Marital status variables are removed
lm_2_as_Jan6_fe<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+factor(STATEID),data=dat_another_state)
write.csv(tidy(lm_2_as_Jan6_fe), file.path(path, "lm_2_as_Jan6_fe.csv"))

###Abroad
lm_2_a_Jan6<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+EAG+married,data=dat_abroad)
write.csv(tidy(lm_2_a_Jan6), file.path(path, "lm_2_a_Jan6.csv"))
#Fixed effect:
lm_2_a_Jan6_fe<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+married+factor(STATEID),data=dat_abroad)
write.csv(tidy(lm_2_a_Jan6_fe), file.path(path, "lm_2_a_Jan6_fe.csv"))


#### incorporating new variables: March 12,2021
lm_2_ss_march122021<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+factor(STATEID)+NMinors+NElders,data=dat_same_state)
summary(lm_2_ss_march122021)
write.csv(tidy(lm_2_ss_march122021), file.path(path, "lm_2_ss_march122021_fe.csv"))

##separate variables for male and female minors
lm_2_ss_march122021_part2<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+factor(STATEID)+NMinorsF+NMinorsM+NElders,data=dat_same_state)
summary(lm_2_ss_march122021_part2)
write.csv(tidy(lm_2_ss_march122021_part2), file.path(path, "lm_2_ss_march122021_part2.csv"))

#using fraction of minors instead of number of minors
lm_2_ss_march122021_part3<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+hh_size+urban+factor(STATEID)+fraction_of_minors+NElders,data=dat_same_state)
summary(lm_2_ss_march122021_part3)
write.csv(tidy(lm_2_ss_march122021_part3), file.path(path, "lm_2_ss_march122021_part3.csv"))

#without hh_size as an explanatory variable
lm_2_ss_march122021_part4<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NElders+NElders,data=dat_same_state)
summary(lm_2_ss_march122021_part4)
write.csv(tidy(lm_2_ss_march122021_part4), file.path(path, "lm_2_ss_march122021_part4.csv"))

lm_2_ss_march122021_part5<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+urban+hh_size+factor(STATEID)+NElders,data=dat_same_state)
summary(lm_2_ss_march122021_part4)
write.csv(tidy(lm_2_ss_march122021_part5), file.path(path, "lm_2_ss_march122021_part5.csv"))

lm_2_ss_march122021_part6<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+urban+factor(STATEID)+NElders,data=dat_same_state)
summary(lm_2_ss_march122021_part4)
write.csv(tidy(lm_2_ss_march122021_part6), file.path(path, "lm_2_ss_march122021_part6.csv"))

lm_2_ss_march122021_part7<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+urban+factor(STATEID)+NELDERF+NELDERM,data=dat_same_state)
summary(lm_2_ss_march122021_part7)
write.csv(tidy(lm_2_ss_march122021_part7), file.path(path, "lm_2_ss_march122021_part7.csv"))

#put marital status and kids back in
#check for interaction factors
#gender and logIncome
# gender story so anything
#maybe use poly(age)
# urban and gender

### March 18, 2021
lm_2_ss_march182021_1<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NElders+NElders+married,data=dat_same_state)
summary(lm_2_ss_march182021_1)
write.csv(tidy(lm_2_ss_march182021_1), file.path(path, "lm_2_ss_march182021_1.csv"))

#poly(Age) instead of Age+Age^2
#Age+Age^2:
lm_2_ss_march182021_2<-lm(log(Remittance)~Age+Age^2+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NElders+NElders+married,data=dat_same_state)
summary(lm_2_ss_march182021_2)
write.csv(tidy(lm_2_ss_march182021_2), file.path(path, "lm_2_ss_march182021_2.csv"))
#poly(Age):
lm_2_ss_march182021_2_2<-lm(log(Remittance)~poly(Age,2)+`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NElders+NElders+married,data=dat_same_state%>%drop_na(Age))
summary(lm_2_ss_march182021_2_2)
write.csv(tidy(lm_2_ss_march182021_2_2), file.path(path, "lm_2_ss_march182021_2_2.csv"))

write.csv(dat_same_state,file.path(path, "dat_same_state.csv"))

#March 24,2021

#Using interaction terms
#age and gender?
#gender and education?
#gender and married

#interacting gender and married
lm_2_ss_march242021_2<-lm(log(Remittance)~poly(Age)+`Gender(Female=1)`*married+num_non_res+edu_num+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NElders,data=dat_same_state%>%drop_na(Age))
summary(lm_2_ss_march242021_2)
write.csv(tidy(lm_2_ss_march242021_2), file.path(path, "lm_2_ss_march242021_2.csv"))
#not significant

#interacting poly(Age and gender)
age_gender<-lm(log(Remittance)~poly(Age)*`Gender(Female=1)`+num_non_res+edu_num+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NElders+married,data=dat_same_state%>%drop_na(Age))
summary(age_gender)
write.csv(tidy(age_gender), file.path(path, "age_gender.csv"))
#not significant

#interacting gender and education
edu_gender<-lm(log(Remittance)~poly(Age)+`Gender(Female=1)`*edu_num+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NElders+married,data=dat_same_state%>%drop_na(Age))
summary(edu_gender)
#not significant

#interacting age and education
edu_age<-lm(log(Remittance)~poly(Age)*edu_num+`Gender(Female=1)`+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NElders+NElders+married,data=dat_same_state%>%drop_na(Age))
summary(edu_age)
#significant

#with NELDERM and NELDERF instead of NElders?
edu_age2<-lm(log(Remittance)~poly(Age)*edu_num+`Gender(Female=1)`+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NELDERF+NELDERM+married,data=dat_same_state%>%drop_na(Age))
summary(edu_age2)


#interacting gender and log_hhY_minus_Remt
logHHY_age2<-lm(log(Remittance)~poly(Age)+edu_num+poly(Age):edu_num+`Gender(Female=1)`+`Gender(Female=1)`:log_hhY_minus_Remt+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NELDERF+NELDERM+married,data=dat_same_state%>%drop_na(Age))
summary(logHHY_age2)
#not significant

#interacting gender and num_non_res
gender_numnonres<-lm(log(Remittance)~poly(Age)+edu_num+poly(Age):edu_num+`Gender(Female=1)`+`Gender(Female=1)`:num_non_res+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NELDERF+NELDERM+married,data=dat_same_state%>%drop_na(Age))
summary(gender_numnonres)
#significant!

#interacting gender and urban
gender_urban<-lm(log(Remittance)~poly(Age)+edu_num+poly(Age):edu_num+`Gender(Female=1)`+`Gender(Female=1)`:num_non_res+`Gender(Female=1)`:urban+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NELDERF+NELDERM+married,data=dat_same_state%>%drop_na(Age))
summary(gender_urban)
#not significant

#interacting gender and NMinorsF
gender_NMinorsF<-lm(log(Remittance)~poly(Age)+edu_num+poly(Age):edu_num+`Gender(Female=1)`+`Gender(Female=1)`:num_non_res+`Gender(Female=1)`:NMinorsF+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NELDERF+NELDERM+married,data=dat_same_state%>%drop_na(Age))
summary(gender_NMinorsF)
#significant!
write.csv(tidy(gender_NMinorsF), file.path(path, "best_interactions.csv"))

#interacting gender and  NMinorsM and NMinorsF
gender_NMinors_both<-lm(log(Remittance)~poly(Age)+edu_num+poly(Age):edu_num+`Gender(Female=1)`+`Gender(Female=1)`:num_non_res+`Gender(Female=1)`:NMinorsF+`Gender(Female=1)`:NMinorsM+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NELDERF+NELDERM+married,data=dat_same_state%>%drop_na(Age))
summary(gender_NMinors_both)
#NMinorsM is not significant

#interacting gender and NMinorsM
gender_NMinorsM<-lm(log(Remittance)~poly(Age)+edu_num+poly(Age):edu_num+`Gender(Female=1)`+`Gender(Female=1)`:num_non_res+`Gender(Female=1)`:NMinorsM+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NELDERF+NELDERM+married,data=dat_same_state%>%drop_na(Age))
summary(gender_NMinorsM)
#not significant!

gender_NMinorandElderF<-lm(log(Remittance)~poly(Age)+edu_num+Age:edu_num+`Gender(Female=1)`+`Gender(Female=1)`:num_non_res+`Gender(Female=1)`:NMinorsF+`Gender(Female=1)`:NElders+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NELDERF+NELDERM+married,data=dat_same_state%>%drop_na(Age))
summary(gender_NMinorandElderF)
#not significant

gender_NMinorsF<-lm(log(Remittance)~poly(Age)+edu_num+Age:edu_num+`Gender(Female=1)`+`Gender(Female=1)`:num_non_res+`Gender(Female=1)`:NMinorsF+num_non_res+log_hhY_minus_Remt+urban+factor(STATEID)+NMinorsF+NMinorsM+NELDERF+NELDERM+married,data=dat_same_state%>%drop_na(Age))
summary(gender_NMinorsF)

#Correlations: age is maybe correlated with married to some extent

