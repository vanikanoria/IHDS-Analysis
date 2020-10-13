dat <- da36151.0007
datHH <- da36151.0002
dat_indiv<-da36151.0001
library("tidyverse")
library("patchwork")

##------Summary Statistics--------##
#Filter by NR8 - see how many abroad vs internal
table(dat$NR8)

#Group by NR8 and then summary statistics for Rs. Received
dat %>% group_by(NR8) %>% summarize(mean = mean(NR13A,na.rm=TRUE), median = median(NR13A,na.rm=TRUE),min = min(NR13A, na.rm = TRUE),max=max(NR13A, na.rm = TRUE))

#Group by NR8 and then summary statistics for Rs. Sent
dat %>% group_by(NR8) %>% summarize(mean = mean(NR13B,na.rm=TRUE), median = median(NR13B,na.rm=TRUE),min = min(NR13B, na.rm = TRUE),max=max(NR13B, na.rm = TRUE))

#checking to tap code == 999999 in Rs. sent and received
#Rs received:
dat %>% filter(NR13A>=900000) 
#Rs sent:
dat %>% filter(NR13B>=900000)

# --> so not a stop code

#INCOME
#summarizing income data
datHH %>% summarize(mean = mean(INCOME,na.rm=TRUE), median = median(INCOME,na.rm=TRUE),min = min(INCOME, na.rm = TRUE),max=max(INCOME, na.rm = TRUE))
datHH %>% filter(INCOME<0) %>% dim()
datHH %>% filter(INCOME<0) %>% summarize(mean = mean(INCOME,na.rm=TRUE), median = median(INCOME,na.rm=TRUE),min = min(INCOME, na.rm = TRUE),max=max(INCOME, na.rm = TRUE))

#datHH %>% group_by(NR8) %>% summarize(mean = mean(NR13A,na.rm=TRUE), median = median(NR13A,na.rm=TRUE),min = min(NR13A, na.rm = TRUE),max=max(NR13A, na.rm = TRUE))

#checking the 2.5th quantile and 97.5th quantile of INCOME so I can plot between them
quantile(datHH$INCOME, c(0.025, 0.975))
quantile(datHH$INCOME, c(0.01, 0.99))

datHH_mid95<- datHH %>% filter(INCOME>-682.15) %>% filter(INCOME<853164)

#plot income data : HISTOGRAM
ggplot(data=datHH_mid95, aes(x=INCOME)) + #which data to plot
  geom_histogram(bins=50,            #how many bins to use
                 fill = "lightblue", color="black")  + #color the histogram)  
  xlab("INCOME")  + #x axis label
  ylab("Frequency")                  + #y axis label
  
  #ggtitle("") + #add title to plot
  theme_bw()                    + #removes grey background 
  geom_hline(yintercept=0)          

#plot income data : VIOLIN PLOT
ggplot(datHH_mid95, aes(x="", y=INCOME)) +
  geom_violin(fill="lightblue",
              trim = FALSE,
              draw_quantiles = c(0.25,0.5,0.75),
              alpha = 0.5,
              show.legend = FALSE)+
  xlab("")  + #x axis label
  ylab("Frequencies of INCOME")      +
  ggtitle("") + #add title to plot
  theme_bw()  #removes grey background
  #geom_jitter(position=position_jitter(0.2))

#Join databases
library("dplyr")
datHH_slim <- datHH[,c("HHID","HHSPLITID","STATEID","DISTID","PSUID","INCOME","INCOMEPC","INCREMIT","NPERSONS","COPC")] %>% filter(!is.na(INCREMIT))
dat_slim <- dat[,c("HHID","HHSPLITID","STATEID","DISTID","PSUID","NR8","NR13A","NR13B","NR10","NR5","NR6","NNR")]
dim(datHH_slim)
dat_joined<- left_join(dat_slim,datHH_slim, by= c("NR13A"="INCREMIT","HHID"="HHID","HHSPLITID"="HHSPLITID","STATEID"="STATEID","DISTID"="DISTID","PSUID"="PSUID"))
dat_new<-inner_join(dat_slim,datHH_slim, by= c("HHID"="HHID","HHSPLITID"="HHSPLITID","STATEID"="STATEID","DISTID"="DISTID","PSUID"="PSUID"))
dat_indiv_nonres<-left_join(dat_slim,dat_indiv, by= c("HHID"="HHID","HHSPLITID"="HHSPLITID","STATEID"="STATEID","DISTID"="DISTID","PSUID"="PSUID","PERSONID"="PERSONID"))

dim(dat_indiv_nonres)


length(dat_joined$NPERSONS)
table(is.na(dat_joined$NPERSONS))
table(is.na(dat_new$NPERSONS))
table(is.na(datHH$NPERSONS))
table(is.na(dat_joined$INCOME))
table(is.na(datHH$INCOME))

#GROUP BY STATE and then summarize and plot

#Household income stats
dat_joined %>% group_by(NR8)%>% summarize(mean = mean(INCOME,na.rm=TRUE), median = median(INCOME,na.rm=TRUE),min = min(INCOME, na.rm = TRUE),max=max(INCOME, na.rm = TRUE))

#income per capita stats
dat_joined %>% group_by(NR8)%>% summarize(mean = mean(INCOMEPC,na.rm=TRUE), median = median(INCOMEPC,na.rm=TRUE),min = min(INCOMEPC, na.rm = TRUE),max=max(INCOMEPC, na.rm = TRUE))

#cleaning data and removing is.na
dat_joined <- dat_joined %>% filter(!is.na(NR8)) %>% filter(!is.na(INCOME))



#HISTOGRAM
d_pp<-density(dat_joined[dat_joined$NR8=="(1) same state","INCOMEPC"]) # Fit density for +/+
d_pm<-density(dat_joined[dat_joined$NR8=="(2) another state","INCOMEPC"]) # Fit density for +/-
d_mm<-density(dat_joined[dat_joined$NR8=="(3) abroad","INCOMEPC"]) # Fit density for -/-

Ship1.labels <- c(`(1) same state` = "SAME STATE",
                  `(2) another state` = "ANOTHER STATE",
                  `(3) abroad-` = "ABROAD")

bw<-mean(d_pp$bw,d_pm$bw,d_mm$bw) # Use the mean binwidth across densities
min.x<-min(c(d_pp$x,d_pm$x,d_mm$x)) # minimum for x-axis
max.x<-max(c(d_pp$x,d_pm$x,d_mm$x)) # maximum for x-axis
ggplot(data=dat_joined,aes(x=INCOMEPC))+
  geom_histogram(aes(y = ..density..), #plots the density
                 binwidth=(max.x-min.x)/200, #sets bin width
                 fill = "lightblue", color="black")  + #color the histogram
  geom_density(color="darkred",fill="red", alpha = 0.2) +
  #facet_grid(~Ship1, labeller = as_labeller(Ship1.labels))+
  # xlab("Frequencies of iNKT Cells")  + #x axis label
  # ylab("Density")                    + #y axis label
  # ggtitle("Frequencies of Invariant Natural Killer T (iNKT) Cell Populations")+
  theme_bw()                    + #removes grey background 
  geom_hline(yintercept=0) +            #adds a line for the x-axis
  xlim(min.x,max.x) #show the full density plot      

#VIOLIN PLOTS FOR EACH NR8 CATEGORY

#make NNR a factor variable
dat_joined$NNR<-as.factor(dat_joined$NNR)

#make new column with ages categories using the 'age' column NR6
dat_joined<-dat_joined%>% mutate(age_categ<- ifelse(
  NR6<25,"0-24",ifelse(NR6<40,"25-39",ifelse(
    NR6<55,"40-54",ifelse(NR6<70,"55-69",">70")
  ))
))
colnames(dat_joined)[15]<- "Age_categ"
dat_joined$Age_categ<-factor(dat_joined$Age_categ,levels = c("0-24","25-39","40-54","55-69",">70"))

#make new column collapsing by education level
#collapsing by broad classes
#install.packages("forcats")
library(forcats)
dat_joined$edu_level<-fct_collapse(dat_joined$NR10, "None" = c("(00) none"),
                                   "Classes 1-5" = c("(01) 1st class","(02) 2nd class","(03) 3rd class",
                                                     "(04) 4th class", "(05) 5th class"),
                                   "Classes 6-10" = c("(06) 6th class","(07) 7th class","(08) 8th class",
                                                      "(09) 9th class","(10) Secondary"),
                                   "Classes 11-12" = c("(11) 11th Class","(12) High Secondary"),
                                   "1-2 years post-secondary"= c("(13) 1 year post-secondary",
                                                                  "(14) 2 years post-secondary"),
                                   "Bachelors"= "(15) Bachelors", "Above Bachelors"= "(16) Above Bachelors")



#making separate datasets for each state: same state/another state/abroad
dat_same_state <- dat_joined %>% filter(NR8 == "(1) same state")
dat_another_state <- dat_joined %>% filter(NR8 == "(2) another state")
dat_abroad <- dat_joined %>% filter(NR8 == "(3) abroad")

dim(dat_same_state)
dim(dat_another_state)
dim(dat_abroad)

v1<- ggplot(dat_same_state, aes(x="", y=INCOMEPC)) +
  geom_violin(fill="lightblue",
              trim = FALSE,
              draw_quantiles = c(0.25,0.5,0.75),
              alpha = 0.5,
              show.legend = FALSE)+
  xlab("")  + #x axis label
  ylab("Income per capita in Rupees")      +
  ggtitle("same state non-resident") + #add title to plot
  theme_bw()  #removes grey background

v2 <- ggplot(dat_another_state, aes(x="", y=INCOMEPC)) +
  geom_violin(fill="lightgreen",
              trim = FALSE,
              draw_quantiles = c(0.25,0.5,0.75),
              alpha = 0.5,
              show.legend = FALSE)+
  xlab("")  + #x axis label
  ylab("Income per capita in Rupees")      +
  ggtitle("another state non-resident") + #add title to plot
  theme_bw()  #removes grey background

v3 <- ggplot(dat_abroad, aes(x="", y=INCOMEPC)) +
  geom_violin(fill="orange",
              trim = FALSE,
              draw_quantiles = c(0.25,0.5,0.75),
              alpha = 0.5,
              show.legend = FALSE)+
  xlab("")  + #x axis label
  ylab("Income per capita in Rupees")      +
  ggtitle("abroad non-resident") + #add title to plot
  theme_bw()  #removes grey background

v1+v2+v3

#REMITTANCES/INCOME

v4<- ggplot(dat_same_state, aes(x="", y=NR13A*100/INCOME)) +
  geom_violin(fill="lightblue",
              trim = FALSE,
              draw_quantiles = c(0.25,0.5,0.75),
              alpha = 0.5,
              show.legend = FALSE)+
  xlab("")  + #x axis label
  ylab("Remittance received/Income")      +
  ggtitle("same state non-resident") + #add title to plot
  theme_bw()  #removes grey background

v5 <- ggplot(dat_another_state, aes(x="", y=NR13A*100/INCOME)) +
  geom_violin(fill="lightgreen",
              trim = FALSE,
              draw_quantiles = c(0.25,0.5,0.75),
              alpha = 0.5,
              show.legend = FALSE)+
  xlab("")  + #x axis label
  ylab("Remittance received/Income")      +
  ggtitle("another state non-resident") + #add title to plot
  theme_bw()  #removes grey background

v6 <- ggplot(dat_abroad, aes(x="", y=NR13A*100/INCOME)) +
  geom_violin(fill="orange",
              trim = FALSE,
              draw_quantiles = c(0.25,0.5,0.75),
              alpha = 0.5,
              show.legend = FALSE)+
  xlab("")  + #x axis label
  ylab("Remittance received/Income")      +
  ggtitle("abroad non-resident") + #add title to plot
  theme_bw()  #removes grey background
v4+v5+v6

#Oct 1, 2020
#LOOK AT income question - why is it negative -see which
#Is remittance part of income?

#gender, education level, household size, by state (using stateID), age, number of non-residents

#Plotting remittance received by gender for each state
g1<-ggplot(dat_same_state, aes(x=NR5, y=NR13A)) + 
  geom_violin(width = 0.50,fill="lightblue")+
  xlab("Gender of migrant")  + #x axis label
  ylab("Remittance received by household")    + #y axis label
  ggtitle("Remittances by Gender From Migrants in the Same State") + #add title to plot
  theme_bw()  #removes grey background 

g2<-ggplot(dat_another_state, aes(x=NR5, y=NR13A)) + 
  geom_violin(width = 0.50,fill="lightblue")+
  xlab("Gender of migrant")  + #x axis label
  ylab("Remittance received by household")    + #y axis label
  ggtitle("Remittances by Gender From Migrants in Another State") + #add title to plot
  theme_bw()  #removes grey background 

g3<-ggplot(dat_abroad, aes(x=NR5, y=NR13A)) + 
  geom_violin(width = 0.50,fill="lightblue")+
  xlab("Gender of migrant")  + #x axis label
  ylab("Remittance received by household")    + #y axis label
  ggtitle("Remittances by Gender From Migrants Abroad") + #add title to plot
  theme_bw()  #removes grey background 

g1/g2/g3
#Plotting remittance received by number of non-residents for each state
n1<-ggplot(dat_same_state, aes(x=NNR, y=NR13A)) + 
  geom_violin(width = 0.50,fill="lightblue")+
  xlab("#non-residents")  + #x axis label
  ylab("Remittance received by household")    + #y axis label
  ggtitle("Remittances by #Non-residents in the Same State") + #add title to plot
  theme_bw()  #removes grey background 

n2<-ggplot(dat_another_state, aes(x=NNR, y=NR13A)) + 
  geom_violin(width = 0.50,fill="lightblue")+
  xlab("#non-residents")  + #x axis label
  ylab("Remittance received by household")    + #y axis label
  ggtitle("Remittances by #Non-residents in Another State") + #add title to plot
  theme_bw()  #removes grey background 

n3<-ggplot(dat_abroad, aes(x=NNR, y=NR13A)) + 
  geom_violin(width = 0.50,fill="lightblue")+
  xlab("#non-residents")  + #x axis label
  ylab("Remittance received by household")    + #y axis label
  ggtitle("Remittances by #Non-residents Abroad") + #add title to plot
  theme_bw()  #removes grey background 

#Plotting remittance received by age category

options(scipen = 999) #to disable scientific notation
a1<-ggplot(dat_same_state, aes(x=Age_categ, y=NR13A)) + 
  geom_violin(width = 0.50,fill="lightblue",
              trim = FALSE,
              draw_quantiles = c(0.25,0.5,0.75),
              alpha = 0.5,show.legend = FALSE)+
  xlab("Ages")  + #x axis label
  ylab("Remittance received by hoh")    + #y axis label
  ggtitle("Remittances by Ages from Migrants In the Same State") + #add title to plot
  theme_bw()  #removes grey background 

a2<-ggplot(dat_another_state, aes(x=Age_categ, y=NR13A)) + 
  geom_violin(width = 0.50,fill="lightblue",
              trim = FALSE,
              draw_quantiles = c(0.25,0.5,0.75),
              alpha = 0.5,show.legend = FALSE)+
  xlab("Ages")  + #x axis label
  ylab("Remittance received by hh")    + #y axis label
  ggtitle("Remittances by Ages from Migrants In Another State") + #add title to plot
  theme_bw()  #removes grey background 

a3<-ggplot(dat_abroad, aes(x=Age_categ, y=NR13A)) + 
  geom_violin(width = 0.50,fill="lightblue",
              trim = FALSE,
              draw_quantiles = c(0.25,0.5,0.75),
              alpha = 0.5,show.legend = FALSE)+
  xlab("Ages")  + #x axis label
  ylab("Remittance received by hh")    + #y axis label
  ggtitle("Remittances by Ages from Migrants Abroad") + #add title to plot
  theme_bw()  #removes grey background 

a1/a2/a3

#Plotting remittance received by education level
e1<-ggplot(dat_same_state, aes(x=edu_level, y=NR13A)) + 
  geom_boxplot(width = 0.50,fill="lightblue")+
  xlab("Education level of Migrant")  + #x axis label
  ylab("Remittance received by hh")    + #y axis label
  ggtitle("Remittances by Education Level from Migrants in the Same State") + #add title to plot
  theme_bw()  #removes grey background 

e2<-ggplot(dat_another_state, aes(x=edu_level, y=NR13A)) + 
  geom_boxplot(width = 0.50,fill="lightblue")+
  xlab("Education level of Migrant")  + #x axis label
  ylab("Remittance received by hh")    + #y axis label
  ggtitle("Remittances by Education Level from Migrants in Another State") + #add title to plot
  theme_bw()  #removes grey background 

e3<-ggplot(dat_abroad, aes(x=edu_level, y=NR13A)) + 
  geom_boxplot(width = 0.50,fill="lightblue")+
  xlab("Education level of Migrant")  + #x axis label
  ylab("Remittance received by hh")    + #y axis label
  ggtitle("Remittances by Education Level from Migrants Abroad") + #add title to plot
  theme_bw()  #removes grey background 

# Oct 7, 20
#Plotting remittance/income by education level

#Plotting income by education level
ie1<-ggplot(dat_same_state, aes(x=edu_level, y=INCOMEPC)) + 
  geom_boxplot(width = 0.50,fill="lightblue")+
  xlab("Education level of Migrant")  + #x axis label
  ylab("Per Capita Income of hh")    + #y axis label
  ggtitle("Per Capita Income by Education Level") + #add title to plot
  theme_bw()  #removes grey background 

ie2<-ggplot(dat_another_state, aes(x=edu_level, y=INCOMEPC)) + 
  geom_boxplot(width = 0.50,fill="lightblue")+
  xlab("Education level of Migrant")  + #x axis label
  ylab("Per Capita Income of hh")    + #y axis label
  ggtitle("Per Capita Income by Education Level") + #add title to plot
  theme_bw()  #removes grey background 

ie3<-ggplot(dat_abroad, aes(x=edu_level, y=INCOMEPC)) + 
  geom_boxplot(width = 0.50,fill="lightblue")+
  xlab("Education level of Migrant")  + #x axis label
  ylab("Per Capita Income of hh")    + #y axis label
  ggtitle("Per Capita Income by Education Level") + #add title to plot
  theme_bw()  #removes grey background 

e1
e2
e3
e1/e2/e3

#Plotting remittance received by household size and state

h1<-ggplot(dat_same_state, aes(x=NPERSONS, y=NR13A)) + 
  geom_boxplot(width = 0.50,fill="lightblue")+
  xlab("Education level of Migrant")  + #x axis label
  ylab("Remittance received by hh")    + #y axis label
  ggtitle("Remittances by Education Level from Migrants in the Same State") + #add title to plot
  theme_bw()  #removes grey background 

h2<-ggplot(dat_another_state, aes(x=NPERSONS, y=NR13A)) + 
  geom_boxplot(width = 0.50,fill="lightblue")+
  xlab("Education level of Migrant")  + #x axis label
  ylab("Remittance received by hh")    + #y axis label
  ggtitle("Remittances by Education Level from Migrants in Another State") + #add title to plot
  theme_bw()  #removes grey background 

h3<-ggplot(dat_abroad, aes(x=NPERSONS, y=NR13A)) + 
  geom_boxplot(width = 0.50,fill="lightblue")+
  xlab("Education level of Migrant")  + #x axis label
  ylab("Remittance received by hh")    + #y axis label
  ggtitle("Remittances by Education Level from Migrants Abroad") + #add title to plot
  theme_bw()  #removes grey background 

h3





