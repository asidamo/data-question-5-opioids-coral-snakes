
rm(list=ls())
memory.size(max=F)
setwd("C:\\Users\\fevty\\Desktop\\NSS\\data-question-5-opioids-coral-snakes")
library(tidyverse)
library(ggplot2)
library(tidyr)
library(magrittr)
library(dplyr)
library(broom)
library(rmarkdown)
library(knitr)
#library(gdata)

# import Opioids  prescription data
opioids_df<-read_csv('opioids.csv')

tail(opioids_df)

# import overdose deaths data

overdose_df<-read_csv('overdoses.csv')
head(overdose_df,10)

# Explore overdoses datasets
# sort overdoses data frame by number of deaths

head(overdose_df,2)
# sort overdose data in descending order
overdose_df_SORTED<-overdose_df[order(overdose_df$Deaths,decreasing=TRUE),]

head(overdose_df_SORTED,5)

# bar plot number of overdoses related deaths by states
  ggplot(data=overdose_df_SORTED, aes(x=State, y=Deaths,fill='red')) +
  geom_bar(stat="identity") +
  guides(fill=FALSE,color=FALSE) +
  coord_flip()
# to plot the bar plot horizontally  coord_flip to plot verticall bars just run deaths_by_state


# map showing increase in overdose related deaths by States
# https://www.cdc.gov/drugoverdose/data/prescribing.html
# https://www.cdc.gov/media/releases/2018/p0329-drug-overdose-deaths.html 

#  read prescribers informations

prescribers<-read.csv('prescriber-info.csv',stringsAsFactors = FALSE)

head(prescribers,5)

# change the shape of the

rx_per_prescriber <- gather(prescribers, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)
head(rx_per_prescriber,5)

# drugs identified as matching in Opioids prescription data and prescribers information data.

matches <- c("ACETAMINOPHEN.CODEINE", "FENTANYL", "HYDROMORPHONE.HCL", 'HYDROCODONE.ACETAMINOPHEN',"METHADONE.HCL", "MORPHINE.SULFATE", "MORPHINE.SULFATE.ER", "OXYCODONE.HCL", "OXYCODONE.ACETAMINOPHEN",
             "OXYCONTIN", "TRAMADOL.HCL")

# create logical variable determines if RX by the NPIs are Opioids or not
opioids_prescribed <- rx_per_prescriber$rx %in% matches

# filter only NPI records where the prescribed drugs were opioids

rx_opioids <- rx_per_prescriber[opioids_prescribed == TRUE,]

head(rx_opioids)
# keep only qualified prescribers and countrx column not missing

rx_opioids<- filter(rx_opioids,Opioid.Prescriber==1,!(is.na(countrx)))

head(rx_opioids)



  # question # 2 test pareto effect (20% of top prescribers are prescribing 80% of prescriptions)

# let's sort rx_opiodis dataset by countrx (count of presscriptions)
rx_opioids<-rx_opioids[order(rx_opioids$countrx,decreasing=TRUE),]
head(rx_opioids)

# let's find total opiodis prescription in by using 
tot_prescription<-sum(rx_opioids$countrx,na.rm = TRUE)

print(tot_prescription)
# calculate opioids precentage per drug per NPI 
rx_opioids$pctPrescribed<- (rx_opioids$countrx/tot_prescription)*100


head(rx_opioids)



# take only NPI, countrx and pctPrescribed columns

rx_opioids_per_NPI<-data.frame(rx_opioids$NPI,rx_opioids$countrx,rx_opioids$pctPrescribed)

# rename the columns as "NPI","countrx","pctPrescribed"
colnames(rx_opioids_per_NPI)<-c('NPI','countrx',"pctPrescribed")

# uncomment the next line to compute for actual prescribers only.
#rx_opioids_per_NPI<-  filter(rx_opioids_per_NPI,countrx!=0)
# group by NPI 
prescription_per_NPI<-rx_opioids_per_NPI%>% 
group_by(NPI)%>%
summarise(countrx=sum(countrx),pctPrescribed=sum(pctPrescribed))

head(prescription_per_NPI)

# this should be 100%
sum(prescription_per_NPI$pctPrescribed)


# sort by descending order by percentage of opioids prescribed per NPI

prescription_per_NPI<-prescription_per_NPI[order(prescription_per_NPI$pctPrescribed,decreasing=TRUE),]

# create cummulative sum of percentages prescribed by prescribers
head(prescription_per_NPI)
opioids_per_NPI<-prescription_per_NPI%>%
  mutate(cummulativePct=cumsum(pctPrescribed))

head(opioids_per_NPI)
# select 80% of prescriptions
prescription_80_pct<-opioids_per_NPI[opioids_per_NPI$cummulativePct<=80,]

# calculate count of top 80 percent prescribers
count_top_80_pct<-nrow(prescription_80_pct)

# how many prescribers are prescribing 80 percent of the prescription?
print(count_top_80_pct)

# calculate count of bottom 20 percent prescribers
bottom_20_pct<-opioids_per_NPI[opioids_per_NPI$cummulativePct>80,]
count_bottom_20_pct<-nrow(bottom_20_pct)
# How many of the prescribers share only 20 percent of total prescription
print(count_bottom_20_pct)

# Do 20 percent of prescribers prescribe 80 percent of the total prescription?

pareto_effect<-count_top_80_pct*100/(count_bottom_20_pct+count_top_80_pct)

print(pareto_effect)

# let's divde the data into 20 % cummulative percentage ranges

opioids_ranges<-opioids_per_NPI%>%
  transmute(first_20_pct=cummulative_pct<20,second_20_pct=(cummulative_pct>=20 & cummulative_pct<40),third_20_pct=(cummulative_pct>=40 & cummulative_pct<60),
            fourth_20_pct=(cummulative_pct>=60 & cummulative_pct<80),fivth_20_pct=(cummulative_pct>=80))
# look what the data looks

head(opioids_ranges)

# look opioids_per_NPI

head(opioids_per_NPI)

#  group based on cummulative percentage of count of prescriptions by NPI and get count for ranges
first_20_pct<-nrow(opioids_per_NPI[opioids_per_NPI$cummulativePct<20,])
second_20_pct<-nrow(opioids_per_NPI[opioids_per_NPI$cummulativePct>=20 & opioids_per_NPI$cummulativePct<40,])
third_20_pct<-nrow(opioids_per_NPI[opioids_per_NPI$cummulativePct>=40 & opioids_per_NPI$cummulativePct<60,])
fourth_20_pct<-nrow(opioids_per_NPI[opioids_per_NPI$cummulativePct>=60 & opioids_per_NPI$cummulativePct<80,])
fifth_20_pct<-nrow(opioids_per_NPI[opioids_per_NPI$cummulativePct>=80,])




# rename columns as range

pareto_df_1<-data.frame(first_20_pct,second_20_pct,third_20_pct,fourth_20_pct,fifth_20_pct)

head(pareto_df_1)
# renaming the columns here help to plot the ranges in order from first 20% to fifth 20% of prescribers.

colnames(pareto_df_1)<-c('share1','share2','share3','share4','share5')

# calculate share of prescriptions by ranges
pareto_df_1<-pareto_df_1%>%
  mutate(share1=round(fifth_20_pct/nrow(opioids_per_NPI),3),share2=round(fourth_20_pct/nrow(opioids_per_NPI),3),
         share3=round(third_20_pct/nrow(opioids_per_NPI),3),share4=round(second_20_pct/nrow(opioids_per_NPI),3),
         share5=round(first_20_pct/nrow(opioids_per_NPI),3))
# look the data

head(pareto_df_1)
pareto_df_1<-pareto_df_1*100

# gather the data from wide to long
pareto_df_1<-gather(pareto_df_1)

head(pareto_df_1)

# rename column names
colnames(pareto_df_1)<-c('cummulativePctShareRx','pctOfPrescribers')

# plot the pareto_df_1 dataset 
# 80 % of the prescribers share only 20% of the prescription

ggplot(pareto_df_1, aes(cummulativePctShareRx, pctOfPrescribers)) +   
  geom_bar(aes(fill = pctOfPrescribers), position = "dodge", stat="identity") +
  guides(fill=FALSE,color=FALSE) +
  ggtitle('Opioids Prescribers by cummulative \n Share of Prescriptions')+
  theme(plot.title = element_text(hjust = 0.5))


# total number of prescribers

nrow(opioids_per_NPI)

# count of bottom 20 pct and top 80 pct prescribers

pareto_df<-data.frame(count_bottom_20_pct,count_top_80_pct)

head(pareto_df)

# calculate percentage share of  bottom 80 pct and top 20 pct prescribers out of total prescription
pareto_df2<-pareto_df%>%
  transmute(bottom_80_pct=round(count_top_80_pct/(count_top_80_pct+count_bottom_20_pct),3),top_20_pct=round(count_bottom_20_pct/(count_top_80_pct+count_bottom_20_pct),3))

# have a look at the data
head(pareto_df2)

pareto<-gather(pareto_df2)
# rename columns
colnames(pareto)<-c('ranges','pctPrescribed')
head(pareto)
  
# plot percentage of prescriptions shared by bottom 80 pct and top 20 pct prescribers

ggplot(pareto, aes(ranges, pctPrescribed)) +   
  geom_bar(aes(fill = pctPrescribed), position = "dodge", stat="identity") +
    guides(fill=FALSE,color=FALSE) +
     ggtitle('Pareto Effect for Opioids prescribers \n top_20 vs bottom_80 percent')+
      theme(plot.title = element_text(hjust = 0.5))


# Only 20.26% of the qualified prescribers have prescribed 80% of Opioids prescriptions
# when only actuall prescribers are considered 31% of prescribers prescribed 80% of the Opioids

# plot histogram 
ggplot(
 data = opioids_per_NPI,
 aes(x = cummulativePct,fill='orange2')
) +
  geom_histogram(binwidth =20)+
 guides(fill=FALSE,color=FALSE) 
  
  
# rename columns
head(prescription_per_NPI)
colnames(prescription_per_NPI)<-c('countRxPerNPI','cummulativePct')

# plot cummulative  distribution

head(opioids_per_NPI)
plot(opioids_per_NPI$cummulativePct, xlab = 'totCountRx', ylab = 'cummulativePct', main = 'Cumluative Distribution\nOpioids Prescriptions')



# Question # 3  Build a model to predict the number of fatal opioid overdoses
# try to create a model for a specific state

# data for diabetic diseases by states
# https://gis.cdc.gov/grasp/diabetes/DiabetesAtlas.html

diabetes<- read_csv('DiabetesAtlasData.csv',skip =2)

# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk 
# import population by race data, this data takes only non hispanic white.

race<-read.csv('PEP_2014_PEPSR6H_with_ann.csv')

tail(race,15)

# https://www.census.gov/library/publications/2015/demo/p60-253.html 

# import pecentage of uninsured population by states

insurance<-read.csv('hic04_acs.csv')

# total insured row is not needed, we wilter it out.
insurance <-insurance%>%
  filter(coverage!='Total')
  

# https://www.kff.org/other/state-indicator/health-spending-per-capita/?currentTimeframe=0&selectedRows=%7B%22states%22:%7B%22all%22:%7B%7D%7D,%22wrapups%22:%7B%22united-states%22:%7B%7D%7D%7D&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

# import percapita health expenditure by states 

percapita_HE<-read_csv('raw_data.csv',skip =2)


head(percapita_HE)
# keep only by states data, we don't need USA total.
percapita_HE<-percapita_HE%>%
  filter(state!='United States')

# let's merge supplementary data with prescription data
# check headers of the datasets 
head(diabetes,2)
head(rx_opioids,2)
head(race,20)
head(insurance,10)
head(percapita_HE,2)

# clean diabetes data
head(diabetes)
diabetes<-data.frame(diabetes$State,diabetes$Percentage)
colnames(diabetes)<-c('state','diabetes_pct')
# capitalize the satates
diabetes<-diabetes%>%
  filter(state!='Median of States')%>%
  mutate(state=toupper(state))


# clean race data, let's keep 2014 estimate 

race<-race%>%
  filter(Year.id=='est72014',Sex.id=='totsex',GEO.display.label!="United States")
race_df<-data.frame(race$GEO.display.label,race$Hisp.id,race$totpop,race$wa,race$ba,race$ia,race$aa,race$na,race$tom)

head(race_df)
# keep only non_hispanic race
race_df<-race_df%>%
  filter(race.Hisp.id=='nhisp')

head(race_df)
# rename columns
colnames(race_df)<-c('state','nonhisp','totpop','wa','ba','ia','aa','na','tom')
race_df<-race_df<-race_df%>%
  mutate(state=toupper(state),wa=round(wa/totpop*100,2),ba=round(ba/totpop*100,2),ia=round(ia/totpop*100,2),aa=round(aa/totpop*100,2),na=round(na/totpop*100,2),tom=round(tom/totpop*100,2))
head(race_df)


# rename columns for percentage of insurance coveragd and unisured population by states

colnames(insurance)<-c('state','Coverage_type','uninsured2016','uninsuredPct2014')
# let's keep only uninsured from insurance dataset
uninsured<-insurance%>%
  filter(Coverage_type=='Uninsured')

# rename columns for per_capita health expenditure
colnames(percapita_HE)<-c('state','perCapitaHealthExp')

# states in opiods_prescription dataset are in abbreviation, we cannot match with other data sets
#So we need to create state name and state abbriviation data

state_df<-data.frame(state.name,state.abb)
colnames(state_df)<-c('state_name','State')
# state_df doesn't include district of columbia.
# create state name and abbreviation for DC.
DC_state<-data.frame(state_name="District of Columbia",State="DC")
# bind it with state_df
state_df<-rbind.data.frame(state_df,DC_state)


# to check the header of rx_opioids dataset
head(rx_opioids)

# join state names  data with prescription data
rx_opioids_df<-rx_opioids%>%
   left_join(state_df,by='State')
  
head(rx_opioids_df)
# rename column names

colnames(rx_opioids_df)[colnames(rx_opioids_df)=='State']<-'State_code'
colnames(rx_opioids_df)[colnames(rx_opioids_df)=='state_name']<-'state'

head(rx_opioids_nNPI)

# to check if DC has full state name in the dataset
rx_opioids_df%>%
  filter(State_code=='DC')


# capitalize states to match with other data sets
rx_opioids_df<-rx_opioids_df%>%
  mutate(state=toupper(state))


head(rx_opioids_df)
# take selected columns
rx_opioids_df<-data.frame(rx_opioids_df$state,rx_opioids_df$NPI,rx_opioids_df$countrx,rx_opioids_df$Opioid.Prescriber)
# group rx_opioids_df by state
prescription_by_state<-rx_opioids_df%>% 
  group_by(rx_opioids_df.state)%>%
  summarize(countrx=sum(rx_opioids_df.countrx),total_prescribers=sum(rx_opioids_df.Opioid.Prescriber))
  
# rename columns

colnames(prescription_by_state)<-c('state','countrx','total_prescribers')

head(prescription_by_state)

# merge overdose data with prescription data


colnames(overdose_df)<-c('state','population','deaths','abbrev')

head(overdose_df)

overdose_df<-overdose_df%>%
  mutate(state=toupper(state))

head(overdose_df)
# merge overdose data with opioids rx
merged_df1<- overdose_df %>%
  left_join(prescription_by_state,by='state')


head(uninsured)

# join health percentage of uninsured population by states
merged_df2<-merged_df1 %>%
  left_join(uninsured,by='state')

head(merged_df2)

# capitalize state names

percapita_HE<-percapita_HE%>%
  mutate(state=toupper(state))

# join per capita health expenditure 
merged_df3<-merged_df2 %>%
  left_join(percapita_HE,by='state')

head(merged_df3)

# merge diabets by states data to merged data
merged_df4<-merged_df3%>%
              left_join(diabetes,by='state')
     head(merged_df4)       

     
# merge race_df to the merged data

merged_df5<-merged_df4%>%
  left_join(race_df,by='state')

head(merged_df5)


head(merged_df5)

# normalize deaths and count of prescription by 100 people
merged_df5<-merged_df5%>%
  mutate(deaths=deaths/100,countrx=countrx/100)

# import unemployment data

unemployment<-read_csv('emp_unemployment.csv')

# merge unemployment rate data 
merged_df6<-inner_join(merged_df5,unemployment,by='state')
head(merged_df6)


# import age data
age_df<-read_csv('age_data.csv')
head(age_df)

merged_df7<-left_join(merged_df6,age_df,by='state')
head(merged_df7)

# import median household income
median_hsld_income<-read_csv('Median Household Income by State.csv')
head(median_hsld_income)

# merge  median income data
merged_df7<-left_join(merged_df7,median_hsld_income,by='state')

# import education data

education_df<-read_csv('education_highscool_or_more.csv')
head(education_df)

# Merge education data 
merged_df7<-left_join(merged_df7,education_df,by='state')
head(uninsured)

# create data frame for only variables that will be used in the model
modelVariables<-data.frame(merged_df7$deaths,merged_df7$countrx, merged_df7$medianAge,merged_df7$unemployment,merged_df7$diabetes_pct,merged_df7$uninsuredPct2014,merged_df7$perCapitaHealthExp,merged_df7$Education,merged_df7$Income,merged_df7$wa,merged_df7$ba)

colnames(modelVariables)<-c('deaths','countrx','medianAge','unemp','diabetes','uninsured','HealthExp','Educ','Income','wa','ba')

# calculate correlation among the model variables
# race variables have opposite sign in the regression compared with correlation coefficient.
round(cor(modelVariables),2)

# linear model to estimate the overdose deaths
linearModel <-lm(formula = deaths~countrx + medianAge + unemployment + 
                   uninsuredPct2014+perCapitaHealthExp+ +uninsuredPct2014+Income+Education +wa + ba, data =merged_df7 )

# linear model by removing three weakly correlated variables

linearModel1 <-lm(formula = deaths~countrx + Education + unemployment + 
                   uninsuredPct2014 + +uninsuredPct2014 + wa + ba, data =merged_df7 )
tidy(linearModel1)
glance(linearModel1)
# model without race variables and  with No Education
linearModel3 <-lm(formula = deaths~countrx + medianAge + unemployment + 
                     uninsuredPct2014+perCapitaHealthExp+ +uninsuredPct2014+Income, data =merged_df7 )
tidy(linearModel3)
glance(linearModel3)

# model with only six variables with unemployment rate
linearModel4 <-lm(formula = deaths~countrx + medianAge + unemployment + 
                    uninsuredPct2014, data =merged_df7 )
tidy(linearModel4)
glance(linearModel4)

# model with only six variables with education 
linearModel5 <-lm(formula = deaths~countrx + medianAge + Education + 
                    uninsuredPct2014, data =merged_df7 )
tidy(linearModel5)
glance(linearModel5)


# model with fewer variables

linearMode2 <-lm(formula = deaths~countrx + unemployment + 
                   uninsuredPct2014+perCapitaHealthExp+ +uninsuredPct2014+Education++wa+ba, data =merged_df7 )

tidy(linearMode2)
round(glance(linearMode2),3)

# adding percentage of diabetic people into the model improves R_square
# it has positive effect ,but not statistically significant.
#linearModel_dbts <-lm(formula = deaths~countrx + diabetes_pct+ tot_median_age + unemployment + uninsured_2014+HE_per_capita+ Education+Income+wa+ba, data =merged_df7 )

#tidy(linearModel_dbts)
#glance(linearModel_dbts)

# check the coefficients and fitness of the model
tidy(linearModel)
glance(linearModel)


glance<-glance(linearModel)

# plot the model 
plot(linearModel)


augmented_df<-augment(linearModel)


augmented_df%>%
  ggplot(aes(x=medianAge))+
  geom_point(aes(y=deaths))+
  geom_line(aes(y=.fitted,color='red')) + 
  guides(fill=FALSE,color=FALSE) + 
  ggtitle("relationship between overdose deaths\n  and median_age by states")
theme(plot.title = element_text(hjust = 0.5))


augmented_df%>%
  ggplot(aes(x=perCapitaHealthExp))+
  geom_point(aes(y=deaths))+
  geom_line(aes(y=.fitted,color='red')) + 
  guides(fill=FALSE,color=FALSE) + 
  ggtitle("overdose deaths and health expenditure \n by States")
theme(plot.title = element_text(hjust = 0.5))

augmented_df%>%
  ggplot(aes(x=uninsuredPct2014))+
  geom_point(aes(y=deaths))+
  geom_line(aes(y=.fitted,color='red')) + 
  guides(fill=FALSE,color=FALSE) + 
  ggtitle("overdose deaths and percentage of uninsured")
theme(plot.title = element_text(hjust = 0.5))

augmented_df%>%
  ggplot(aes(x=unemployment))+
  geom_point(aes(y=deaths))+
  geom_line(aes(y=.fitted,color='red')) + 
  guides(fill=FALSE,color=FALSE) + 
  ggtitle("overdose deaths and unemployment rate")
theme(plot.title = element_text(hjust = 0.5))

augmented_df%>%
  ggplot(aes(x=Education))+
  geom_point(aes(y=deaths))+
  geom_line(aes(y=.fitted,color='red')) + 
  guides(fill=FALSE,color=FALSE) + ggtitle("overdose deaths and percentage of \n Highschool and above by states")
theme(plot.title = element_text(hjust = 0.5))

augmented_df%>%
  ggplot(aes(x=Income))+
  geom_point(aes(y=deaths))+ 
    geom_line(aes(y=.fitted,color='red')) + 
    guides(fill=FALSE,color=FALSE) + ggtitle("overdose deaths and median household income by states")
