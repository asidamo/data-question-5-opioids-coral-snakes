
rm(list=ls())
memory.size(max=F)
setwd("C:\\Users\\fevty\\Desktop\\NSS\\data-question-5-opioids-coral-snakes")
library(tidyverse)
library(ggplot2)
library(tidyr)
library(magrittr)
library(dplyr)
library(broom)
#library(gdata)


# questions
# opiodis data 
# overdose data state level informations
# prescribers data includes both opiodis and other drugs.
# 1. explore the data, plot by states
# 2. states with highest number of overdoses and opioids prescribers
# 3. plot box, historgrams or any appropriate plot
# 4. pareto effect of opiodis
# 5. build model to to predict number of opiodis fatal death
# 6. find other data that may affect health outcomes
# 7. variability in numberof opiodis and mean , median
# prescribers data
# NPI is key indicator in your data
# find number of prescriptions they provided.


opioids_df<-read_csv('opioids.csv')

tail(opioids_df)

overdose_df<-read_csv('overdoses.csv')
head(overdose_df,10)



# Explore overdoses datasets

# sort overdoses data frame by number of deaths


head(overdose_df,2)

overdose_df_SORTED<-overdose_df[order(overdose_df$Deaths,decreasing=TRUE),]


head(overdose_df_SORTED,5)

# bar plot number of overdoses related deaths by states
deaths_by_state<-ggplot(data=overdose_df_SORTED, aes(x=State, y=Deaths,fill='red')) +
  geom_bar(stat="identity")
# to plot the bar plot horizontally  coord_flip to plot verticall bars just run deaths_by_state
deaths_by_state + coord_flip()

# map showing increase in overdose related deaths by States
# https://www.cdc.gov/drugoverdose/data/prescribing.html
# https://www.cdc.gov/media/releases/2018/p0329-drug-overdose-deaths.html 



#  read prescribers informations

prescribers<-read_csv('prescriber-info.csv')


head(prescribers,5)

# change the shape of the

rx_per_prescriber <- gather(prescribers, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)
head(rx_per_prescriber,5)


matches <- c("ACETAMINOPHEN.CODEINE", "FENTANYL", "HYDROMORPHONE.HCL", "METHADONE.HCL", "MORPHINE.SULFATE", "MORPHINE.SULFATE.ER", "OXYCODONE.HCL", "OXYCODONE.ACETAMINOPHEN",
             "OXYCONTIN", "TRAMADOL.HCL")

opioids_prescribed <- rx_per_prescriber$rx %in% matches
rx_opioids <- rx_per_prescriber[opioids_prescribed == TRUE,]

head(rx_opioids)
# keep only qualified prescribers and countrx column not missing

rx_opioids<- filter(rx_opioids,Opioid.Prescriber==1,!(is.na(countrx)))
head(rx_opioids)


rx_opioids$NPI
  # question # 2 test pareto effect (20% of top prescribers are prescribing 80% of prescriptions)

# let's sort rx_opiodis dataset by countrx (count of presscriptions)
rx_opioids<-rx_opioids[order(rx_opioids$countrx,decreasing=TRUE),]
head(rx_opioids)

# let's find total opiodis prescription in by using 
tot_prescription<-sum(rx_opioids$countrx,na.rm = TRUE)

print(tot_prescription)
# calculate opioids precentage per drug per NPI 
rx_opioids$pct_prescribed<- (rx_opioids$countrx/tot_prescription)*100

head(rx_opioids)

# take only NPI, countrx and pct_prescribed columns

rx_opioids_per_NPI<-data.frame(rx_opioids$NPI,rx_opioids$countrx,rx_opioids$pct_prescribed)

head(rx_opioids_per_NPI)

# rename the columns as "NPI","countrx","pct_prescribed"
colnames(rx_opioids_per_NPI)<-c("NPI","countrx","pct_prescribed")

# uncomment the next line to compute for actual prescribers only.
#rx_opioids_per_NPI<-  filter(rx_opioids_per_NPI,countrx!=0)


# group by NPI 
prescription_per_NPI<-rx_opioids_per_NPI%>% 
group_by(NPI)%>%
summarise(countrx=sum(countrx),pct_prescribed=sum(pct_prescribed))

head(prescription_per_NPI)


sum(prescription_per_NPI$pct_prescribed)


# sort by descending order by percentage of opioids prescribed per NPI

prescription_per_NPI<-prescription_per_NPI[order(prescription_per_NPI$pct_prescribed,decreasing=TRUE),]

# create cummulative sum of percentages prescribed by prescribers
head(prescription_per_NPI)
prescription_per_NPI<-prescription_per_NPI%>%
  mutate(cummulative_pct=cumsum(pct_prescribed))

head(prescription_per_NPI)
# select 80% of prescriptions
prescription_80_pct<-prescription_per_NPI[prescription_per_NPI$cummulative_pct<=80,]

# calculate count of top 80 percent prescribers
count_top_80_pct<-nrow(prescription_80_pct)
count_top_80_pct

# calculate count of bottom 20 percent prescribers
bottom_20_pct<-prescription_per_NPI[prescription_per_NPI$cummulative_pct>80,]
count_bottom_20_pct<-nrow(bottom_20_pct)
count_bottom_20_pct

pareto_effect<-count_top_80_pct*100/(count_bottom_20_pct+count_top_80_pct)

# Only 20.26% of the qualified prescribers have prescribed 80% of Opioids prescriptions
# when only actuall prescribers are considered 30.75% of prescribers prescribed 80% of the Opioids
pareto_effect

# plot histogram 
ggplot(
  data = prescription_per_NPI,
  aes(x = cummulative_pct,fill='orange2')
) +
  geom_histogram(binwidth =20)

#prescription_per_NPI<-prescription_per_NPI %>%
 # mutate(cum_cdf=ecdf(pct_prescribed))

head(prescription_per_NPI)
plot(prescription_per_NPI$cummulative_pct, xlab = 'count_of_prescriptions', ylab = 'cummulative_percent', main = 'Empirical Cumluative Distribution\nOpioids Prescriptions')



# Question # 3 module for prescription practices 

# data for diabetic diseases by states
# https://gis.cdc.gov/grasp/diabetes/DiabetesAtlas.html

diabetes<- read_csv('DiabetesAtlasData.csv',skip =2)

str(diabetes)
diabetes_by_state<-ggplot(data=diabetes, aes(x=State, y=Percentage,fill='red')) + geom_bar(stat="identity")

diabetes_by_state + coord_flip()

tail(diabetes)

# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk 
race<-read.csv('PEP_2014_PEPSR6H_with_ann.csv')

tail(race,15)

# https://www.census.gov/library/publications/2015/demo/p60-253.html 

insurance<-read.csv('hic04_acs.csv')

# total column is not needed
insurance <-insurance%>%
  filter(coverage!='Total')
  


# https://www.kff.org/other/state-indicator/health-spending-per-capita/?currentTimeframe=0&selectedRows=%7B%22states%22:%7B%22all%22:%7B%7D%7D,%22wrapups%22:%7B%22united-states%22:%7B%7D%7D%7D&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

percapita_HE<-read_csv('raw_data.csv',skip =2)

head(percapita_HE)
percapita_HE<-percapita_HE%>%
  filter(state!='United States')

# let's merge supplementary data with prescription data

head(diabetes,2)
head(rx_opioids,2)
head(race,20)
head(insurance,10)
head(percapita_HE,2)
# rename columns for per_capita health expenditure
colnames(percapita_HE)<-c('State','Coverage_type','2016_pct','2014_pct')
# let's keep only uninsured from insurance dataset
insurance<-insurance%>%
  filter(coverage=='Uninsured')

# let's keep 2014 estimate 

race<-race%>%
  filter(Year.id=='est72014',Sex.id=='totsex',GEO.display.label!="United States")
race_df<-data.frame(race$GEO.display.label,race$Hisp.id,race$totpop,race$wa,race$ba,race$ia,race$aa,race$na,race$tom)

head(race_df)

race_df<-race_df%>%
  filter(race.Hisp.id=='nhisp')

head(race_df)

colnames(race_df)<-c('state','nonhisp','totpop','wa','ba','ia','aa','na','tom')
race_df<-race_df<-race_df%>%
  mutate(state=toupper(state),wa=round(wa/totpop*100,2),ba=round(ba/totpop*100,2),ia=round(ia/totpop*100,2),aa=round(aa/totpop*100,2),na=round(na/totpop*100,2),tom=round(tom/totpop*100,2))
head(race_df)
# create state name and state abbriviation data

state_df<-data.frame(state.name,state.abb)
colnames(state_df)<-c('state_name','State')
DC_state<-data.frame(state_name="District of Columbia",State="DC")
state_df<-rbind.data.frame(state_df,DC_state)


head(rx_opioids)



# join state names  data with prescription data
rx_opioids_df<-rx_opioids%>%
   left_join(state_df,by='State')
  
head(rx_opioids_df)
# rename column names

colnames(rx_opioids_df)[colnames(rx_opioids_df)=='State']<-'State_code'
colnames(rx_opioids_df)[colnames(rx_opioids_df)=='state_name']<-'state'



rx_opioids_gender<-rx_opioids_df%>%
  group_by(Gender)%>%
  summarize(Sum_CountRx=sum(countrx))

ggplot(rx_opioids_gender,aes(x=Gender)) + geom_bar()


head(rx_opioids_nNPI)

# to check if DC has full state name in the dataset
rx_opioids_df%>%
  filter(State_code=='DC')

head(insurance)

# join opioids prescription data with  insurance data
rx_opioids_df<-rx_opioids_df%>%
  mutate(state=toupper(state))


head(rx_opioids_df)
# take only NPI and countrx column
rx_opioids_df<-data.frame(rx_opioids_df$state,rx_opioids_df$NPI,rx_opioids_df$countrx)
# group rx_opioids_df by state
prescription_by_state<-rx_opioids_df%>% 
  group_by(rx_opioids_df.state)%>%
  summarize(countrx=sum(rx_opioids_df.countrx))
  # rename columns

colnames(prescription_by_state)<-c('state','countrx')


# merge overdose data with prescription data


colnames(overdose_df)<-c('state','population','deaths','abbrev')

head(overdose_df)

overdose_df<-overdose_df%>%
  mutate(state=toupper(state))

head(overdose_df)
# merge overdose data with opioids rx
merged_df1<- overdose_df %>%
  left_join(prescription_by_state,by='state')


# join health insurance by states
merged_df2<-merged_df1 %>%
  left_join(insurance,by='state')
head(insurance)

head(merged_df2)


# join opioids prescription data with  percapita HE data

percapita_HE<-percapita_HE%>%
  mutate(state=toupper(state))

merged_df3<-merged_df2 %>%
  left_join(percapita_HE,by='state')
head(merged_df3)

# merge diabets by states data to merged data
head(diabetes)
diabetes<-data.frame(diabetes$State,diabetes$Percentage)
colnames(diabetes)<-c('state','diabetes_pct')
# capitalize the satates
diabetes<-diabetes%>%
  filter(state!='Median of States')%>%
  mutate(state=toupper(state))

# merge diabetes
merged_df4<-merged_df3%>%
              left_join(diabetes,by='state')
     head(merged_df4)       
# merge race_df to the merged data

merged_df5<-merged_df4%>%
  left_join(race_df,by='state')

head(merged_df5)

# it looks states with more uninsured popualtion has likely have more Opiodis prescription.

ggplot(merged_df5,aes(x=countrx,y=deaths)) + geom_point()

# outlier function
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
#  customized outlier

is_outlier_fn <- function(x) {
  return(x < 5.00| x > 15)
}


ggplot(merged_df5,aes(x=HE_per_capita,y=deaths)) + geom_point()

ggplot(merged_df5,aes(x=diabetes_pct,y=deaths)) + geom_point() +scale_y_log10()

ggplot(merged_df5,aes(x=wa,y=deaths))+geom_point() +scale_y_log10()
ggplot(merged_df5,aes(x=ba,y=deaths))+geom_point() +scale_y_log10()

ggplot(merged_df5,aes(x=population,y=deaths)) + geom_point()


head(merged_df5)

# estimate a linear regression

# gap_models contains regression model for each country

linearModel <-lm(formula = deaths~countrx +population + X2014_pct+HE_per_capita+diabetes_pct+ wa+ba, data =merged_df5 )

tidy(linearModel)
glance(linearModel)
augmented_df<-augment(linearModel)

head(augmented_df)

augmented_df%>%
ggplot(aes(x=countrx))+geom_point(aes(y=deaths))+geom_line(aes(y=.fitted,color='red')) + 
guides(fill=FALSE,color=FALSE) + ggtitle("relationship between overdose deaths  and number of rx")

augmented_df%>%
  ggplot(aes(x=diabetes_pct))+geom_point(aes(y=deaths))+ scale_y_log10()+geom_line(aes(y=.fitted,color='red')) + 
  guides(fill=FALSE,color=FALSE) + ggtitle("overdose deaths and percentage of diabetic population")

augmented_df%>%
  ggplot(aes(x=HE_per_capita))+geom_point(aes(y=deaths))+geom_line(aes(y=.fitted,color='red')) + 
  guides(fill=FALSE,color=FALSE) + ggtitle("overdose deaths and percapita health expenditure")


augmented_df%>%
  ggplot(aes(x=X2014_pct))+geom_point(aes(y=deaths))+ scale_y_log10()+geom_line(aes(y=.fitted,color='red')) + 
  guides(fill=FALSE,color=FALSE) + ggtitle("overdose deaths and percentage of uninsured")

######################## model with only two explanatory variables: countrx and population ########### 
# linear mode 2
linearModel_2<-lm(formula = deaths~countrx +population, data =merged_df5)
tidy(linearModel_2)
glance(linearModel_2)



