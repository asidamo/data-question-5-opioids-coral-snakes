# Things to do:
# ~ summarize opioid prescribers by state
# ~ States in 95th percentile for prescriptions and deaths
# ~ Concentration of prescribers in states
# ~ Seems likely that a prescription is the result of a diagnosis. 
#   ~ Do we have data on diagnosis billing codes used in association with rxs?
# ~ Compare opioid data frame prescription rates to overall rates for all drugs
# ~ Compare overall rx rates by state.  Compare overall rx rates of opioid v non-opioid prescribers 
# ~ Compare proportions of opioid prescriptions between populations/states
# 
# ~ Overall prescriber behavior as count of annual Rxs and proportion of opioids
# ~ Population sample: all prescribers. 
# ~ Test hypothesis that there are differences in prescriber and opioid prescriber behavior
# ~ Analyze population covariances by specialty
#


library(tidyverse)
library(magrittr)
library(ggplot2)
library(beeswarm)
library(dplyr)

opioids_raw <- read_csv('data/opioids.csv')
overdoses_raw <- read_csv('data/overdoses.csv')
prescribers_raw <- read_csv('data/prescriber-info.csv')
prescribers_16_raw <- read_csv('prescriber-info_CY16.csv')

#non-states identified, dropping from raw data

nonstates <- c("PR", "ZZ", "AA", "AE", "GU","VI","DC")

nonstatematches <- prescribers_raw$State %in% nonstates
prescribers_raw <- prescribers_raw[nonstatematches == TRUE]
mutate(prescribers_raw, total_rx = sum(ACETAMINOPHEN.CODEINE:ZOLPIDEM.TARTRATE))
# Using gather to associate counts of prescriptions of individual drugs with providers.
all_rx_per_npi <- gather(prescribers_raw, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)
all_rx_per_npi_16 <- gather(prescribers_16_raw, rx, countrx, ACETAMINOPHEN.CODEINE:ZOLPIDEM.TARTRATE)
all_rx_op <- all_rx_per_npi[all_rx_per_npi$Opioid.Prescriber == 1,]
all_rx_non <- all_rx_per_npi[all_rx_per_npi$Opioid.Prescriber == 0,]
sum(all_rx_op$countrx)
summary(all_rx_per_npi)
# 11 drugs are identified as opioids from opioids.csv. Non-matching labels require manual 
# construction of a matching vector

# N.B. All drugs identified as present in 2014 dataset are also in 2016 dataset

# Missing opioids from prescriber data:
# Buprenorphine, Buprenorphine HCl, BUTALBIT/ACETAMIN/CAFF/CODEINE, BUTORPHANOL TARTRATE,
# CODEINE SULFATE, CODEINE/BUTALBITAL/ASA/CAFFEIN, CODEINE/CARISOPRODOL/ASPIRIN, 
# DHCODEINE BT/ACETAMINOPHN/CAFF, DIHYDROCODEINE/ASPIRIN/CAFFEIN, HYDROCODONE BITARTRATE, 
# HYDROCODONE/ACETAMINOPHEN, HYDROCODONE/IBUPROFEN, HYDROMORPHONE HCL/PF, IBUPROFEN/OXYCODONE HCL,
# LEVORPHANOL TARTRATE, MEPERIDINE HCL, MEPERIDINE HCL/PF, NALBUPHINE HCL,
# OPIUM TINCTURE, OPIUM/BELLADONNA ALKALOIDS, OXYCODONE HCL/ASPIRIN, PENTAZOCINE HCL/ACETAMINOPHEN, 
# PENTAZOCINE HCL/NALOXONE HCL, PENTAZOCINE LACTATE, TAPENTADOL HCL, TRAMADOL HCL/ACETAMINOPHEN



matches <- c("ACETAMINOPHEN.CODEINE", "FENTANYL", "HYDROCODONE.ACETAMINOPHEN", "HYDROMORPHONE.HCL", "METHADONE.HCL",
             "MORPHINE.SULFATE", "MORPHINE.SULFATE.ER", "OXYCODONE.HCL", "OXYCODONE.ACETAMINOPHEN",
             "OXYCONTIN", "TRAMADOL.HCL")

# Carisoprodol is omitted from analysis of opioids as single-agent formulation is not present in opioid listing.
matching_records <- all_rx_per_npi$rx %in% matches
matching_records_16 <- all_rx_per_npi_16$rx %in% matches


sum(prescribers_raw$Opioid.Prescriber)
# 14503 prescribers have value of Opioid Prescriber = 1, assumed to be True

# Slicing gathered dataset for only opioid drugs and Opioid prescribers. 
opioid_per_npi <- all_rx_per_npi[matching_records == TRUE,]
unique(opioid_per_npi$rx)
opioid_bynpi_hirx <- opioid_per_npi[opioid_per_npi$Opioid.Prescriber == 1,]
non_prescribers <- opioid_per_npi[opioid_per_npi$Opioid.Prescriber == 0,]

# Need to add mean, max, summaries/bar plots
View(count_of_opioids)
count_of_opioids <- opioid_bynpi_hirx %>% 
  group_by(rx) %>% 
  summarise(total = sum(countrx))
opioids_per_state <- 
  opioid_bynpi_hirx %>% 
  group_by(State) %>% 
  filter(State %in% c("MT", "RI", "NH"))

ggplot(opioids_per_state, aes(x = reorder(State, countrx), y = countrx)) +
  geom_jitter() +
#  labs(x = "Drug", y = "Total Prescriptions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(count_of_opioids, aes(x = reorder(rx, total), y = total)) +
       geom_col() +
      labs(x = "Drug", y = "Total Prescriptions") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

avg_all_opioids <- mean(count_of_opioids$total)
avg_all_opioids

avgrx <- sum(all_rx_per_npi$countrx) / 25000
avgrx
sdrx <- sd(all_rx_per_npi$countrx)
sdrx
unique(all_rx_op$NPI)
avgrx_op <- sum(all_rx_op$countrx) / 14503
avgrx_op

opcount <- sum(opioid_bynpi_hirx$countrx) / 14503

avgrx_non <- sum(all_rx_non$countrx) / 10497
avgrx_non
countperstate <- all_rx_non %>% 
  group_by(State) %>% 
  summarise(total = sum(countrx))
View(opioidprescriberperstate)
mean(countperstate$total)
ggplot(data = opioidprescriberperstate, aes(x = reorder(State, total), y = total)) +
         geom_col()

opioidprescriberperstate <- 
  prescribers_raw %>% 
  group_by(State) %>% 
  summarise(total = sum(Opioid.Prescriber))
View(opioidprescriberperstate)

mutate(overdoses_raw, NormDeaths, as.numeric(Deaths)/as.numeric(Population))

ggplot(overdoses_raw, aes(x = reorder(State, NormDeaths), y = NormDeaths)) +
  geom_col() +
  labs(x = "State", y = "Deaths Per Capita") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

