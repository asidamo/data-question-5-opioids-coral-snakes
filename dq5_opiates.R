# Opioids data - remove duplicates from Generic.Name - only field needed
# prescribers data - split drug data from prescriber data in order
#                    to rotate drug data

library(tidyverse)
library(magrittr)
library(ggplot2)
library(beeswarm)
library(dplyr)

opioids_raw <- read.csv('data/opioids.csv')
overdoses_raw <- read.csv('data/overdoses.csv')
prescribers_raw <- read.csv('data/prescriber-info.csv')

# Using gather to associate counts of prescriptions of individual drugs with providers.
rx_per_npi <- gather(prescribers, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)

# 10 drugs are identified as opioids from opioids.csv. Non-matching labels require manual 
# construction of a matching vector
matches <- c("ACETAMINOPHEN.CODEINE", "FENTANYL", "HYDROMORPHONE.HCL", "METHADONE.HCL",
             "MORPHINE.SULFATE", "MORPHINE.SULFATE.ER", "OXYCODONE.HCL", "OXYCODONE.ACETAMINOPHEN",
             "OXYCONTIN", "TRAMADOL.HCL")

matching_records <- rx_per_npi$rx %in% matches

# Missing opioids from prescriber data:
# Buprenorphine, Buprenorphine HCl, BUTALBIT/ACETAMIN/CAFF/CODEINE, BUTORPHANOL TARTRATE
# CODEINE SULFATE, CODEINE/BUTALBITAL/ASA/CAFFEIN, CODEINE/CARISOPRODOL/ASPIRIN, DHCODEINE BT/ACETAMINOPHN/CAFF
# DIHYDROCODEINE/ASPIRIN/CAFFEIN, HYDROCODONE BITARTRATE, HYDROCODONE/ACETAMINOPHEN, HYDROCODONE/IBUPROFEN
# HYDROMORPHONE HCL/PF, IBUPROFEN/OXYCODONE HCL, LEVORPHANOL TARTRATE, MEPERIDINE HCL, MEPERIDINE HCL/PF, NALBUPHINE HCL,
# OPIUM TINCTURE, OPIUM/BELLADONNA ALKALOIDS, OXYCODONE HCL/ASPIRIN, PENTAZOCINE HCL/ACETAMINOPHEN, PENTAZOCINE HCL/NALOXONE HCL,
# PENTAZOCINE LACTATE, TAPENTADOL HCL, TRAMADOL HCL/ACETAMINOPHEN

sum(prescribers$Opioid.Prescriber)

# 14,688 prescribers have value of Opioid Prescriber = 1, assumed to be True

# Slicing gathered dataset for only opioid drugs and Opioid prescribers. 
opioid_per_npi <- rx_per_npi[matching_records == TRUE,]

opioid_bynpi_hirx <- opioid_per_npi[opioid_per_npi$Opioid.Prescriber == 1,]
non_prescribers <- opioid_per_npi[opioid_per_npi$Opioid.Prescriber == 0,]

# Need to add mean, max, summaries/bar plots
opioid_bynpi_hirx %>% 
  group_by(rx) %>% 
  summarise(sd(countrx))

drugs <- unique(rx_per_npi$rx)
