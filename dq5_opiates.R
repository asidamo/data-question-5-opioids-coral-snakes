# Opioids data - remove duplicates from Generic.Name - only field needed
# prescribers data - split drug data from prescriber data in order
#                    to rotate drug data

library(tidyverse)
library(magrittr)
library(ggplot2)

opioids <- read.csv('data/opioids.csv')
overdoses <- read.csv('data/overdoses.csv')
prescribers <- read.csv('data/prescriber-info.csv')

rx_per_prescriber <- gather(prescribers, rx, countrx, ABILIFY:ZOLPIDEM.TARTRATE)

generics <- unique(opioids$Generic.Name)


rx_per_prescriber[rx_per_prescriber$rx %in% generics]



