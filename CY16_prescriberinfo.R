
library(tidyr)
library(magrittr)
library(data.table)
library(dplyr)
t0 <- proc.time()
row.number.limit <- 25000
num_drugs <- 250 # maximum number of prescription drugs to randomly select from the total pool, will be truncated if too large


NROWS = -1 # negative number will use all rows in the original data
# Read data files
prescriber.info <- data.frame(fread("data/PartD_Prescriber_PUF_NPI_Drug_16.txt",header=TRUE,sep="\t",nrows=NROWS)) # contains information for each drug and prescription writer
meta <- data.frame(fread("data/PartD_Prescriber_PUF_NPI_16.txt",header=TRUE,sep="\t")) # contains details about prescription writer

# I'll choose the most common drugs. This is biased, but with so many features I want to capture some meaningful information without the dataset being enormous
drug_names <- prescriber.info %>%
  group_by(drug_name) %>%
  summarise(occurences = n()) %>%
  arrange(desc(occurences))
tot_num_drugs <- length(drug_names$drug_name)

# Select a sample of the total drugs
num_drugs <- ifelse(tot_num_drugs <= num_drugs,tot_num_drugs,num_drugs)
#drugs <- as.character(drug_names[sample(tot_num_drugs,num_drugs)])
drugs <- as.character(drug_names$drug_name[1:num_drugs])

print(paste("Total Number of Drugs:",num_drugs))

# Replace any hypenated or compound drug names with periods
drugs <- sort(gsub("\ |-",".",drugs))
prescriber.info$drug_name <- gsub("\ |-",".",prescriber.info$drug_name)

# Only consider entries that prescribed at least one of the drugs
prescriber.info %<>% filter(drug_name %in% drugs)
prescriber.info <- data.frame(prescriber.info)

# Combine the prescriptions for drugs that are repeated (multiple entries for the same drug for the same prescriber)
prescriber.info <- prescriber.info %>%
  group_by(npi,nppes_provider_last_org_name,nppes_provider_first_name,drug_name) %>%
  mutate(total_claim_count=sum(total_claim_count,na.rm=TRUE)) %>%
  filter(!duplicated(drug_name)) %>%
  ungroup()

# Convert from long to wide format and collapse the rows to one row per prescriber with the number of prescriptions written for each drug
prescriber.info <- prescriber.info %>% 
  select(npi,drug_name, total_claim_count) %>%
  spread(key=drug_name, value=total_claim_count,fill=0) %>%
  select(npi, one_of(drugs))

head(prescriber.info %>% arrange(npi),n=10)

# Merge with metadata about the prescriber

prescriber.info <- prescriber.info %>% 
  merge(meta, by="npi") %>%
  mutate(Opioid.Prescriber=ifelse( (opioid_bene_count<10 | is.na(opioid_bene_count)) & (opioid_claim_count<10 | is.na(opioid_claim_count)) ,0,1)) %>%
  select(npi, Gender=nppes_provider_gender, State=nppes_provider_state, Credentials=nppes_credentials, Specialty=specialty_description, one_of(drugs), Opioid.Prescriber) %>%
  slice(sample(nrow(prescriber.info),row.number.limit))

# head(prescriber.info %>% arrange(NPI),n=10)
write.csv(prescriber.info,'prescriber-info.csv_CY16',row.names=FALSE)
print(sprintf("Finished in %f seconds",(proc.time()-t0)[3]))

