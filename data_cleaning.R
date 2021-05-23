library(tidyverse)
library(readxl)

raw_contaminations <- read_excel('data/563–572 (2020) Dataset.xlsx',sheet = 'contaminations')
raw_viruses <- read_excel('data/563–572 (2020) Dataset.xlsx',sheet = 'viruses')
raw_product_testing <- read_excel('data/563–572 (2020) Dataset.xlsx',sheet = 'product_testing')
raw_testing_methods <- read_excel('data/563–572 (2020) Dataset.xlsx',sheet = 'testing_methods')

raw_viruses <- mutate(raw_viruses,pathogenic_status = ifelse(raw_viruses$human_pathogenic,'Huamn Pathogenic','Non-Human Pathogenic'))
viruses_data <- select(raw_viruses,name,pathogenic_status,short_description)
write.csv(viruses_data,'data/viruses_data.csv')

raw_product_testing <- mutate(raw_product_testing,protocol = ifelse(raw_product_testing$is_cGMP,'cGMP','Non-cGMP'))
raw_product_testing <- mutate(raw_product_testing,quality = ifelse(raw_product_testing$is_contaminated,'Contaminated','Clean'))
product_testing_data <- select(raw_product_testing,product,protocol,quality)
write.csv(product_testing_data,'data/product_testing_data.csv')

write.csv(raw_contaminations,'data/contaminations_data.csv')
write.csv(raw_testing_methods,'data/testing_methods.csv')
