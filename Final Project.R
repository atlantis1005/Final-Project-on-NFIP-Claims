library(tidyverse)
library(tidycensus)
library(janitor)

nfip <- read_csv("https://www.fema.gov/api/open/v2/FimaNfipClaims.csv")


nfipNYC <- nfip %>% 
  filter(state == 'NY',
         originalNBDate >= '2012-01-01' & originalNBDate <'2023-01-01',
         countyCode == '36005' | countyCode == '36047' | countyCode == '36061' | countyCode == '36081' | countyCode == '36085'
         ) %>% 
  clean_names()


nfipNYC %>%
  group_by(floodEvent) %>% 
  summarize(numberclaims = n()) 

nfipNYC_sandy<- nfipNYC %>% 
  filter(flood_event == "Hurricane Sandy") %>% 
  group_by(reported_zip_code) %>% 
  summarize (totalclaims = n())

nfipNYC_ida<- nfipNYC %>% 
  filter(flood_event == "Hurricane Ida") %>% 
  group_by(reported_zip_code) %>% 
  summarize (totalclaims = n())

zhvi <- read_csv("Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
zhvi$RegionName <- as.character(zhvi$RegionName)

zhviNYC <- zhvi %>% 
  filter(City == 'New York') %>% 
  clean_names()

zhviSandy <- zhviNYC %>% 
  select(region_name, x9_30_12, x9_30_13, x9_30_14, x9_30_15, x9_30_16) 
  
zhviIda <- zhviNYC %>% 
  select(region_name, x8_31_21, x8_31_22, x8_31_23)
  
hvi_claims_sandy <- full_join(zhviSandy, nfipNYC_sandy, join_by ("region_name" == "reported_zip_code") )


hvi_claims_sandy$totalclaims[is.na(hvi_claims_sandy$totalclaims)] = 0

hvi_claims_ida <- full_join(zhviIda, nfipNYC_ida, join_by ("region_name" == "reported_zip_code") )
hvi_claims_ida$totalclaims[is.na(hvi_claims_ida$totalclaims)] = 0

ggplot(hvi_claims_sandy)+
  aes(x=totalclaims, y= x9_30_12)+
  geom_point()

ggplot(hvi_claims_sandy)+
  aes(x=totalclaims, y= x9_30_15)+
  geom_point()

ggplot(hvi_claims_ida)+
  aes(x=totalclaims, y= x8_31_21)+
  geom_point()

ggplot(hvi_claims_ida)+
  aes(x=totalclaims, y= x8_31_23)+
  geom_point()+
  theme_classic()

write.csv(zhviNYC, "zhviNYC.csv", row.names=FALSE)

write.csv(nfipNYC_sandy, "nfipNYC_sandy.csv", row.names=FALSE)

write.csv(nfipNYC_ida, "nfipNYC_ida.csv", row.names=FALSE)

  