library(tidyverse)
library(tidycensus)
library(janitor)

#Loading FEMA NFIP dataset
nfip <- read_csv("https://www.fema.gov/api/open/v2/FimaNfipClaims.csv")

#Filtering to only NYC claims between 2012 and 2023
nfipNYC <- nfip %>% 
  filter(state == 'NY',
         originalNBDate >= '2012-01-01' & originalNBDate <'2023-01-01',
         countyCode == '36005' | countyCode == '36047' | countyCode == '36061' | countyCode == '36081' | countyCode == '36085'
         ) %>% 
  clean_names()

#Looking at flood events with most claims (Hurricanes Sandy and Ida)
nfipNYC %>%
  group_by(flood_event) %>% 
  summarize(numberclaims = n()) 

#Isolating Sandy related claims by zip code
nfipNYC_sandy<- nfipNYC %>% 
  filter(flood_event == "Hurricane Sandy") %>% 
  group_by(reported_zip_code) %>% 
  summarize (totalclaims = n())

#Isolating Sandy related claims by zip code
nfipNYC_ida<- nfipNYC %>% 
  filter(flood_event == "Hurricane Ida") %>% 
  group_by(reported_zip_code) %>% 
  summarize (totalclaims = n())

#Loading Zillow Home Value Index Data
zhvi <- read_csv("Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") %>% 
  mutate(RegionName = as.character(RegionName))

#Isolating NYC
zhviNYC <- zhvi %>% 
  filter(City == 'New York') %>% 
  clean_names()

#Isolating some relevant years and calculating a percent change over 2 years for
#both Sandy and Ida
zhviSandy <- zhviNYC %>% 
  select(region_name, x9_30_12, x9_30_13, x9_30_14, x9_30_15, x9_30_16) %>% 
  mutate(pct_change1412 = (x9_30_14 - x9_30_12) / x9_30_12 )
  
zhviIda <- zhviNYC %>% 
  select(region_name, x8_31_21, x8_31_22, x8_31_23)%>% 
  mutate(pct_change2321 = (x8_31_23 - x8_31_21) / x8_31_21 )

#Exporting CSVs for use in QGIS
write.csv(zhviIda, "zhviIda.csv", row.names=FALSE)
write.csv(zhviSandy, "zhviSandy.csv", row.names=FALSE)


#Joining the zillow and nfip data with a full join to see how other unaffected
#zipcodes fared
hvi_claims_sandy <- full_join(zhviSandy, nfipNYC_sandy, join_by ("region_name" == "reported_zip_code") )
hvi_claims_sandy$totalclaims[is.na(hvi_claims_sandy$totalclaims)] = 0

hvi_claims_ida <- full_join(zhviIda, nfipNYC_ida, join_by ("region_name" == "reported_zip_code") )
hvi_claims_ida$totalclaims[is.na(hvi_claims_ida$totalclaims)] = 0

#Joining the zillow and nfip data with a left join to only look at zipcodes with
#nfip claims
hvi_claims_sandy_left <-  left_join (nfipNYC_sandy, zhviSandy, join_by ("reported_zip_code" == "region_name") )
hvi_claims_ida_left <-  left_join (nfipNYC_ida, zhviIda, join_by ("reported_zip_code" == "region_name") )

#Exporting CSVs for use in QGIS
write.csv(nfipNYC_sandy, "nfipNYC_sandy.csv", row.names=FALSE)
write.csv(nfipNYC_ida, "nfipNYC_ida.csv", row.names=FALSE)

#For access to census data
census_api_key("3b79f2297b821c222a4a0c50536134944d3f9e91", install = TRUE)

# B07011_001(median income total)
#	B07012_002 (below 100% poverty level)

acs12 <- get_acs(
  geography = "zcta", 
  variables = c( med_inc12 = "B07011_001",
                 pov12 = "B07012_002",
                 pop12 = "B01003_001"), 
  state = "New York", 
  output = "wide",
  year = 2012, survey = "acs5") %>% 
  mutate(zipcode = str_sub(NAME, 7, 11)) %>%
  mutate( pov_pct12 = pov12E / pop12E) %>% 
  clean_names()

acs13 <- get_acs(
  geography = "zcta", 
  variables = c( med_inc13 = "B07011_001",
                 pov13 = "B07012_002",
                 pop13 = "B01003_001"), 
  state = "New York", 
  output = "wide",
  year = 2013, survey = "acs5") %>% 
  mutate(zipcode = str_sub(NAME, 7, 11)) %>%
  mutate( pov_pct13 = pov13E / pop13E) %>% 
  clean_names()


acs21 <- get_acs(
  geography = "zcta", 
  variables = c( med_inc21 = "B07011_001",
                 pov21 = "B07012_002",
                 pop21 = "B01003_001"), 
  output = "wide",
  year = 2021, survey = "acs5") %>% 
  mutate(zipcode = str_sub(NAME, 7, 11)) %>% 
  mutate( pov_pct21 = pov21E / pop21E) %>% 
   clean_names()

acs22 <- get_acs(
  geography = "zcta", 
  variables = c( med_inc22 = "B07011_001",
                 pov22 = "B07012_002",
                 pop22 = "B01003_001"), 
  output = "wide",
  year = 2022, survey = "acs5") %>% 
  mutate(zipcode = str_sub(NAME, 7, 11)) %>% 
  mutate( pov_pct22 = pov22E / pop22E) %>% 
  clean_names()

#Joining acs data to the joined home value- nfip claim data and exporting
hvi_claims_sandy_left_acs <- left_join(hvi_claims_sandy_left, acs12, join_by("reported_zip_code"== "zipcode") )

hvi_claims_ida_left_acs <- left_join(hvi_claims_ida_left, acs21, join_by("reported_zip_code"== "zipcode") )

write.csv(hvi_claims_sandy_left_acs, "hvi_claims_sandy_left_acs.csv", row.names=FALSE)

write.csv(hvi_claims_ida_left_acs, "hvi_claims_ida_left_acs.csv", row.names=FALSE)


#Finding zip codes that have claims from both Sandy and Ida

total <- full_join(hvi_claims_ida_left, hvi_claims_sandy_left, by = "reported_zip_code") 

intersection <- total %>% 
  filter( totalclaims.x >= 1 & totalclaims.y >=1 ) %>% 
  mutate(ida_claims = totalclaims.x) %>% 
  mutate(sandy_claims = totalclaims.y)
  
#exporting for use in QGIS
write.csv(intersection, "intersection.csv", row.names=FALSE)




  