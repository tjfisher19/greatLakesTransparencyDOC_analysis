#############################################################
##
## 01_data_processing.R
##
## This first file instructs the user on where to get the
##    data and does all the initial processing
##
##
## Step 1 - Download the zip file from here: 
##   https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1321&revision=6
##
## Unzip in a folder with this file at the base directory
##    ex: this file is saved in /home/user/project1
##
##    The data is in /home/user/project1/
##  
## Step 2 - We use the data that was supplied from the original source here:
##   https://data.ontario.ca/dataset/water-chemistry-great-lakes-nearshore-areas
##
##   NOTE: Our version is in other_data_sources/DOC Data_GLMNN 2002-2022_ORIGINAL.xlsx
##
## Step 3 - User the BlagraveIDTable data from our github
##    https://github.com/tjfisher19/greatLakesTransparencyDOC_analysis/other_data_sources
##
## Put in another folder
##       /home/user/project1/blagraveID
##

library(tidyverse)



###################
## We do not consider these sites in our analysis
sites_to_drop <- c("Site596", "Site597", "Site598", "Site601", "Site619", "Site620", "Site621", "Site622",
                   "Site623", "Site624", "Site637", "Site638", "Site639", "Site640", "Site650", "Site589", "Site590",
                   "Site591", "Site592", "Site593", "Site594", "Site595", "Site669", "Site670", "Site671", "Site672",
                   "Site673", "Site1055", "Site1056", "Site1057", "Site1058", "Site1059", "Site1060", "Site1061",
                   "Site1062", "Site1063", "Site1064", "Site1065", "Site1066", "Site1067", "Site1068", "Site1069",
                   "Site1074")


###############################
## Get the UV and PAR data
##   along with information on each site

tmp_light_data <- read_csv("./edi.1321.6/Data_GreatLakes.csv") |>
  dplyr::filter(!SiteID %in% sites_to_drop)

################################
## Eventually use this code, but
##   The EDI database need to
##   be updated

# tmp_site_data <- read_csv("./edi.1321.6/SiteInformation_GreatLakes.csv") |>
  # dplyr::filter(!SiteID %in% sites_to_drop)

## So for now, use this
tmp_site_data <- read_csv("./other_data_sources/Howell-StationID.csv") |>
  dplyr::filter(!SiteID %in% sites_to_drop) |>
  mutate(Lake = str_extract(NewSiteName, "Erie|Superior|Huron|Ontario") ) |>
  dplyr::select(SiteID, Site=NewSiteName, Lake, BOW, Station)

## Some processing and merging/joining

light_data <- tmp_light_data |>
  dplyr::filter(Variable %in% c("Kd320", "KdPAR") ) |>
  mutate(ss.1pc.estimate = 4.605/Value,
         Date = date(Date)) |>
  left_join(tmp_site_data,
            by=c("SiteID")
  ) |>
  dplyr::select(SiteID, Date, Variable, ss.1pc.estimate, BOW, Station, Lake)



####################################################
## Now the DOC data
#########################################################
##  Date from 2002--2022
## 
##  Currently from an emailed file
##    eventually use the originals 
##    once published online - code similar to the above
##    should work
##
tmp_lake_doc_data <- readxl::read_excel("./other_data_sources/DOC Data_GLMN 2002-2022_ORGINAL.xlsx")

lakes_doc_data <- tmp_lake_doc_data |>
  filter(TestCode == "DOC",
         SMP == "12") |>
  mutate(Date = date(Date) ) %>%
  mutate(STATION_FULL = as.character(STATION_FULL) ) |>
  mutate(BOW = ifelse(str_length(STATION_FULL)==9,
                      as.numeric(str_sub(STATION_FULL, 1, 1) ),
                      as.numeric(str_sub(STATION_FULL, 1, 2) ) ),
         Station = ifelse(str_length(STATION_FULL)==9,
                          as.numeric(str_sub(STATION_FULL, 6, 9) ),
                          as.numeric(str_sub(STATION_FULL, 7, 10) ) ) ) |>
  group_by(Date=date(Date), lat=LAT, long=LONG, BOW, Station) %>%
  summarize(DOC = mean(as.numeric(Result), na.rm=TRUE))

 


###############################################
## Merge/join the light data with the DOC
##
## Here, when the Date, BOW and Station numbers
##    match.  Alternatively, you could try 
##    and do this when the long & lat match
light_doc_data <- light_data |> 
  inner_join(lakes_doc_data,
             by=c("Date", "BOW", "Station"))


##############################
## Lastly, the Blagrave IDs
tmp_blagrave <- readxl::read_excel("other_data_sources/BlagraveIDTable_23sep2024.xlsx")

full_light_data <- light_doc_data |>
  inner_join(tmp_blagrave, by="SiteID")

#######################################################
## Some additional data Processing
##
## Add seasonality and label some of the factor
##   variables for better presentation later
## 
#######################################################
## To determine the season we look at the date
##    of observation, and use astronomial seasons
## Snagged from here: https://stackoverflow.com/questions/36502140/determine-season-from-date-using-lubridate-in-r
##
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}


full_light_data <- full_light_data |>
  mutate(Season = getSeason(Date),
         WaterBody = case_when(str_sub(BlagraveID, 1,1)=="E" ~ "Embayment",
                               str_sub(BlagraveID, 1,1)=="O" ~ "Open Waters",
                               .default = NA),
         RiverPresent = case_when(str_sub(BlagraveID, 2,2)=="R" ~ "River",
                                  str_sub(BlagraveID, 2,2)=="N" ~ "None",
                                  .default= NA) ,
         Year = year(Date)-2001 ) |>
  mutate(Lake = factor(Lake, levels=c("Superior", "Huron", "Erie", "Ontario")),
         BlagraveID = factor(BlagraveID, levels=c("EN", "ER", "ON", "OR"),
                             labels=c("Embayment,\nNo River", "Embayment,\nRiver Present", "Open Sea,\nNo River", "Open Sea,\nRiver Present")))


#####################################
## We will analyze the UV data and
##    PAR data later, we drop
##    any winter measurements
##    and remove missing values

UVdata <- full_light_data %>%
  filter(Variable == "Kd320", 
         Season != "Winter") %>%
  drop_na()


PARdata <- full_light_data %>%
  filter(Variable == "KdPAR", 
         Season != "Winter") %>%
  drop_na()

## Save the results

save(full_light_data, UVdata, PARdata,
     file="fully_processed_data.RData")


