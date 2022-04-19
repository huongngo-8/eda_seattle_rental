library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(sf)

# read in data
rental_permits <- read_csv("/Users/huongngo/Desktop/PERSONAL PROJECTS/eda_seattle_rentals/Data/Rental_Property_Registration.csv")

# check if data set contains any missing registered dates and (latitude, longitude)
colMeans(is.na(rental_permits))

# remove all registrations that have missing (latitude, longitude)
rental_permits <- rental_permits %>%
  filter(!is.na(Longitude),
         !is.na(Latitude))

# convert dates into a processable format
registered_date <- as.POSIXlt(rental_permits$RegisteredDate,
           format = "%Y/%m/%d")
expires_date <- as.POSIXlt(rental_permits$ExpiresDate,
                              format = "%Y/%m/%d")

# create registered year and month column
rental_permits$RegisteredYear = year(registered_date)
rental_permits$RegisteredMonth = month(registered_date)
rental_permits$ExpiresYear = year(expires_date)
rental_permits$ExpiresMonth = month(expires_date)

# remove unnecessary columns
rental_permits <- rental_permits %>%
  select(-Link, -Location1)

rental_permits_sf <- st_as_sf(rental_permits,
                              coords = c("Longitude", "Latitude"),
                              crs = 4326)

# loading geographic boundaries
community_reporting_areas <-
  st_read("~/Desktop/PERSONAL PROJECTS/eda_seattle_rentals/Data/Community_Reporting_Areas/") %>%
  select(GEN_ALIAS, NEIGHDIST) %>%
  rename(CommunityReportingArea = GEN_ALIAS,
         NeighborhoodDistrict = NEIGHDIST)

council_districts <-
  st_read("~/Desktop/PERSONAL PROJECTS/eda_seattle_rentals/Data/Council_Districts/") %>%
  select(C_DISTRICT) %>%
  rename(CouncilDistrict = C_DISTRICT)

# match registration to council districts and community reporting areas
rental_permits_final <- rental_permits_sf %>%
  st_join(council_districts) %>%
  st_join(community_reporting_areas) %>%
  st_set_geometry(NULL)

# save cleaned data
write_csv(rental_permits_final, "~/Desktop/PERSONAL PROJECTS/eda_seattle_rentals/Data/rental_registration_clean.csv")




