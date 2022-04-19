library(tidyverse)
library(ggplot2)
library(kableExtra)
library(ggpubr)
rental_reg <- read_csv("~/Desktop/PERSONAL PROJECTS/eda_seattle_rentals/Data/rental_registration_clean.csv")

g_theme <- theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))


rental_reg$RegisteredYearFct <- factor(rental_reg$RegisteredYear)
rental_reg$RegisteredMonthFct <- factor(rental_reg$RegisteredMonth)
rental_reg$ExpiresYearFct <- factor(rental_reg$ExpiresYear)
rental_reg$ExpiresMonthFct <- factor(rental_reg$ExpiresMonth)

rental_reg$NeighborhoodDistrict[rental_reg$NeighborhoodDistrict == "Delridge Neighborhoods"] <- "Delridge"
rental_reg$NeighborhoodDistrict[rental_reg$NeighborhoodDistrict == "Magnolia/Queen Anne"] <- "Magnolia"

ggplot(rental_reg %>% 
         filter(RegisteredYear %in% c(2018, 2019, 2020, 2021, 2022)), 
       aes(x = RegisteredYear)) +
  geom_bar() +
  labs(title = "New Rental Registrations Each Year",
       x = "Year",
       y = "Number of rental registrations",
       caption = "Figure 1") +
  g_theme

total_rental <- rental_reg %>%
  filter(RegisteredYearFct %in% c(2018, 2019, 2020, 2021, 2022)) %>%
  count(RegisteredYearFct)
total_rental <- rename(total_rental, Registrations = n, "Registered Year" = RegisteredYearFct)
kable_styling(kable(total_rental))

year <- c(2018, 2019, 2020, 2021, 2022)
year <- factor(year)

reg_year_2018 <- "2015, 2016, 2017, 2018"
reg_year_2019 <- "2015, 2016, 2017, 2018, 2019"
reg_year_2020 <- "2016, 2017, 2018, 2019, 2020"
reg_year_2021 <- "2017, 2018, 2020, 2021"
reg_year_2022 <- "2018, 2019, 2021, 2022"

existing_reg_tb <- data.frame("Year" = year, 
                              "Registration Years" = c(reg_year_2018, 
                                                       reg_year_2019,
                                                       reg_year_2020,
                                                       reg_year_2021,
                                                       reg_year_2022),
                              check.names = FALSE)
kable_styling(kable(existing_reg_tb))

existing_reg_2018 <- rental_reg %>%
  filter(RegisteredYear %in% c(2015, 2016, 2017, 2018))

existing_reg_2019 <- rental_reg %>%
  filter(RegisteredYear %in% c(2015, 2016, 2017, 2018, 2019))

existing_reg_2020 <- rental_reg %>%
  filter(RegisteredYear %in% c(2016, 2017, 2018, 2019, 2020))

existing_reg_2021 <- rental_reg %>%
  filter(RegisteredYear %in% c(2017, 2018, 2020, 2021))

existing_reg_2022 <- rental_reg %>%
  filter(RegisteredYear %in% c(2018, 2019, 2021, 2022))

no_of_reg <- c(nrow(existing_reg_2018), nrow(existing_reg_2019), nrow(existing_reg_2020), nrow(existing_reg_2021) + 102, nrow(existing_reg_2022) + 429)

year <- c(2018, 2019, 2020, 2021, 2022)

year <- factor(year)

existing_reg <- data.frame(year = year,
                           no_of_reg = no_of_reg)

existing_reg_tb <- data.frame(Year = year,
                              "Number of Registrations" = no_of_reg,
                              check.names = FALSE)

ggplot(existing_reg, aes(x = year,
                         y = no_of_reg)) +
  geom_col() +
  labs(title = "Existing Registrations in Each Year",
       x = "Year",
       y = "Number of registrations",
       caption = "Figure 2") +
  g_theme

kable_styling(kable(existing_reg_tb))

monthly_reg <- rental_reg %>%
  filter(RegisteredYearFct %in% c(2018, 2019, 2020, 2021, 2022)) %>%
  group_by(RegisteredYearFct, RegisteredMonthFct) %>%
  count(RegisteredMonthFct)

ggplot(monthly_reg, aes(x = RegisteredMonthFct,
                        y = n,
                        group = RegisteredYearFct,
                        color = RegisteredYearFct)) +
  geom_line() +
  labs(title = "New Registrations in Each Month",
       x = "Month",
       y = "Number of Registrations",
       subtitle = "Grouped by year",
       color = "Year",
       caption = "Figure 3") +
  g_theme

neighborhood_reg <- rental_reg %>%
  filter(RegisteredYearFct %in% c(2018, 2019, 2020, 2021, 2022)) %>%
  group_by(NeighborhoodDistrict, RegisteredYearFct) %>%
  count(RegisteredYearFct)

ggplot(neighborhood_reg %>% filter(!is.na(NeighborhoodDistrict)), aes(x = RegisteredYearFct,
                                                                      y = n)) +
  geom_col() +
  labs(title = "New Rental Registrations Each Year",
       x = "Year",
       y = "Number of registrations",
       subtitle = "Faceted by neighborhood",
       caption = "Figure 4") +
  facet_wrap(vars(NeighborhoodDistrict), scales = "free_x") +
  g_theme

existing_reg_2018_n <- rental_reg %>%
  filter(RegisteredYear %in% c(2015, 2016, 2017, 2018), 
         !is.na(NeighborhoodDistrict)) %>%
  group_by(NeighborhoodDistrict) %>%
  count(NeighborhoodDistrict)

existing_reg_2019_n <- rental_reg %>%
  filter(RegisteredYear %in% c(2015, 2016, 2017, 2018, 2019), 
         !is.na(NeighborhoodDistrict)) %>% 
  group_by(NeighborhoodDistrict) %>%
  count(NeighborhoodDistrict)

existing_reg_2020_n <- rental_reg %>%
  filter(RegisteredYear %in% c(2016, 2017, 2018, 2019, 2020), 
         !is.na(NeighborhoodDistrict)) %>% 
  group_by(NeighborhoodDistrict) %>%
  count(NeighborhoodDistrict)

existing_reg_2021_n <- rental_reg %>%
  filter(RegisteredYear %in% c(2017, 2018, 2020, 2021), 
         !is.na(NeighborhoodDistrict)) %>% 
  group_by(NeighborhoodDistrict) %>%
  count(NeighborhoodDistrict)

existing_reg_2022_n <- rental_reg %>%
  filter(RegisteredYear %in% c(2018, 2019, 2021, 2022), 
         !is.na(NeighborhoodDistrict)) %>% 
  group_by(NeighborhoodDistrict) %>%
  count(NeighborhoodDistrict)

existing_reg_2018_n <- existing_reg_2018_n %>%
  mutate(year = 2018)

existing_reg_2019_n <- existing_reg_2019_n %>%
  mutate(year = 2019)

existing_reg_2020_n <- existing_reg_2020_n %>%
  mutate(year = 2020)

existing_reg_2021_n <- existing_reg_2021_n %>%
  mutate(year = 2021)

existing_reg_2022_n <- existing_reg_2022_n %>%
  mutate(year = 2022)

existing_reg_n <- rbind(existing_reg_2018_n, 
                        existing_reg_2019_n, 
                        existing_reg_2020_n, 
                        existing_reg_2021_n, 
                        existing_reg_2022_n)

ggplot(existing_reg_n, aes(x = year, 
                           y = n)) +
  geom_col() +
  labs(title = "Existing Rental Registrations Each Year",
       x = "Year",
       y = "Number of registrations",
       subtitle = "Faceted by neighborhood", 
       caption = "Figure 5") +
  facet_wrap(vars(NeighborhoodDistrict), scales = "free_x") +
  g_theme

neighborhood_reg_line <- rental_reg %>%
  filter(RegisteredYearFct %in% c(2018, 2020, 2021, 2022),
         NeighborhoodDistrict %in% c("Magnolia", "Northwest", "Northeast", "Lake Union")) %>%
  group_by(NeighborhoodDistrict, RegisteredYearFct, RegisteredMonthFct) %>%
  count(RegisteredMonthFct)

magnolia <- ggplot(neighborhood_reg_line %>% filter(NeighborhoodDistrict == "Magnolia"), aes(x = RegisteredMonthFct,
                                                                                             y = n,
                                                                                             group = RegisteredYearFct,
                                                                                             color = RegisteredYearFct)) +
  geom_line() +
  labs(title = "New Registrations Each Month (Magnolia)",
       subtitle = "Grouped by year", 
       x = "Month", 
       y = "Number of rental registrations", 
       color = "Year", 
       caption = "Figure 5") +
  g_theme

northwest <- ggplot(neighborhood_reg_line %>% filter(NeighborhoodDistrict == "Northwest"), aes(x = RegisteredMonthFct,
                                                                                               y = n,
                                                                                               group = RegisteredYearFct,
                                                                                               color = RegisteredYearFct)) +
  geom_line() +
  labs(title = "New Registrations Each Month (Northwest)",
       subtitle = "Grouped by year", 
       x = "Month", 
       y = "Number of rental registrations", 
       color = "Year", 
       caption = "Figure 6") +
  g_theme

lake_union <- ggplot(neighborhood_reg_line %>% filter(NeighborhoodDistrict == "Lake Union"), aes(x = RegisteredMonthFct,
                                                                                                 y = n,
                                                                                                 group = RegisteredYearFct,
                                                                                                 color = RegisteredYearFct)) +
  geom_line() +
  labs(title = "New Registrations Each Month (Lake Union)",
       subtitle = "Grouped by year", 
       x = "Month", 
       y = "Number of rental registrations", 
       color = "Year", 
       caption = "Figure 7") +
  g_theme

northeast <- ggplot(neighborhood_reg_line %>% filter(NeighborhoodDistrict == "Northeast"), aes(x = RegisteredMonthFct,
                                                                                               y = n,
                                                                                               group = RegisteredYearFct,
                                                                                               color = RegisteredYearFct)) +
  geom_line() +
  labs(title = "New Registrations Each Month (Northeast)",
       subtitle = "Grouped by year", 
       x = "Month", 
       y = "Number of rental registrations", 
       color = "Year", 
       caption = "Figure 8") +
  g_theme

ggarrange(magnolia, northwest, lake_union, northeast, ncol = 2, nrow = 2)

ggplot(neighborhood_reg %>% filter(!is.na(NeighborhoodDistrict), 
                                   RegisteredYearFct %in% c(2020, 2021, 2022)), 
       aes(x = NeighborhoodDistrict,
           y = n)) +
  geom_col() +
  labs(title = "New Rental Registrations in Each Neighborhood",
       x = "Neighborhood",
       y = "Number of registrations",
       subtitle = "Faceted by year", 
       caption = "Figure 9") +
  facet_grid(vars(RegisteredYearFct), scales = "free_x") +
  g_theme

ggplot(existing_reg_n %>% filter(year %in% c(2020, 2021, 2022)), aes(x = NeighborhoodDistrict,
                                                                     y = n)) +
  geom_col() +
  labs(title = "Existing Rental Registrations in Each Neighborhood",
       x = "Neighborhood",
       y = "Number of registrations",
       subtitle = "Faceted by year", 
       caption = "Figure 10") +
  facet_grid(vars(year), scales = "free_x") +
  g_theme

total_units_monthly <- rental_reg %>%
  filter(RegisteredYearFct %in% c(2018, 2019, 2020, 2021, 2022)) %>%
  group_by(RegisteredYearFct, RegisteredMonthFct) %>%
  summarize(total = sum(RentalHousingUnits), .groups = "drop")

ggplot(total_units_monthly, aes(x = RegisteredMonthFct, 
                                y = total, 
                                group = RegisteredYearFct, 
                                color = RegisteredYearFct)) +
  geom_line() + 
  labs(title = "New Rental Units Each Month",
       subtitle = "Grouped by year", 
       x = "Month", 
       y = "Number of rental units", 
       color = "Year", 
       caption = "Figure 10") +
  g_theme

rental_reg <- rental_reg %>%
  mutate(unit_type = case_when(
    RentalHousingUnits == 1 ~ "Single Unit",
    RentalHousingUnits >= 2 & RentalHousingUnits <= 5 ~ "2 - 5 Units",
    RentalHousingUnits >= 6 & RentalHousingUnits <= 25 ~ "6 - 25 Units", 
    RentalHousingUnits >= 26 & RentalHousingUnits <= 50 ~ "26 - 50 Units",
    RentalHousingUnits >= 51 & RentalHousingUnits <= 99 ~ "51 - 99 Units",
    RentalHousingUnits >= 100 & RentalHousingUnits <= 200 ~ "100 - 200 Units",
    RentalHousingUnits > 200 ~ "200+ Units",
  ))

existing_reg_2018_unit <- rental_reg %>%
  filter(RegisteredYear %in% c(2015, 2016, 2017, 2018), 
         !is.na(unit_type)) %>%
  group_by(unit_type) %>%
  count(unit_type)

existing_reg_2019_unit <- rental_reg %>%
  filter(RegisteredYear %in% c(2015, 2016, 2017, 2018, 2019), 
         !is.na(unit_type)) %>% 
  group_by(unit_type) %>%
  count(unit_type)

existing_reg_2020_unit <- rental_reg %>%
  filter(RegisteredYear %in% c(2016, 2017, 2018, 2019, 2020), 
         !is.na(unit_type)) %>% 
  group_by(unit_type) %>%
  count(unit_type)

existing_reg_2021_unit <- rental_reg %>%
  filter(RegisteredYear %in% c(2017, 2018, 2020, 2021), 
         !is.na(unit_type)) %>% 
  group_by(unit_type) %>%
  count(unit_type)

existing_reg_2022_unit <- rental_reg %>%
  filter(RegisteredYear %in% c(2018, 2019, 2021, 2022), 
         !is.na(unit_type)) %>% 
  group_by(unit_type) %>%
  count(unit_type)

existing_reg_2018_unit <- existing_reg_2018_unit %>%
  mutate(year = 2018)

existing_reg_2019_unit <- existing_reg_2019_unit %>%
  mutate(year = 2019)

existing_reg_2020_unit <- existing_reg_2020_unit %>%
  mutate(year = 2020)

existing_reg_2021_unit <- existing_reg_2021_unit %>%
  mutate(year = 2021)

existing_reg_2022_unit <- existing_reg_2022_unit %>%
  mutate(year = 2022)

existing_reg_unit <- rbind(existing_reg_2018_unit, 
                           existing_reg_2019_unit, 
                           existing_reg_2020_unit, 
                           existing_reg_2021_unit, 
                           existing_reg_2022_unit)

ggplot(existing_reg_unit %>% 
         filter(year %in% c(2018, 2020, 2021),
                !is.na(unit_type)), aes(x = unit_type, 
                                        y = n)) +
  geom_col() +
  labs(title = "Existing Registrations for Each Rental Unit", 
       x = "Unit category", 
       y = "Number of registrations", 
       subtitle = "Faceted by year", 
       caption = "Figure 11") +
  facet_grid(vars(year), scales = "free_x") +
  g_theme

existing_reg_2018_unit_n <- rental_reg %>%
  filter(RegisteredYear %in% c(2015, 2016, 2017, 2018), 
         !is.na(unit_type)) %>%
  group_by(NeighborhoodDistrict, unit_type) %>%
  count(unit_type)

existing_reg_2020_unit_n <- rental_reg %>%
  filter(RegisteredYear %in% c(2016, 2017, 2018, 2019, 2020), 
         !is.na(unit_type)) %>% 
  group_by(NeighborhoodDistrict, unit_type) %>%
  count(unit_type)

existing_reg_2021_unit_n <- rental_reg %>%
  filter(RegisteredYear %in% c(2017, 2018, 2020, 2021), 
         !is.na(unit_type)) %>% 
  group_by(NeighborhoodDistrict, unit_type) %>%
  count(unit_type)

existing_reg_2018_unit_n <- existing_reg_2018_unit_n %>%
  mutate(year = 2018)

existing_reg_2020_unit_n <- existing_reg_2020_unit_n %>%
  mutate(year = 2020)

existing_reg_2021_unit_n <- existing_reg_2021_unit_n %>%
  mutate(year = 2021)

existing_reg_2018_2020_2021 <- rbind(existing_reg_2018_unit_n, 
                                     existing_reg_2020_unit_n, 
                                     existing_reg_2021_unit_n)

ggplot(existing_reg_2018_2020_2021 %>% 
         filter(unit_type %in% c("Single Unit"), 
                !is.na(NeighborhoodDistrict)), aes(x = NeighborhoodDistrict, 
                                                   y = n)) +
  geom_col() +
  labs(title = "Existing Registrations for Each Neighborhood (Single Unit)", 
       x = "Neighborhood", 
       y = "Number of registrations", 
       subtitle = "Faceted by year", 
       caption = "Figure 12") +
  facet_grid(vars(year), scales = "free_x") +
  theme_bw(base_size = 27) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5))