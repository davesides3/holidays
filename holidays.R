# pull holidays for multiple years from https://date.nager.at
# customize for specific corporate holidays and save to Excel

library(dplyr, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(writexl)

# pull CSVs for common holidays in the US for
# the years of interest - see date.nager.at for what is available

# initialize - defensive programming
if (exists("all_years")) {
  rm(all_years)
}
for (year in 2019:2025) {
  url_string = paste0("https://date.nager.at/PublicHoliday/Country/US/", year, "/CSV")
  year_holidays <- read.csv(file = url_string) %>% 
    clean_names() %>% 
    rename(holiday = name,
           date_observed = date)
  if (exists("all_years")) {
    all_years <- bind_rows(all_years, year_holidays)
  } else {
    all_years <- year_holidays
  }
}

common_holidays <- all_years %>% 
  # remove optional dates
  filter(type == "Public") %>% 
  # date type for date math
  mutate(date_observed = ymd(date_observed)) %>% 
  # # not perfect, but if a holiday falls on a weekend, choose the closest weekday
  # mutate(date_observed = if_else(wday(date, label = TRUE, abbr = TRUE) == "Sat", date - 1,
  #                                if_else(wday(date, label = TRUE, abbr = TRUE) == "Sun", date + 1,
  #                                date))) %>% 
  # only keep what we need
  select(holiday, date_observed)

# common holidays that are not observed by our company
# note that this does not consider holidays by year so
# depending upon how precise your process you may need
# to add additional logic to account for history
# also tweak some common holiday names
common_trimmed <- common_holidays %>% 
  filter(!(holiday %in% c("Good Friday", "Columbus Day"))) %>% 
  mutate(holiday = if_else(holiday == "Labour Day", "Labor Day", holiday),
         holiday = if_else(holiday == "Washington's Birthday", "Presidents' Day", holiday))
  
# added holidays specific to our company
custom_holidays <- common_trimmed %>% 
  filter(holiday == "Thanksgiving Day") %>% 
  mutate(holiday = "Day After Thanksgiving", date_observed = date_observed + 1)

# merge trimmed and custom holidays and format for convenience
holidays <- common_trimmed %>% 
  bind_rows(custom_holidays) %>% 
  mutate(day_observed = wday(date_observed, label = TRUE, abbr = TRUE),
         calendar_year = if_else(holiday == "New Year's Day" & month(date_observed) == 12,
                                 year(date_observed) + 1, year(date_observed))) %>% 
  select(calendar_year, date_observed, day_observed, holiday) %>% 
  arrange(date_observed)

# save to Excel
tmp <- writexl::write_xlsx(list(Holidays = holidays), "Holidays.xlsx")
