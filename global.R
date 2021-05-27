library(readr)
library(plyr)
library(plotly)
library(dplyr)
library(shiny)
library(shinythemes)
library(shinyjs)


# get the data from the john hopkins github repository (data provided by who)
confirmed <-
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

confirmed_data <-
  as.data.frame(read_csv(url(confirmed), col_types = cols()))

confirmed_data$`Country/Region` <-
  data.table::fifelse(
    is.na(confirmed_data$`Province/State`),
    confirmed_data$`Country/Region`,
    paste0(
      confirmed_data$`Country/Region`,
      " ",
      "(",
      confirmed_data$`Province/State`,
      ")"
    )
  )

deaths <-
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

death_data <-
  as.data.frame(read_csv(url(deaths), col_types = cols()))

death_data$`Country/Region` <-
  data.table::fifelse(
    is.na(death_data$`Province/State`),
    death_data$`Country/Region`,
    paste0(
      death_data$`Country/Region`,
      " ",
      "(",
      death_data$`Province/State`,
      ")"
    )
  )

recovered <-
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

recover_data <-
  as.data.frame(read_csv(url(recovered), col_types = cols()))

recover_data$`Country/Region` <-
  data.table::fifelse(
    is.na(recover_data$`Province/State`),
    recover_data$`Country/Region`,
    paste0(
      recover_data$`Country/Region`,
      " ",
      "(",
      recover_data$`Province/State`,
      ")"
    )
  )

# get the global time series
# confirmed case
global_conf <- select(confirmed_data, -c(1, 3, 4))
global_conf <- as.data.frame(t(global_conf))
global_conf_u <- global_conf[2:nrow(global_conf),]
gdate <- global_conf_u
global_conf_u <- data.frame(apply(global_conf_u, 2, as.numeric))
global_conf_u$Date <-
  as.Date(row.names(gdate), format = "%m/%d/%y")
names(global_conf_u) <- c(confirmed_data$`Country/Region`, "Date")

global_conf_u <-
  mutate(global_conf_u, global = rowSums(global_conf_u[, 1:(ncol(global_conf_u) -
                                                              1)]))

# deaths
global_death <- select(death_data, -c(1, 3, 4))
global_death <- as.data.frame(t(global_death))
global_death_u <- global_death[2:nrow(global_death),]
gdate <- global_death_u
global_death_u <- data.frame(apply(global_death_u, 2, as.numeric))
global_death_u$Date <-
  as.Date(row.names(gdate), format = "%m/%d/%y")
names(global_death_u) <- c(death_data$`Country/Region`, "Date")

global_death_u <-
  mutate(global_death_u, global = rowSums(global_death_u[, 1:(ncol(global_death_u) -
                                                                1)]))
# recovery
global_recov <- select(recover_data, -c(1, 3, 4))
global_recov <- as.data.frame(t(global_recov))
global_recov_u <- global_recov[2:nrow(global_recov),]
gdate <- global_recov_u
global_recov_u <- data.frame(apply(global_recov_u, 2, as.numeric))
global_recov_u$Date <-
  as.Date(row.names(gdate), format = "%m/%d/%y")
names(global_recov_u) <- c(recover_data$`Country/Region`, "Date")

global_recov_u <-
  mutate(global_recov_u, global = rowSums(global_recov_u[, 1:(ncol(global_recov_u) -
                                                                1)]))


# get the global summary
confirmed_cases <- sum(confirmed_data[, ncol(confirmed_data)])
deaths <- sum(death_data[, ncol(death_data)])
recovered <- sum(recover_data[, ncol(recover_data)])

# filter only bangladesh data
bangladesh_confirmed <-
  filter(confirmed_data, confirmed_data[, 2] == "Bangladesh")

bangladesh_death <-
  filter(death_data, death_data[, 2] == "Bangladesh")

bangladesh_recover <-
  filter(recover_data, recover_data[, 2] == "Bangladesh")

bangladesh_data <-
  bind_rows(bangladesh_confirmed, bangladesh_death, bangladesh_recover)

# keep only the required columns
bangladesh_data <-
  select(bangladesh_data, c(5:ncol(bangladesh_data)))

# turn the data long from wide
bangladesh_data <- as.data.frame(t(bangladesh_data))

# rename the columns
bangladesh_data <-
  rename(
    bangladesh_data,
    Confirmed = V1,
    Deaths = V2,
    Recovered = V3
  )

# get the dates and format the column
bangladesh_data$Date <- row.names(bangladesh_data)

bangladesh_data$Date <-
  as.Date(bangladesh_data$Date, format = "%m/%d/%y")

bangladesh_data <- na_if(bangladesh_data, 0)

# get the list of the available countries
country_list <- confirmed_data$`Country/Region`

# remove bangladesh from the available counties (as it is the base country)
country_list <- country_list %>% .[. != "Bangladesh"]
