# This script contains the codes to clean the raw data in R and match Excel output

# load packages
pacman::p_load(here, dplyr, ggplot2, lubridate, kableExtra, readxl, janitor, 
               tidyverse, purrr, stringr)

###########################################################
# Contango (July 2020) and Backwardation (June 2021) graph
###########################################################

# Load in the 'Graphs' sheet of the Excel file
  # make sure Excel file is not opened. You will get an error otherwise.  
compare <- read_excel(here("Data", "Wheat Forward Curve Compare.xlsx"), 
                      sheet = "Graphs", range = "B1:F9") %>% # read selected range only 
  select(c(1,2,5)) %>% # select by col index since names are too long
  clean_names() %>%
  rename(contracts = x1) %>%
  # changed to factor so that the order is preserved in the plot
  # i.e. wont graph by alphabetical order of contract names
  mutate(contracts = factor(contracts, levels = contracts))

# recreate graph on p.17 of notes (but not dual axis)
# corn_july20 is the oil forward curve on july 2020
# corn_june21 is the oil forward curve on july 2021

# note: str_wrap(x, 3) from {stringr} allows me to add a line break in the x labels 
  # after 3 characters, which is the length of the month names

corn_july20 <- ggplot(compare, aes(x = stringr::str_wrap(contracts, 3), y = july_2_2020)) +
  geom_col(fill = "lightblue") +
  coord_cartesian(ylim = c(3, 3.80)) + # restricted the y axis limits
  theme_classic(base_size = 15) + # increase size of axis and labels
  geom_text(aes(label = round(july_2_2020,2)), vjust = -0.5, size = 3) + # add value labels
  labs(x = "Contracts", y = "Futures: July 2, 2020 ($/bu)")

corn_june21 <- ggplot(compare, aes(x = stringr::str_wrap(contracts, 3), y = june_2_2021)) +
  geom_col(fill = "orange") +
  coord_cartesian(ylim = c(4.5, 6.8)) +
  theme_classic(base_size = 15) +
  geom_text(aes(label = round(june_2_2021,2)), vjust = -0.5, size = 3) +
  labs(x = "Contracts", y = "Futures: July 2, 2020 ($/bu)")

# corn_futures will do a grouped bar chart similar to the notes, 
  # but not with a dual axis

# first we reshape the data to a long format

compare_long <- compare %>%
  pivot_longer(-contracts, # reshape all columns except contract column
               names_to = "date", values_to = "price")

corn_futures <- ggplot(compare_long, aes(x = stringr::str_wrap(contracts,3), 
                                         y = price, fill = date)) + 
  geom_bar(position = "dodge", stat = "identity") + # grouped bars
  scale_fill_manual(values = c("lightblue", "orange"), # set colors of bars
                    name = "Date", # set legend name
                    labels = c("July 2, 2020", "June 2, 2021")) +
  # set the value labels above the grouped bars
  geom_text(aes(label = round(price,2)), 
            size = 3, position = position_dodge(width = 1), vjust = -0.5) + 
  labs(x = "Contracts", y = "Futures Price ($/bu)") +
  theme_classic(base_size = 13.5)

# remove width/height options if the image size is different from what you like to appear on the page

# ggsave(corn_july20, file = here("Images", "corn_forward_curve_20.png"), width = 8, height = 4)
# ggsave(corn_june21, file = here("Images", "corn_forward_curve_21.png"),  width = 8, height = 4)
# ggsave(corn_futures, file = here("Images", "corn_forward_curve.png"), width = 8, height = 4)  


###########################################################
# WTI Crude Oil: Data Cleaning, Contango, Backwardation
###########################################################

# load data to recreate Combined sheet
    # make sure Excel file is not opened. You will get an error otherwise
crude_spot <- read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), 
                         # looks at second sheet, skips first two lines
                         sheet = 2, skip = 2) %>% 
  clean_names() %>% # fix the column names 
  rename(spot = cushing_ok_wti_spot_price_fob_dollars_per_barrel) %>% 
  mutate(time = as.Date(date, format = c("%Y-%m-%d"))) %>% # fix the dates
  filter(time >= "2018-09-14" & time <= "2019-08-20") %>% #filter to relevant dates 
  select(time, spot) #select relevant columns

# create data_cleaning() function that takes in `sheet` as an argument,
    # cleans the var names, 
    # makes sure 'time' is recognized as a date format, 
    # filters to the relevant dates, and select  the relevant columns only

data_cleaning <- function(sheet){
  sheet <- sheet %>% clean_names() %>%
    mutate(time = as.Date(time, format = c("%Y-%m-%d"))) %>%
    filter(time >= "2018-09-14" & time <= "2019-08-20") %>%
    select(time, last)
}

# export only nov18 to mar19 sheets (used in analysis) and apply the data_cleaning() function
  # rename the column 'last' to the date/yr
nov_18 <- data_cleaning(read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), 
                                   sheet = "Nov 18")) %>% rename(nov_18 = last)
dec_18 <- data_cleaning(read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), 
                                   sheet = "Dec 18")) %>% rename(dec_18 = last)
jan_19 <- data_cleaning(read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), 
                                   sheet = "Jan 19")) %>% rename(jan_19= last)
feb_19 <- data_cleaning(read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), 
                                   sheet = "Feb 19")) %>% rename(feb_19 = last)
mar_19 <- data_cleaning(read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), 
                                   sheet = "Mar 19")) %>% rename(mar_19 = last)

# merge multiple data frames together in one line 
  # https://stackoverflow.com/questions/14096814/merging-a-lot-of-data-frames
combined <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                   list(crude_spot, nov_18, dec_18, jan_19, feb_19, mar_19))

# do the same as above but for 2021 data
    # make sure Excel file is not opened. You will get an error. 
crude_spot21 <- read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), 
                           sheet = 2, skip = 2) %>%
  clean_names() %>%
  rename(spot = cushing_ok_wti_spot_price_fob_dollars_per_barrel) %>%
  mutate(time = as.Date(date, format = c("%Y-%m-%d"))) %>%
  filter(time >= "2021-07-15" & time <= "2021-11-19") %>%
  select(time, spot)

# create data cleaning function for 2021 data
data_cleaning21 <- function(sheet){
  sheet <- sheet %>% clean_names() %>%
    mutate(time = as.Date(time, format = c("%Y-%m-%d"))) %>%
    filter(time >= "2021-07-15" & time <= "2021-11-19") %>%
    select(time, last)
}

# read in files 
aug_21 <- data_cleaning21(read_excel(here("Data", "Crude Futures Data 2021.xlsx"), 
                                     sheet = "Aug 21")) %>% rename(aug_21 = last)
sep_21 <- data_cleaning21(read_excel(here("Data", "Crude Futures Data 2021.xlsx"), 
                                     sheet = "Sep 21")) %>% rename(sep_21 = last)
oct_21 <- data_cleaning21(read_excel(here("Data", "Crude Futures Data 2021.xlsx"), 
                                     sheet = "Oct 21")) %>% rename(oct_21 = last)
nov_21 <- data_cleaning21(read_excel(here("Data", "Crude Futures Data 2021.xlsx"), 
                                     sheet = "Nov 21")) %>% rename(nov_21 = last)
dec_21 <- data_cleaning21(read_excel(here("Data", "Crude Futures Data 2021.xlsx"), 
                                     sheet = "Dec 21")) %>% rename(dec_21 = last)

# combine 2021 dataframes
combined_21 <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                      list(crude_spot21, aug_21, sep_21, oct_21, nov_21, dec_21))

# create graph in p.20 of notes
# Slight diff in Excel: Dr. Vercammen inserted the avg of previous day and following 
  # day's spot prices for public holidays/non trading days. 
  # In R, I just dropped if missing so that the line graph is smooth. 
  # Calculations are very similar

# WTI Basis and December 2021 Futures for Contango Scenario
wti_basis_contango <- combined %>% 
  filter(!is.na(spot)) %>% # drop if missing
  mutate(basis = spot - mar_19) %>%
  filter(time >= "2018-10-18" & time <= "2019-02-20") %>% 
  ggplot(aes(x = time, y = basis)) + 
  geom_line(color = "orange", size = 0.75) + # make line size thicker
  theme_classic(base_size = 15) +
  scale_x_date(date_labels = "%b %y") +
  labs(x = "Date", y = "March 2019 Basis ($/barrel)")
wti_basis_contango

wti_futures_contango <- combined %>% 
  filter(!is.na(spot)) %>% 
  filter(time >= "2018-10-18" & time <= "2019-02-20") %>% 
  ggplot(aes(x = time, y = mar_19)) + 
  coord_cartesian(ylim = c(0, 80)) +
  geom_line(color = "blue", size = 0.75) + 
  theme_classic(base_size = 15) +
  scale_x_date(date_labels = "%b %y") + 
  labs(x = "Date", y = "March 2019 Futures ($/barrel)")
wti_futures_contango

# Forward Curve for Oil on October 22
forward_curve <- combined %>% 
  filter(time == "2018-10-22") %>% 
  select(nov_18, dec_18, jan_19, feb_19, mar_19) %>%
  pivot_longer(everything(), 
               names_to = "time",
               values_to = "futures") %>%
  # need to have a day to be considered a date
  mutate(time = as.Date(paste0("01_", time), format = "%d_%b_%y")) 

forward_contango <- ggplot(forward_curve, aes(x = time, y = futures)) + 
  geom_col(fill = "lightblue") + 
  coord_cartesian(ylim = c(69,69.75)) + # set the coordinates to match excel graph
  labs(title = "Forward Curve for WTI Crude Oil: Oct 22, 2018", subtitle = "Last trading day for November contract", y = "$/barrel", x = "Date") +
  scale_x_date(date_labels = "%b %y") + 
  theme_classic() +
  geom_text(aes(label = futures), vjust = -0.5, size = 3.5)
forward_contango

# You can modify the width
# ggsave(wti_basis_contango, file = here("Images", "wti_basis_contango.png"), width = 8)
# ggsave(wti_futures_contango, file = here("Images", "wti_futures_contango.png"), width = 8)
# ggsave(forward_contango, file = here("Images", "forward_contango.png"))


# WTI Basis and December 2021 Futures for Backwardation Scenario

wti_basis_backwardation <- combined_21 %>% 
  mutate(basis = spot - dec_21) %>%
  ggplot(aes(x = time, y = basis)) + 
  geom_line(color = "orange", size = 0.75) + 
  theme_classic(base_size = 15) +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) + 
  scale_x_date(date_labels = "%b %y") +
  labs(x = "Date", y = "Dec 2021 Basis ($/barrel)")
wti_basis_backwardation

wti_futures_backwardation <- combined_21 %>% 
  ggplot(aes(x = time, y = dec_21)) + 
  coord_cartesian(ylim = c(0, 85)) +
  geom_line(color = "blue", size = 0.75) + 
  theme_classic(base_size = 15) +
  scale_x_date(date_labels = "%b %y") + 
  labs(x = "Date", y = "Dec 2021 Futures ($/barrel)")
wti_futures_backwardation

# Forward curve for oil - Aug 20, 2021
forward_curve_back <- combined_21 %>% 
  filter(time == "2021-08-20") %>%
  select(sep_21, oct_21, nov_21, dec_21) %>%
  pivot_longer(everything(), 
               names_to = "time",
               values_to = "futures") %>%
  # need to have a day to be considered a date
  mutate(time = as.Date(paste0("01_", time), format = "%d_%b_%y")) 

forward_backwardation <- ggplot(forward_curve_back, aes(x = time, y = futures)) + 
  geom_col(fill = "lightblue") + 
  coord_cartesian(ylim = c(61,62.5)) + 
  labs(title = "Forward Curve for WTI Crude Oil: Aug 20, 2021", 
       subtitle = "Last trading day for September contract", y = "$/barrel", x = "Date") +
  scale_x_date(date_labels = "%b %y") + 
  theme_classic() +
  geom_text(aes(label = futures), vjust = -0.5, size = 3.5)
forward_backwardation

# ggsave(wti_basis_backwardation, file = here("Images", "wti_basis_back.png"), width = 8)
# ggsave(wti_futures_backwardation, file = here("Images", "wti_futures_back.png"), width = 8)
# ggsave(forward_backwardation, file = here("Images", "forward_backwardation.png"))


###########################################################
# Construct Sequence of Futures Joined Prices - Contango
# Dates: Oct 18, 2018 - Feb 20, 2019
###########################################################

combined <- combined %>%
  # filter to relevant dates
  filter(time >= "2018-10-18" & time <= "2019-02-20") %>% 
# create roll columns that calculate the difference in futures prices
  # these are roll adjustments
mutate(roll1 = nov_18 - dec_18,
       roll2 = dec_18 - jan_19,
       roll3 = jan_19 - feb_19,
       roll4 = feb_19 - mar_19) %>%
  # calculate index of each roll - last date the contract trades for 
  # max(which(!is.na(data$roll1))) gives us the index of the largest non NA value in roll1 
  mutate(i_roll1 = max(which(!is.na(roll1))),
         i_roll2 = max(which(!is.na(roll2))),
         i_roll3 = max(which(!is.na(roll3))),
         i_roll4 = max(which(!is.na(roll4)))) %>%
  # add an index 1 to n for use in the next function
  # march 19 is the last contract in consideration for this dataframe
  mutate(index = c(1:sum(!is.na(mar_19)))) %>%
  # create column called `current` that contains the next to expire futures price
  # from buy til the last day nov18 contract trades, current = nov18 price
  # from roll to dec18 contract to the last day it trades, current = dec18 price
  mutate(current = ifelse(index < i_roll1, nov_18,
                          ifelse(index < i_roll2, dec_18,
                                 ifelse(index < i_roll3, jan_19,
                                        ifelse(index < i_roll4, feb_19, mar_19)))))

# calc value of the cumulative roll (_con to indicate contango)
# it was easier/saves space to save these values outside the dataframe
# max(which(!is.na(combined$roll1))) gives the index number for the last non-NA in roll1 
  # which is the price of back contract (nov18) - price of front contract (dec18) 
  # at the last date of trade of back contract
# for the next roll, we take the back contract (dec18) - front contract (jan19) on the last
  # trading date of back contract PLUS the first roll yield since it's cumulative 
cum_roll1_con <- combined$roll1[max(which(!is.na(combined$roll1)))]
cum_roll2_con <- cum_roll1_con + combined$roll2[max(which(!is.na(combined$roll2)))] 
cum_roll3_con <- cum_roll2_con + combined$roll3[max(which(!is.na(combined$roll3)))]
cum_roll4_con <- cum_roll3_con + combined$roll4[max(which(!is.na(combined$roll4)))]

# calc cumulative roll
# from buy date to the last day the nov18 contract trades, cum_roll = 0
# rolling to dec18 contract to the last day dec18 contract trades, cum roll = dec18-nov18 (cum_roll1_con)
# rolling to jan19 contract to the last day jan19 contract trades, cum roll = jan19-dec18 (cum_roll2_con)
combined <- combined %>% 
  mutate(cum_roll = ifelse(index < i_roll1, 0, 
                           ifelse(index < i_roll2, cum_roll1_con,
                                  ifelse(index < i_roll3, cum_roll2_con,
                                         ifelse(index < i_roll4, cum_roll3_con, cum_roll4_con))))) %>%
  # add cum_roll to current price to get the joined price
  mutate(joined = current + cum_roll)
  
# saveRDS(combined, here("Data", "combined.RDS"))

###########################################################
# Contango: Verification of profits 
# Buy date: October 18, 2018
# Sell date: Feb 20, 2019
###########################################################  
# Not in PPT slides (only Backwardation was in there)

# Create Investor B returns - rolls a long futures position through 5 contracts

# Create empty data frame with Date, Buy, Sell, Gain as the column names

n <- 6 # number of rows
investor_b_con <- data.frame(Date = rep(NA, n), 
                             Buy = rep(NA, n), 
                             Sell = rep(NA, n), 
                             Gain = rep(NA, n))

investor_b_con$Date <- c("Nov 18", "Dec 18", "Jan 19", "Feb 19", "Mar 19", "Cumulative Gain")

investor_b_con$Buy[1] <- combined$nov_18[combined$time == "2018-10-18"]
investor_b_con$Buy[2] <- combined$dec_18[combined$time == "2018-10-22"]
investor_b_con$Buy[3] <- combined$jan_19[combined$time == "2018-11-19"]
investor_b_con$Buy[4] <- combined$feb_19[combined$time == "2018-12-19"]
investor_b_con$Buy[5] <- combined$mar_19[combined$time == "2019-01-22"]

investor_b_con$Sell[1] <- combined$nov_18[combined$time == "2018-10-22"]
investor_b_con$Sell[2] <- combined$dec_18[combined$time == "2018-11-19"]
investor_b_con$Sell[3] <- combined$jan_19[combined$time == "2018-12-19"]
investor_b_con$Sell[4] <- combined$feb_19[combined$time == "2019-01-22"]
investor_b_con$Sell[5] <- combined$mar_19[combined$time == "2019-02-20"]

investor_b_con$Gain <- investor_b_con$Sell - investor_b_con$Buy
investor_b_con$Gain[6] <- sum(investor_b_con$Gain[1:5])
investor_b_con[is.na(investor_b_con)] <- "" # replace NA to empty string 
investor_b_con

# Create Investor A returns - takes a long position in the joined futures
n = 3
investor_a_con <-  data.frame(Date = rep(NA, n), 
                              Buy = rep(NA, n), 
                              Sell = rep(NA, n), 
                              Gain = rep(NA,n))


investor_a_con$Date <- c("Oct 18, 2018", "Feb 20, 2019", "Cumulative Gain")
investor_a_con$Buy[1] <- combined$nov_18[combined$time == "2018-10-18"]
investor_a_con$Sell[2] <- combined$joined[combined$time == "2019-02-20"]
investor_a_con$Gain[3] <- investor_a_con$Sell[2] - investor_a_con$Buy[1]
investor_a_con

cumul_index_con <- combined$joined[combined$time == "2019-02-20"] - combined$nov_18[combined$time == "2018-10-18"]
cumul_spot_con <- combined$spot[combined$time == "2019-02-20"] - combined$spot[combined$time == "2018-10-18"]
cumul_rollyield_con <- cumul_index_con - cumul_spot_con

# saveRDS(investor_a_con, here("Data", "investor_a_con.RDS"))
# saveRDS(investor_b_con, here("Data", "investor_b_con.RDS"))

###########################################################
# Construct Sequence of Futures Joined Prices - Backwardation
# Dates: July 15, 2021 - Nov 19, 2021 
###########################################################  
# same method as in contango , so did not put comments anymore 

combined_21 <- combined_21 %>%
  mutate(roll1 = aug_21 - sep_21,
         roll2 = sep_21 - oct_21,
         roll3 = oct_21 - nov_21,
         roll4 = nov_21 - dec_21) %>%
  mutate(i_roll1 = max(which(!is.na(roll1))),
         i_roll2 = max(which(!is.na(roll2))),
         i_roll3 = max(which(!is.na(roll3))),
         i_roll4 = max(which(!is.na(roll4)))) %>%
  mutate(index = c(1:nrow(combined_21))) %>%
  mutate(current = ifelse(index < i_roll1, aug_21,
                          ifelse(index < i_roll2, sep_21,
                                 ifelse(index < i_roll3, oct_21,
                                        ifelse(index < i_roll4, nov_21, dec_21)))))

cum_roll1 <- combined_21$roll1[max(which(!is.na(combined_21$roll1)))]
cum_roll2 <- cum_roll1 + combined_21$roll2[max(which(!is.na(combined_21$roll2)))]
cum_roll3 <- cum_roll2 + combined_21$roll3[max(which(!is.na(combined_21$roll3)))]
cum_roll4 <- cum_roll3 + combined_21$roll4[max(which(!is.na(combined_21$roll4)))]

combined_21 <- combined_21 %>% 
  mutate(cum_roll = ifelse(index < i_roll1, 0, 
                           ifelse(index < i_roll2, cum_roll1,
                                  ifelse(index < i_roll3, cum_roll2,
                                         ifelse(index < i_roll4, cum_roll3, cum_roll4))))) %>%
  mutate(joined = current + cum_roll)

# saveRDS(combined_21, here("Data", "combined_21.RDS"))

###########################################################
# Backwardation: Verification of profits 
# Buy date: July 15, 2021
# Sell date: November 19, 2021
###########################################################  

# Create Investor B returns - rolls a long future position through 5 contracts

# creates an empty data frame with Date, Buy, Sell, Gain as the column names
n <- 6 # number of rows

investor_b <- data.frame(Date = rep(NA, n), 
                         Buy = rep(NA, n), 
                         Sell = rep(NA, n), 
                         Gain = rep(NA, n))

investor_b$Date <- c("Aug 21", "Sep 21", "Oct 21", "Nov 21", "Dec 21", "Cumulative Gain")

investor_b$Buy[1] <- combined_21$aug_21[combined_21$time == "2021-07-15"]
investor_b$Buy[2] <- combined_21$sep_21[combined_21$time == "2021-07-20"]
investor_b$Buy[3] <- combined_21$oct_21[combined_21$time == "2021-08-20"]
investor_b$Buy[4] <- combined_21$nov_21[combined_21$time == "2021-09-21"]
investor_b$Buy[5] <- combined_21$dec_21[combined_21$time == "2021-10-20"]

investor_b$Sell[1] <- combined_21$aug_21[combined_21$time == "2021-07-20"]
investor_b$Sell[2] <- combined_21$sep_21[combined_21$time == "2021-08-20"]
investor_b$Sell[3] <- combined_21$oct_21[combined_21$time == "2021-09-21"]
investor_b$Sell[4] <- combined_21$nov_21[combined_21$time == "2021-10-20"]
investor_b$Sell[5] <- combined_21$dec_21[combined_21$time == "2021-11-19"]

investor_b$Gain <- investor_b$Sell - investor_b$Buy
investor_b$Gain[6] <- sum(investor_b$Gain[1:5])
investor_b[is.na(investor_b)] <- "" # replace NA to empty string 

investor_b

# Create Investor A returns - takes a long position in the joined futures
n = 3

investor_a <- data.frame(Date = rep(NA, n), 
                         Buy = rep(NA, n), 
                         Sell = rep(NA, n), 
                         Gain = rep(NA,n))

investor_a$Date <- c("Jul 15, 2021", "Nov 19, 2021", "Cumulative Gain")
investor_a$Buy[1] <- combined_21$aug_21[combined_21$time == "2021-07-15"]
investor_a$Sell[2] <- combined_21$joined[combined_21$time == "2021-11-19"]
investor_a$Gain[3] <- investor_a$Sell[2] - investor_a$Buy[1]
investor_a[is.na(investor_a)] <- ""
investor_a

cumul_index <- combined_21$joined[combined_21$time == "2021-11-19"] - combined_21$aug_21[combined_21$time == "2021-07-15"]
cumul_spot <- combined_21$spot[combined_21$time == "2021-11-19"] - combined_21$spot[combined_21$time == "2021-07-15"]
cumul_rollyield <- cumul_index - cumul_spot

# saveRDS(investor_a, here("Data", "investor_a.RDS"))
# saveRDS(investor_b, here("Data", "investor_b.RDS"))

###########################################################
# Contango: Adjusted Spot and Joined Futures 
###########################################################

adjusted_con <- combined %>%
  filter(time >= "2018-10-18" & time <= "2019-02-20") %>%
  select(time, spot, joined) %>% 
  mutate(spot2 = spot + cumul_rollyield_con)%>%
  select(time, spot, spot2, joined)

contango_futures_spot <- adjusted_con %>%
  filter(!is.na(spot) & time >= "2018-12-17" & time <= "2019-02-20") %>%
  ggplot(aes(x = time)) + 
  geom_line(aes(y = spot), color = "blue", size = 0.75) +
  geom_line(aes(y = joined), color = "orange", size = 0.75) +
  theme_classic() +
  labs(x = "Date", y = "Price ($/barrel)", caption = "Blue = Spot, Orange = Joined") +
  geom_text(aes(x = time[42], y = spot[42], label = spot[42]), size = 3, hjust = 0.5, vjust = -0.5, color = "blue") +
  geom_text(aes(x = time[42], y = joined[42], label = joined[42]), size = 3, hjust = 0.5, vjust = -0.8) 
contango_futures_spot

contango_left <- adjusted_con %>% 
  filter(!is.na(spot) & time <= "2018-12-17") %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = spot2), color = "blue", size = 0.75) +
  geom_line(aes(y = joined), color = "orange", size = 0.75) +
  theme_classic() +
  labs(x = "Date", y = "Price ($/barrel)", caption = "Blue = Adjusted Spot, Orange = Joined") +
  # manually adding the starting values as labels
  geom_text(aes(x = time[1], y = spot2[1], label = spot2[1]), size = 3, hjust = 0.3, vjust = 1.3, color = "blue") +
  geom_text(aes(x = time[1], y = joined[1], label = joined[1]), size = 3, hjust = 0.5, vjust = 1.3) 
contango_left

contango_right <- adjusted_con %>% 
  filter(!is.na(spot) & time >= "2018-12-17") %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = spot2), color = "blue", size = 0.75) +
  geom_line(aes(y = joined), color = "orange", size = 0.75) +
  theme_classic() +
  labs(x = "Date", y = "Price ($/barrel)", caption = "Blue = Adjusted Spot, Orange = Joined")
contango_right

# ggsave(contango_futures_spot, file = here("Images", "contango_futures_spot.png"), width = 5)
# ggsave(contango_right, file = here("Images", "contango_right.png"), width = 5)
# ggsave(contango_left, file = here("Images", "contango_left.png"), width = 5)

###########################################################
# Backwardation: Adjusted Spot and Joined Futures 
###########################################################
# not in Excel 

adjusted <- combined_21 %>%
  select(time, spot, joined) %>% 
  mutate(spot2 = spot + cumul_rollyield)%>%
  select(time, spot, spot2, joined)

back_futures_spot <- adjusted %>%
  filter(!is.na(spot) & time >= "2021-10-11") %>%
  ggplot(aes(x = time)) + 
  geom_line(aes(y = spot), color = "blue", size = 0.75) +
  geom_line(aes(y = joined), color = "orange", size = 0.75) +
  theme_classic() +
  labs(x = "Date", y = "Price ($/barrel)", caption = "Blue = Spot, Orange = Joined") +
  geom_text(aes(x = time[30], y = spot[30], label = spot[30]), size = 3, hjust = 1.3, vjust = -0.5, color = "blue") +
  geom_text(aes(x = time[30], y = joined[30], label = joined[30]), size = 3, hjust = 1.3, vjust = -0.8) 
back_futures_spot

back_left <- adjusted %>% 
  filter(!is.na(spot) & time <= "2021-09-15") %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = spot2), color = "blue", size = 0.75) +
  geom_line(aes(y = joined), color = "orange", size = 0.75) +
  theme_classic() +
  labs(x = "Date", y = "Price ($/barrel)", caption = "Blue = Adjusted Spot, Orange = Joined") +
  # manually adding the starting values as labels
  geom_text(aes(x = time[1], y = spot2[1], label = spot2[1]), size = 2.4, hjust = 0.5, vjust = -0.8, color = "blue") +
  geom_text(aes(x = time[1], y = joined[1], label = joined[1]), size = 2.4, hjust = 0.78, vjust = -0.9) 
back_left

back_right <- adjusted %>% 
  filter(!is.na(spot) & time >= "2021-09-15") %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = spot2), color = "blue", size = 0.75) +
  geom_line(aes(y = joined), color = "orange", size = 0.75) +
  theme_classic(base_size = 15) +
  labs(x = "Date", y = "Price ($/barrel)", caption = "Blue = Adjusted Spot, Orange = Joined")
back_right

# ggsave(back_futures_spot, file = here("Images", "back_futures_spot.png"), width = 5)
# ggsave(back_right, file = here("Images", "back_right.png"), width = 5)
# ggsave(back_left, file = here("Images", "back_left.png"), width = 5)

