# This script contains the codes to clean the raw data in R and match Excel output

pacman::p_load(here, dplyr, ggplot2, lubridate, kableExtra, readxl, janitor, tidyverse, purrr, stringr, directlabels)

# Load in the 'Graphs' sheet of the Excel file
  # make sure Excel file is not opened. You will get an error. 
compare <- read_excel(here("Data", "Wheat Forward Curve Compare.xlsx"), sheet = "Graphs", range = "B1:F9") %>% 
  select(c(1,2,5)) %>%
  clean_names() %>%
  rename(contracts = x1) %>%
  # changed type to factor so that the order is preserved in the plot
  mutate(contracts = factor(contracts, levels = contracts))

###########################################################
# Contango (July 2020) and Backwardation (June 2021) graph
###########################################################

# recreate graph on p.17 of notes (but not dual axis)
# note: str_wrap() from {stringr} allows me to add a line break in the x labels after 3 chars
corn_july20 <- ggplot(compare, aes(x = stringr::str_wrap(contracts, 3), y = july_2_2020)) +
  geom_col(fill = "lightblue") +
  coord_cartesian(ylim = c(3, 3.80)) +
  theme_classic(base_size = 15) + # increase size of axis and labels
  geom_text(aes(label = round(july_2_2020,2)), vjust = -0.5, size = 3) +
  labs(x = "Contracts", y = "Futures: July 2, 2020 ($/bu)")

# recreate graph on p.17 of notes (but not dual axis)
corn_june21 <- ggplot(compare, aes(x = stringr::str_wrap(contracts, 3), y = june_2_2021)) +
  geom_col(fill = "orange") +
  coord_cartesian(ylim = c(4.5, 6.8)) +
  theme_classic(base_size = 15) +
  geom_text(aes(label = round(june_2_2021,2)), vjust = -0.5, size = 3) +
  labs(x = "Contracts", y = "Futures: July 2, 2020 ($/bu)")

# ggsave(corn_july20, file = here("Images", "corn_forward_curve_20.png"), width = 8, height = 4)
# ggsave(corn_june21, file = here("Images", "corn_forward_curve_21.png"),  width = 8, height = 4)

# pivot df to long format to create grouped bar chart
compare_long <- compare %>%
  pivot_longer(-contracts,
               names_to = "date", values_to = "price")

corn_futures <- ggplot(compare_long, aes(x = stringr::str_wrap(contracts,3), y = price, fill = date)) + 
  geom_bar(position = "dodge", stat = "identity") + # grouped bars
  scale_fill_manual(values = c("lightblue", "orange"), # set colors of bars
                    name = "Date", # set legend name
                    labels = c("July 2, 2020", "June 2, 2021")) +
  # set the value labels above the grouped bars
  geom_text(aes(label = round(price,2)), 
            size = 3, position = position_dodge(width = 1), vjust = -0.5) + 
  labs(x = "Contracts", y = "Futures Price ($/bu)") +
  theme_classic(base_size = 13.5)

# ggsave(corn_futures, file = here("Images", "corn_forward_curve.png"), width = 8, height = 4)  


###### Case Study: WTI Crude Oil

# load data to recreate Combined sheet
    # make sure Excel file is not opened. You will get an error. 
crude_spot <- read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), 
                         sheet = 2, skip = 2) %>% # looks at second sheet, skips first two lines
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

# export only nov18 to mar19 sheets and apply the data_cleaning() function
  # rename the column 'last' to the date
nov_18 <- data_cleaning(read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), sheet = "Nov 18")) %>% rename(nov_18 = last)
dec_18 <- data_cleaning(read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), sheet = "Dec 18")) %>% rename(dec_18 = last)
jan_19 <- data_cleaning(read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), sheet = "Jan 19")) %>% rename(jan_19= last)
feb_19 <- data_cleaning(read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), sheet = "Feb 19")) %>% rename(feb_19 = last)
mar_19 <- data_cleaning(read_excel(here("Data", "Crude Futures Data 2018 - 2019.xlsx"), sheet = "Mar 19")) %>% rename(mar_19 = last)

# merge multiple data frames together 
  # https://stackoverflow.com/questions/14096814/merging-a-lot-of-data-frames
combined <- Reduce(function(x, y) merge(x, y, all=TRUE), list(crude_spot, nov_18,
                                                              dec_18, jan_19, feb_19, mar_19))

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
aug_21 <- data_cleaning21(read_excel(here("Data", "Crude Futures Data 2021.xlsx"), sheet = "Aug 21")) %>% rename(aug_21 = last)
sep_21 <- data_cleaning21(read_excel(here("Data", "Crude Futures Data 2021.xlsx"), sheet = "Sep 21")) %>% rename(sep_21 = last)
oct_21 <- data_cleaning21(read_excel(here("Data", "Crude Futures Data 2021.xlsx"), sheet = "Oct 21")) %>% rename(oct_21 = last)
nov_21 <- data_cleaning21(read_excel(here("Data", "Crude Futures Data 2021.xlsx"), sheet = "Nov 21")) %>% rename(nov_21 = last)
dec_21 <- data_cleaning21(read_excel(here("Data", "Crude Futures Data 2021.xlsx"), sheet = "Dec 21")) %>% rename(dec_21 = last)

# combine 2021 data
combined_21 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(crude_spot21, aug_21, sep_21, oct_21, nov_21, dec_21))

# create graph in p.20 of notes
# Slight diff in Excel: Dr. Vercammen initially inserted the avg of previous day and following day's spot prices for public holidays/non trading days.

wti_basis_contango <- combined %>% 
  filter(!is.na(spot)) %>% 
  mutate(basis = spot - mar_19) %>%
  filter(time >= "2018-10-18" & time <= "2019-02-20") %>% 
  ggplot(aes(x = time, y = basis)) + 
  geom_line(color = "orange", size = 0.75) + 
  theme_classic(base_size = 15) +
  scale_x_date(date_labels = "%b %y") +
  labs(x = "Date", y = "March 2019 Basis ($/barrel)")

wti_futures_contango <- combined %>% 
  filter(!is.na(spot)) %>% 
  filter(time >= "2018-10-18" & time <= "2019-02-20") %>% 
  ggplot(aes(x = time, y = mar_19)) + 
  coord_cartesian(ylim = c(0, 80)) +
  geom_line(color = "blue", size = 0.75) + 
  theme_classic(base_size = 15) +
  scale_x_date(date_labels = "%b %y") + 
  labs(x = "Date", y = "March 2019 Futures ($/barrel)")

# ggsave(wti_basis_contango, file = here("Images", "wti_basis_contango.png"), width = 8)
# ggsave(wti_futures_contango, file = here("Images", "wti_futures_contango.png"), width = 8)

# creating the forward curve - contango 
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
# ggsave(forward_contango, file = here("Images", "forward_contango.png"))


# create graph in p.22 of notes
# Slight diff in Excel: Dr. Vercammen initially inserted the avg of previous day and following day's spot prices for public holidays/non trading days.

wti_basis_backwardation <- combined_21 %>% 
  mutate(basis = spot - dec_21) %>%
  ggplot(aes(x = time, y = basis)) + 
  geom_line(color = "orange", size = 0.75) + 
  theme_classic(base_size = 15) +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) + 
  scale_x_date(date_labels = "%b %y") +
  labs(x = "Date", y = "Dec 2021 Basis ($/barrel)")

wti_futures_backwardation <- combined_21 %>% 
  ggplot(aes(x = time, y = dec_21)) + 
  coord_cartesian(ylim = c(0, 85)) +
  geom_line(color = "blue", size = 0.75) + 
  theme_classic(base_size = 15) +
  scale_x_date(date_labels = "%b %y") + 
  labs(x = "Date", y = "Dec 2021 Futures ($/barrel)")

# ggsave(wti_basis_backwardation, file = here("Images", "wti_basis_back.png"), width = 8)
# ggsave(wti_futures_backwardation, file = here("Images", "wti_futures_back.png"), width = 8)


# creating the forward curve - backwardation 
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
# ggsave(forward_backwardation, file = here("Images", "forward_backwardation.png"))

###########################################################
# Construct Sequence of Futures Joined Prices
###########################################################

combined <- combined %>%
  filter(time >= "2018-10-18" | time <= "2019-02-20") %>% 
# create columns that calculate the difference in futures prices - these are roll adjustments
mutate(roll1 = nov_18 - dec_18,
       roll2 = dec_18 - jan_19,
       roll3 = jan_19 - feb_19,
       roll4 = feb_19 - mar_19) %>%
  # calculate index of each roll
  # max(which(!is.na(data$roll1))) = gives us the index of the largest non NA value in roll1 
  mutate(i_roll1 = max(which(!is.na(roll1))),
         i_roll2 = max(which(!is.na(roll2))),
         i_roll3 = max(which(!is.na(roll3))),
         i_roll4 = max(which(!is.na(roll4)))) %>%
  # add an index variable for use in the next function
  mutate(index = c(1:nrow(combined))) %>%
  # create column called `current` that contains the next to expire futures price
  mutate(current = ifelse(index < i_roll1, nov_18,
                          ifelse(index < i_roll2, dec_18,
                                 ifelse(index < i_roll3, jan_19,
                                        ifelse(index < i_roll4, feb_19, mar_19)))))

# calc value of the cumulative roll
cum_roll1_con <- combined$roll1[max(which(!is.na(combined$roll1)))]
cum_roll2_con <- cum_roll1_con + combined$roll2[max(which(!is.na(combined$roll2)))]
cum_roll3_con <- cum_roll2_con + combined$roll3[max(which(!is.na(combined$roll3)))]
cum_roll4_con <- cum_roll3_con + combined$roll4[max(which(!is.na(combined$roll4)))]

# create cumulative roll variable in the dataset
combined <- combined %>% 
  mutate(cum_roll = ifelse(index < i_roll1, 0, 
                           ifelse(index < i_roll2, cum_roll1_con,
                                  ifelse(index < i_roll3, cum_roll2_con,
                                         ifelse(index < i_roll4, cum_roll3_con, cum_roll4_con))))) %>%
  # add cum_roll with current price 
  mutate(joined = current + cum_roll)
  # remove the repeating cum_roll
  
  
  
combined_21 <- combined_21 %>%
  # create columns that calculate the difference in futures prices - these are roll adjustments
  mutate(roll1 = aug_21 - sep_21,
         roll2 = sep_21 - oct_21,
         roll3 = oct_21 - nov_21,
         roll4 = nov_21 - dec_21) %>%
  # calculate index of each roll
  # max(which(!is.na(data$roll1))) = gives us the index of the largest non NA value in roll1 
  mutate(i_roll1 = max(which(!is.na(roll1))),
         i_roll2 = max(which(!is.na(roll2))),
         i_roll3 = max(which(!is.na(roll3))),
         i_roll4 = max(which(!is.na(roll4)))) %>%
  # add an index variable for use in the next function
  mutate(index = c(1:nrow(combined_21))) %>%
  # create column called `current` that contains the next to expire futures price
  mutate(current = ifelse(index < i_roll1, aug_21,
                          ifelse(index < i_roll2, sep_21,
                                 ifelse(index < i_roll3, oct_21,
                                        ifelse(index < i_roll4, nov_21, dec_21)))))

# calc value of the cumulative roll
cum_roll1 <- combined_21$roll1[max(which(!is.na(combined_21$roll1)))]
cum_roll2 <- cum_roll1 + combined_21$roll2[max(which(!is.na(combined_21$roll2)))]
cum_roll3 <- cum_roll2 + combined_21$roll3[max(which(!is.na(combined_21$roll3)))]
cum_roll4 <- cum_roll3 + combined_21$roll4[max(which(!is.na(combined_21$roll4)))]

# create cumulative roll variable in the dataset
combined_21 <- combined_21 %>% 
  mutate(cum_roll = ifelse(index < i_roll1, 0, 
                           ifelse(index < i_roll2, cum_roll1,
                                  ifelse(index < i_roll3, cum_roll2,
                                         ifelse(index < i_roll4, cum_roll3, cum_roll4))))) %>%
  # add cum_roll with current price 
  mutate(joined = current + cum_roll)


# verify that joined futures price correctly calculates cumulative profits
# Create investor B returns
# creates an empty data frame with Date, Buy, Sell, Gain as the column names
n <- 6 # number of rows
investor_b <- data.frame(Date = rep(NA, n), Buy = rep(NA, n), Sell = rep(NA, n), Gain = rep(NA, n))

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

# Create investor A  returns
n = 3
investor_a <- data.frame(Date = rep(NA, n), Buy = rep(NA, n), Sell = rep(NA, n), Gain = rep(NA,n))

investor_a$Date <- c("Jul 15, 2021", "Nov 19, 2021", "Cumulative Gain")
investor_a$Buy[1] <- combined_21$aug_21[combined_21$time == "2021-07-15"]
investor_a$Sell[2] <- combined_21$joined[combined_21$time == "2021-11-19"]
investor_a$Gain[3] <- investor_a$Sell[2] - investor_a$Buy[1]
investor_a[is.na(investor_a)] <- ""

# saveRDS(investor_b, here("Data", "investor_b.RDS"))
# saveRDS(investor_a, here("Data", "investor_a.RDS"))

saveRDS(combined_21, here("Data", "combined_21.RDS"))


###  joined futures vs spot in contango market

# assume buy on october 18, 2018 and sell on feb 20, 2021
n <- 6 # number of rows
roll_contango <- data.frame(Date = rep(NA, n), Buy = rep(NA, n), Sell = rep(NA, n), Gain = rep(NA, n))

roll_contango$Date <- c("Nov 18", "Dec 18", "Jan 19", "Feb 19", "Mar 19", "Cumulative Gain")

roll_contango$Buy[1] <- combined$nov_18[combined$time == "2018-10-18"]
roll_contango$Buy[2] <- combined$dec_18[combined$time == "2018-10-22"]
roll_contango$Buy[3] <- combined$jan_19[combined$time == "2018-11-19"]
roll_contango$Buy[4] <- combined$feb_19[combined$time == "2018-12-19"]
roll_contango$Buy[5] <- combined$mar_19[combined$time == "2019-01-22"]

roll_contango$Sell[1] <- combined$nov_18[combined$time == "2018-10-22"]
roll_contango$Sell[2] <- combined$dec_18[combined$time == "2018-11-19"]
roll_contango$Sell[3] <- combined$jan_19[combined$time == "2018-12-19"]
roll_contango$Sell[4] <- combined$feb_19[combined$time == "2019-01-22"]
roll_contango$Sell[5] <- combined$mar_19[combined$time == "2019-02-20"]

roll_contango$Gain <- roll_contango$Sell - roll_contango$Buy
roll_contango$Gain[6] <- sum(roll_contango$Gain[1:5])
roll_contango[is.na(roll_contango)] <- "" # replace NA to empty string 


cumul_index <- combined$joined[combined$time == "2019-02-20"] - combined$nov_18[combined$time == "2018-10-18"]
cumul_spot <- combined$spot[combined$time == "2019-02-20"] - combined$spot[combined$time == "2018-10-18"]
cumul_rollyield <- cumul_index - cumul_spot

adjusted <- combined %>%
  filter(time >= "2018-10-18" & time <= "2019-02-20") %>%
  select(time, spot, joined) %>% 
  mutate(spot2 = spot + cumul_rollyield)%>%
  select(time, spot, spot2, joined)

contango_right <-adjusted %>% 
  filter(!is.na(spot)) %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = spot), color = "orange", size = 0.75) +
  geom_line(aes(y = joined), color = "blue", size = 0.75) +
  theme_classic(base_size = 15) +
  labs(x = "Date", y = "Price ($/barrel)")

contango_left <- adjusted %>% 
  filter(!is.na(spot) & time >= "2018-12-17") %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = spot), color = "orange", size = 0.75) +
  geom_line(aes(y = joined), color = "blue", size = 0.75) +
  theme_classic(base_size = 15) +
  labs(x = "Date", y = "Price ($/barrel)")
ggsave(contango_right, file = here("Images", "contango_right.png"))
ggsave(contango_left, file = here("Images", "contango_left.png"))

